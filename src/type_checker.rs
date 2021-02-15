use crate::ast::*;
use crate::buckets::*;
use crate::filedb::*;
use crate::runtime::Opcode;
use crate::tc_ast::*;
use crate::tc_structs::*;
use crate::util::*;
use core::convert::TryInto;

#[derive(Hash, PartialEq, Eq)]
pub struct TypeDeclSpec {
    unsigned: u8,
    long: u8,
    short: u8,
    char: u8,
    int: u8,
    float: u8,
    signed: u8,
    double: u8,
    void: u8,
}

impl TypeDeclSpec {
    pub fn new() -> Self {
        TypeDeclSpec {
            unsigned: 0,
            long: 0,
            short: 0,
            char: 0,
            int: 0,
            float: 0,
            signed: 0,
            double: 0,
            void: 0,
        }
    }
}

macro_rules! gen_type_decl_spec {
    ($map:expr, $ast:ident, $( $ident:ident )* ) => {{
        let mut decl = TypeDeclSpec::new();

        $(
            gen_type_decl_spec!(@INCR_CORRECT, decl, $ident);
        )*

        $map.insert(decl, TCTypeBase::$ast);
    }};
    (@INCR_CORRECT, $decl:expr, void) => {
        $decl.void += 1;
    };
    (@INCR_CORRECT, $decl:expr, unsigned) => {
        $decl.unsigned += 1;
    };
    (@INCR_CORRECT, $decl:expr, long) => {
        $decl.long += 1;
    };
    (@INCR_CORRECT, $decl:expr, short) => {
        $decl.short += 1;
    };
    (@INCR_CORRECT, $decl:expr, char) => {
        $decl.char += 1;
    };
    (@INCR_CORRECT, $decl:expr, int) => {
        $decl.int += 1;
    };
    (@INCR_CORRECT, $decl:expr, float) => {
        $decl.float += 1;
    };
    (@INCR_CORRECT, $decl:expr, signed) => {
        $decl.signed += 1;
    };
    (@INCR_CORRECT, $decl:expr, double) => {
        $decl.double += 1;
    };
    (@INCR_CORRECT, $decl:expr, ident) => {
        $decl.ident += 1;
    };
}

type BuiltinTransform =
    for<'a, 'b, 'c> fn(&'a mut TypeEnv<'b>, CodeLoc, &'c [Expr]) -> Result<TCExpr, Error>;

lazy_static! {
    pub static ref CORRECT_TYPES: HashMap<TypeDeclSpec, TCTypeBase> = {
        let mut map: HashMap<TypeDeclSpec, TCTypeBase> = HashMap::new();

        gen_type_decl_spec!(map, I32, int);
        gen_type_decl_spec!(map, I64, long);
        gen_type_decl_spec!(map, I16, short);
        gen_type_decl_spec!(map, I8, char);
        gen_type_decl_spec!(map, Void, void);
        gen_type_decl_spec!(map, F32, float);
        gen_type_decl_spec!(map, F64, double);
        gen_type_decl_spec!(map, U32, unsigned);

        gen_type_decl_spec!(map, I8, signed char);
        gen_type_decl_spec!(map, U8, unsigned char);

        gen_type_decl_spec!(map, I16, short int);
        gen_type_decl_spec!(map, U16, unsigned short int);
        gen_type_decl_spec!(map, I16, signed short int);
        gen_type_decl_spec!(map, U16, unsigned short);
        gen_type_decl_spec!(map, I16, signed short);

        gen_type_decl_spec!(map, I32, signed int);
        gen_type_decl_spec!(map, U32, unsigned int);

        gen_type_decl_spec!(map, I64, signed long);
        gen_type_decl_spec!(map, U64, unsigned long);

        gen_type_decl_spec!(map, I64, long int);
        gen_type_decl_spec!(map, I64, long long int);
        gen_type_decl_spec!(map, I64, long long);

        gen_type_decl_spec!(map, I64, signed long int);
        gen_type_decl_spec!(map, I64, signed long long int);
        gen_type_decl_spec!(map, I64, signed long long);

        gen_type_decl_spec!(map, U64, unsigned long int);
        gen_type_decl_spec!(map, U64, unsigned long long);
        gen_type_decl_spec!(map, U64, unsigned long long int);

        map
    };
    pub static ref BUILTINS: HashMap<u32, BuiltinTransform> = {
        let mut m: HashMap<u32, BuiltinTransform> = HashMap::new();

        m.insert(BuiltinSymbol::BuiltinPush as u32, |env, call_loc, args| {
            if args.len() != 1 {
                return Err(error!(
                    "wrong number of arguments to builtin function",
                    call_loc, "called here"
                ));
            }

            let void = TCType::new(TCTypeBase::Void);

            let value = check_expr(&mut *env, &args[0])?;
            return Ok(TCExpr {
                kind: TCExprKind::Builtin(TCBuiltin::Push(env.add(value))),
                ty: void,
                loc: call_loc,
            });
        });

        m.insert(BuiltinSymbol::BuiltinOp as u32, |env, call_loc, args| {
            if args.len() != 2 {
                return Err(error!(
                    "wrong number of arguments to builtin function",
                    call_loc, "called here"
                ));
            }

            let void = TCType::new(TCTypeBase::Void);

            let op = match args[0].kind {
                ExprKind::StringLit(lit) => lit,
                _ => {
                    return Err(error!(
                        "first opcode argument must be string literal",
                        args[0].loc, "this argument"
                    ))
                }
            };
            let ast_ty = match args[1].kind {
                ExprKind::SizeofTy(ty) => ty,
                _ => {
                    return Err(error!(
                        "second opcode argument must be a sizeof(type) expression",
                        args[1].loc, "this argument"
                    ))
                }
            };

            let or_else = |_| {
                error!(
                    "opcode argument wasn't a real opcode",
                    args[0].loc, "this opcode argument"
                )
            };
            let op = op.parse::<Opcode>().map_err(or_else)?;

            let base = parse_spec_quals(&mut *env, ast_ty.specifiers)?;
            let ty = if let Some(decl) = ast_ty.declarator {
                let (ty, id) = check_decl(&mut *env, base, &decl)?;
                assert!(id == n32::NULL);
                ty.to_ref(&*env)
            } else {
                TCType { base, mods: &[] }
            };

            return Ok(TCExpr {
                kind: TCExprKind::Builtin(TCBuiltin::Opcode(op)),
                ty,
                loc: call_loc,
            });
        });

        m
    };
}

pub fn check_tree(
    file: u32,
    symbols: &Symbols,
    tree: &[GlobalStatement],
) -> Result<TranslationUnit, Error> {
    let mut globals = TypeEnv::global(file, symbols);

    for decl in tree {
        match decl.kind {
            GlobalStatementKind::Declaration(decl) => check_declaration(&mut globals, None, decl)?,
            GlobalStatementKind::FunctionDefinition(func) => {
                let func_decl = check_func_defn_decl(&mut globals, &func)?;

                let base = TCTypeBase::InternalTypedef(globals.add(func_decl.return_type));
                let mut ty = TCTypeOwned::new(base);

                if let Some(params) = func_decl.params {
                    if params.params.len() == 0 {
                        ty.mods.push(TCTypeModifier::NoParams);
                    } else {
                        ty.mods
                            .push(TCTypeModifier::BeginParam(params.params[0].ty));
                        for param in &params.params[1..] {
                            ty.mods.push(TCTypeModifier::Param(param.ty));
                        }

                        if params.varargs {
                            ty.mods.push(TCTypeModifier::VarargsParam);
                        }
                    }
                } else {
                    ty.mods.push(TCTypeModifier::UnknownParams);
                }

                let ident = func_decl.ident;
                let ty = ty.to_ref(&globals);
                let init = if func_decl.is_static {
                    TCDeclInit::Static(TCExprKind::FunctionIdent { ident })
                } else {
                    TCDeclInit::Default(TCExprKind::FunctionIdent { ident })
                };
                let decl = TCDecl {
                    ty,
                    init,
                    ident,
                    loc: decl.loc,
                };
                globals.add_var(None, &decl)?;

                let mut func_out = FuncEnv::new(func_decl.return_type, func_decl.loc);
                let mut func_locals = globals.child(&mut func_out, decl.loc);

                if let Some(params) = func_decl.params {
                    for param in params.params {
                        func_locals.add_param(&mut func_out, &param)?;
                    }
                }

                check_block(&mut func_locals, &mut func_out, func.statements)?;
                func_locals.close_scope(&mut func_out);

                globals.complete_func_defn(ident, func_out)?;
            }
            GlobalStatementKind::Pragma(pragma) => {}
        }
    }

    return Ok(globals.tu());
}

pub fn check_block(env: &mut TypeEnv, out: &mut FuncEnv, stmts: Block) -> Result<(), Error> {
    for stmt in stmts.stmts {
        match stmt.kind {
            BlockItemKind::Declaration(decl) => check_declaration(env, Some(out), decl)?,
            BlockItemKind::Statement(stmt) => check_stmt(env, out, stmt)?,
        }
    }

    return Ok(());
}

pub fn check_stmt(env: &mut TypeEnv, out: &mut FuncEnv, stmt: Statement) -> Result<(), Error> {
    let mut op = TCOpcode {
        kind: TCOpcodeKind::Ret,
        loc: stmt.loc,
    };

    match stmt.kind {
        StatementKind::Block(block) => {
            let mut scope = env.child(out, block.loc);
            check_block(&mut scope, out, block)?;
            scope.close_scope(out);
        }
        StatementKind::Expr(expr) => {
            let tc_expr = check_expr(env, &expr)?;
            op.kind = TCOpcodeKind::Expr(tc_expr);
            out.ops.push(op);
        }

        StatementKind::Ret => {
            op.kind = TCOpcodeKind::Ret;
            out.ops.push(op);
        }
        StatementKind::RetVal(expr) => {
            let tc_expr = check_expr(env, &expr)?;
            let or_else = || {
                error!(
                    "couldn't convert expression to return type",
                    tc_expr.loc, "expression found here"
                )
            };

            let tc_expr = env
                .assign_convert(out.return_type, tc_expr, tc_expr.loc)
                .ok_or_else(or_else)?;
            op.kind = TCOpcodeKind::RetVal(tc_expr);
            out.ops.push(op);
        }

        StatementKind::Branch {
            if_cond,
            if_body,
            else_body,
        } => {
            let cond = check_expr(&mut *env, &if_cond)?;

            let else_label = out.label();

            if env.goto_ifz(out, cond, else_label, cond.loc) {
                return Err(condition_non_primitive(cond.ty, cond.loc));
            }

            let mut if_scope = env.child(out, if_body.loc);
            check_stmt(&mut if_scope, out, *if_body)?;
            if_scope.close_scope(out);

            if let Some(else_body) = else_body {
                let end_label = out.label();

                env.goto(out, end_label, stmt.loc);
                env.label(out, else_label, stmt.loc);

                let mut else_scope = env.child(out, else_body.loc);
                check_stmt(&mut else_scope, out, *else_body)?;
                else_scope.close_scope(out);

                env.label(out, end_label, stmt.loc);
            } else {
                env.label(out, else_label, stmt.loc);
            }
        }

        StatementKind::For {
            at_start,
            condition,
            post_expr,
            body,
        } => {
            let (mut scope, cb) = env.loop_child(out, body.loc);

            if let Some(start) = at_start {
                let start = check_expr(&mut scope, &start)?;

                out.ops.push(TCOpcode {
                    kind: TCOpcodeKind::Expr(start),
                    loc: start.loc,
                });
            }

            let begin = out.label();

            scope.label(out, begin, body.loc);

            if let Some(cond) = condition {
                let cond = check_expr(&mut scope, &cond)?;
                let pass_goto = out.label();

                if scope.goto_ifnz(out, cond, pass_goto, cond.loc) {
                    return Err(condition_non_primitive(cond.ty, cond.loc));
                }

                scope.goto(out, cb.br, cond.loc);
                scope.label(out, pass_goto, cond.loc);
            }

            check_stmt(&mut scope, out, *body)?;

            scope.label(out, cb.cont, body.loc);

            if let Some(post) = post_expr {
                let post = check_expr(&mut scope, &post)?;

                out.ops.push(TCOpcode {
                    kind: TCOpcodeKind::Expr(post),
                    loc: post.loc,
                });
            }

            scope.goto(out, begin, body.loc);

            scope.close_scope(out);
            env.label(out, cb.br, body.loc);
        }
        StatementKind::ForDecl {
            decl,
            condition,
            post_expr,
            body,
        } => {
            let (mut scope, cb) = env.loop_child(out, body.loc);
            check_declaration(&mut scope, Some(out), decl)?;

            let begin = out.label();

            scope.label(out, begin, body.loc);

            if let Some(cond) = condition {
                let cond = check_expr(&mut scope, &cond)?;
                let pass_goto = out.label();

                if scope.goto_ifnz(out, cond, pass_goto, cond.loc) {
                    return Err(condition_non_primitive(cond.ty, cond.loc));
                }

                scope.goto(out, cb.br, cond.loc);
                scope.label(out, pass_goto, cond.loc);
            }

            check_stmt(&mut scope, out, *body)?;

            scope.label(out, cb.cont, body.loc);

            if let Some(post) = post_expr {
                let post = check_expr(&mut scope, &post)?;

                out.ops.push(TCOpcode {
                    kind: TCOpcodeKind::Expr(post),
                    loc: post.loc,
                });
            }

            scope.goto(out, begin, body.loc);

            scope.close_scope(out);
            env.label(out, cb.br, body.loc);
        }

        StatementKind::While { condition, body } => {
            let (mut scope, cb) = env.loop_child(out, body.loc);

            scope.label(out, cb.cont, body.loc);

            let cond = check_expr(&mut scope, &condition)?;

            let pass_goto = out.label();
            if scope.goto_ifnz(out, cond, pass_goto, cond.loc) {
                return Err(condition_non_primitive(cond.ty, cond.loc));
            }

            scope.goto(out, cb.br, cond.loc);
            scope.label(out, pass_goto, cond.loc);

            check_stmt(&mut scope, out, *body)?;

            scope.goto(out, cb.cont, body.loc);

            scope.close_scope(out);
            env.label(out, cb.br, body.loc);
        }
        StatementKind::DoWhile { condition, body } => {
            let (mut scope, cb) = env.loop_child(out, body.loc);

            let begin = out.label();
            scope.label(out, begin, body.loc);

            check_stmt(&mut scope, out, *body)?;

            scope.label(out, cb.cont, body.loc);

            let cond = check_expr(&mut scope, &condition)?;

            if scope.goto_ifz(out, cond, cb.br, cond.loc) {
                return Err(condition_non_primitive(cond.ty, cond.loc));
            }

            scope.goto(out, begin, body.loc);
            scope.close_scope(out);
            env.label(out, cb.br, body.loc);
        }

        StatementKind::Goto { label, label_loc } => {
            env.user_goto(out, label, stmt.loc);
        }
        StatementKind::Continue => {
            if env.cont(out, stmt.loc) {
                return Err(error!(
                    "continue used when not in a loop",
                    stmt.loc, "continue used here"
                ));
            }
        }
        StatementKind::Break => {
            if env.br(out, stmt.loc) {
                return Err(error!(
                    "break used when not in a loop or switch",
                    stmt.loc, "break used here"
                ));
            }
        }

        StatementKind::Switch { expr, body } => {
            let cond = check_expr(env, &expr)?;
            let (mut scope, br) = env.switch(cond, out, body.loc)?;
            check_stmt(&mut scope, out, *body)?;
            scope.close_scope(out);
            env.label(out, br, body.loc);
        }
        StatementKind::CaseLabeled {
            case_value,
            labeled,
        } => {
            let case_value = check_expr(env, &case_value)?;
            env.case(out, case_value, stmt.loc)?;
            check_stmt(env, out, *labeled)?;
        }
        StatementKind::DefaultCaseLabeled(labeled) => {
            env.default(out, stmt.loc)?;
            check_stmt(env, out, *labeled)?;
        }
        StatementKind::Labeled {
            label,
            label_loc,
            labeled,
        } => {
            env.user_label(out, label, label_loc)?;
            check_stmt(env, out, *labeled)?;
        }
    }

    return Ok(());
}

pub fn parse_union_decl(
    locals: &mut TypeEnv,
    fields: StructType,
    loc: CodeLoc,
) -> Result<TCTypeBase, Error> {
    let (id, decls) = match fields.kind {
        StructTypeKind::Named(id) => return Ok(locals.check_union_decl(id, loc)),
        StructTypeKind::NamedDecl {
            ident,
            declarations,
        } => (ident.into(), declarations),
        StructTypeKind::UnnamedDecl { declarations } => (n32::NULL, declarations),
    };

    let label = locals.open_union_defn(id, loc)?;

    let mut align = 1;
    let mut size = 0;
    let mut fields: Vec<TCStructField> = Vec::new();

    for decl in decls {
        let base = parse_spec_quals(&mut *locals, decl.specifiers)?;
        if decl.declarators.len() == 0 {
            let (loc, sa) = match base {
                TCTypeBase::UnnamedStruct { loc, sa } => (loc, sa),
                TCTypeBase::UnnamedUnion { loc, sa } => (loc, sa),
                _ => continue,
            };

            let sa_align = sa.align.into();
            align = core::cmp::max(align, sa_align);
            let aligned_size = align_u32(size, sa_align);
            let sa_size: u32 = sa.size.into();
            size = core::cmp::max(aligned_size, sa_size);

            let anon_fields = locals.get_struct_fields(LabelOrLoc::Loc(loc));
            let anon_fields = anon_fields.or_else(|| locals.get_union_fields(LabelOrLoc::Loc(loc)));
            for &field in anon_fields.unwrap() {
                if let Some(prev) = fields.iter().find(|f| f.name == field.name) {
                    return Err(error!(
                        "redeclaration of union field",
                        prev.loc, "previous here", field.loc, "redeclaration here"
                    ));
                }

                fields.push(field);
            }

            continue;
        }

        for &declarator in decl.declarators {
            let (ty, id) = check_decl(locals, base, &declarator.declarator)?;
            let name: u32 = id.into();
            let decl_loc = declarator.loc;

            let sa_size = ty.size();
            if sa_size == n32::NULL {
                return Err(error!(
                    "declared union member of incomplete type",
                    decl_loc, "declared here"
                ));
            }

            let ty = ty.to_ref(&*locals);
            let sa_size: u32 = sa_size.into();
            let sa_align: u32 = ty.align().into();

            align = core::cmp::max(align, sa_align);
            let aligned_size = align_u32(size, sa_align);
            size = core::cmp::max(aligned_size, sa_size);

            let offset = 0;
            #[rustfmt::skip]
            let field = TCStructField { name, ty, offset, loc, };
            if let Some(prev) = fields.iter().find(|f| f.name == name) {
                return Err(error!(
                    "redeclaration of union field",
                    prev.loc, "previous here", field.loc, "redeclaration here"
                ));
            }

            fields.push(field);
        }
    }

    let size = align_u32(size, align);

    let sa = SizeAlign {
        size: size.into(),
        align: align.into(),
    };
    return locals.close_union_defn(label, sa, fields);
}

pub fn parse_struct_decl(
    locals: &mut TypeEnv,
    fields: StructType,
    loc: CodeLoc,
) -> Result<TCTypeBase, Error> {
    let (id, decls) = match fields.kind {
        StructTypeKind::Named(id) => return Ok(locals.check_struct_decl(id, loc)),
        StructTypeKind::NamedDecl {
            ident,
            declarations,
        } => (ident.into(), declarations),
        StructTypeKind::UnnamedDecl { declarations } => (n32::NULL, declarations),
    };

    let label = locals.open_struct_defn(id, loc)?;

    let mut align = 1;
    let mut size = 0;
    let mut fields: Vec<TCStructField> = Vec::new();

    if decls.len() == 0 {
        let sa = SizeAlign {
            size: size.into(),
            align: align.into(),
        };
        return locals.close_struct_defn(label, sa, fields);
    }

    for decl in &decls[..(decls.len() - 1)] {
        let base = parse_spec_quals(&mut *locals, decl.specifiers)?;
        if decl.declarators.len() == 0 {
            let (loc, sa) = match base {
                TCTypeBase::UnnamedStruct { loc, sa } => (loc, sa),
                TCTypeBase::UnnamedUnion { loc, sa } => (loc, sa),
                _ => continue,
            };

            let sa_align = sa.align.into();
            align = core::cmp::max(align, sa_align);
            let offset = align_u32(size, sa_align);
            let sa_size: u32 = sa.size.into();
            size = offset + sa_size;

            let anon_fields = locals.get_struct_fields(LabelOrLoc::Loc(loc));
            let anon_fields = anon_fields.or_else(|| locals.get_union_fields(LabelOrLoc::Loc(loc)));
            for &field in anon_fields.unwrap() {
                let mut field = field;
                field.offset += offset;

                if let Some(prev) = fields.iter().find(|f| f.name == field.name) {
                    return Err(error!(
                        "redeclaration of struct field",
                        prev.loc, "previous here", field.loc, "redeclaration here"
                    ));
                }

                fields.push(field);
            }

            continue;
        }

        for &declarator in decl.declarators {
            // add field
            let (ty, id) = check_decl(locals, base, &declarator.declarator)?;
            let name: u32 = id.into();
            let decl_loc = declarator.loc;

            let sa_size = ty.size();
            if sa_size == n32::NULL {
                return Err(error!(
                    "declared struct member of incomplete type",
                    decl_loc, "declared here"
                ));
            }

            let ty = ty.to_ref(&*locals);
            let sa_size: u32 = sa_size.into();
            let sa_align: u32 = ty.align().into();

            align = core::cmp::max(align, sa_align);
            let offset = align_u32(size, sa_align);
            size = offset + sa_size;

            #[rustfmt::skip]
            let field = TCStructField { name, ty, offset, loc, };
            if let Some(prev) = fields.iter().find(|f| f.name == name) {
                return Err(error!(
                    "redeclaration of struct field",
                    prev.loc, "previous here", field.loc, "redeclaration here"
                ));
            }

            fields.push(field);
        }
    }

    let decl = *decls.last().unwrap();
    let base = parse_spec_quals(&mut *locals, decl.specifiers)?;
    if decl.declarators.len() == 0 {
        if let TCTypeBase::UnnamedStruct { loc, sa } = base {
            let sa_align = sa.align.into();
            align = core::cmp::max(align, sa_align);
            let offset = align_u32(size, sa_align);
            let sa_size: u32 = sa.size.into();
            size = offset + sa_size;

            let anon_fields = locals.get_struct_fields(LabelOrLoc::Loc(loc));
            let anon_fields = anon_fields.or_else(|| locals.get_union_fields(LabelOrLoc::Loc(loc)));
            for &field in anon_fields.unwrap() {
                let mut field = field;
                field.offset += offset;

                if let Some(prev) = fields.iter().find(|f| f.name == field.name) {
                    return Err(error!(
                        "redeclaration of struct field",
                        prev.loc, "previous here", field.loc, "redeclaration here"
                    ));
                }

                fields.push(field);
            }
        }
    } else {
        for &declarator in &decl.declarators[..(decl.declarators.len() - 1)] {
            // add field
            let (ty, id) = check_decl(locals, base, &declarator.declarator)?;
            let name: u32 = id.into();
            let decl_loc = declarator.loc;

            let sa_size = ty.size();
            if sa_size == n32::NULL {
                return Err(error!(
                    "declared struct member of incomplete type",
                    decl_loc, "declared here"
                ));
            }

            let ty = ty.to_ref(&*locals);
            let sa_size: u32 = sa_size.into();
            let sa_align: u32 = ty.align().into();

            align = core::cmp::max(align, sa_align);
            let offset = align_u32(size, sa_align);
            size = offset + sa_size;

            #[rustfmt::skip]
            let field = TCStructField { name, ty, offset, loc, };
            if let Some(prev) = fields.iter().find(|f| f.name == name) {
                return Err(error!(
                    "redeclaration of struct field",
                    prev.loc, "previous here", field.loc, "redeclaration here"
                ));
            }

            fields.push(field);
        }

        let declarator = *decl.declarators.last().unwrap();
        let (ty, id) = check_decl(locals, base, &declarator.declarator)?;
        let name: u32 = id.into();
        let decl_loc = declarator.loc;

        let mut sa_size = ty.size();
        if sa_size == n32::NULL {
            if !ty.is_array() {
                return Err(error!(
                    "declared struct member of incomplete type",
                    decl_loc, "declared here"
                ));
            }

            sa_size = 0u32.into();
        }

        let ty = ty.to_ref(&*locals);
        let sa_size: u32 = sa_size.into();
        let sa_align: u32 = ty.align().into();

        align = core::cmp::max(align, sa_align);
        let offset = align_u32(size, sa_align);
        size = offset + sa_size;

        #[rustfmt::skip]
        let field = TCStructField { name, ty, offset, loc, };
        if let Some(prev) = fields.iter().find(|f| f.name == name) {
            return Err(error!(
                "redeclaration of struct field",
                prev.loc, "previous here", field.loc, "redeclaration here"
            ));
        }

        fields.push(field);
    }

    let size = align_u32(size, align);

    let sa = SizeAlign {
        size: size.into(),
        align: align.into(),
    };
    return locals.close_struct_defn(label, sa, fields);
}

pub fn parse_spec_quals(
    locals: &mut TypeEnv,
    spec_quals: &[SpecifierQualifier],
) -> Result<TCTypeBase, Error> {
    let mut ds = TypeDeclSpec::new();

    for spec_qual in spec_quals {
        // use crate::new_ast::TypeQualifier as TyQual;
        use crate::ast::TypeSpecifier as TySpec;
        use SpecifierQualifierKind::*;
        match spec_qual.kind {
            TypeQualifier(qual) => {}

            TypeSpecifier(TySpec::Ident(id)) => {
                let ty = locals.check_typedef(id, spec_qual.loc)?;
                return Ok(ty);
            }
            TypeSpecifier(TySpec::Union(fields)) => {
                unimplemented!()
            }
            TypeSpecifier(TySpec::Struct(fields)) => {
                return parse_struct_decl(&mut *locals, fields, spec_qual.loc)
            }

            TypeSpecifier(TySpec::Void) => {
                return Ok(TCTypeBase::Void);
            }

            TypeSpecifier(TySpec::Char) => {
                ds.char = ds.char.saturating_add(1);
            }
            TypeSpecifier(TySpec::Short) => {
                ds.short = ds.short.saturating_add(1);
            }
            TypeSpecifier(TySpec::Int) => {
                ds.int = ds.int.saturating_add(1);
            }
            TypeSpecifier(TySpec::Long) => {
                ds.long = ds.long.saturating_add(1);
            }
            TypeSpecifier(TySpec::Signed) => {
                ds.signed = ds.signed.saturating_add(1);
            }
            TypeSpecifier(TySpec::Unsigned) => {
                ds.unsigned = ds.unsigned.saturating_add(1);
            }
            TypeSpecifier(TySpec::Float) => {
                ds.float = ds.float.saturating_add(1);
            }
            TypeSpecifier(TySpec::Double) => {
                ds.double = ds.double.saturating_add(1);
            }
        }
    }

    let begin = spec_quals[0].loc;
    let end = spec_quals.last().unwrap().loc;

    let or_else = || {
        error!(
            "incorrect type specifiers",
            l_from(begin, end),
            "specifiers found here"
        )
    };
    let base = *CORRECT_TYPES.get(&ds).ok_or_else(or_else)?;

    return Ok(base);
}

pub fn parse_decl_specs(
    locals: &mut TypeEnv,
    decl_specs: &[DeclarationSpecifier],
) -> Result<(StorageClass, TCTypeBase), Error> {
    let mut sc = StorageClass::Default;
    let mut ds = TypeDeclSpec::new();

    for decl_spec in decl_specs {
        // use crate::new_ast::TypeQualifier as TyQual;
        use crate::ast::TypeSpecifier as TySpec;
        use DeclarationSpecifierKind::*;
        match decl_spec.kind {
            Extern => {
                sc = StorageClass::Extern;
            }
            Static => {
                sc = StorageClass::Static;
            }
            Typedef => {
                sc = StorageClass::Typedef;
            }
            Register => {}

            TypeQualifier(qual) => {}
            Inline => {}
            Noreturn => {}

            TypeSpecifier(TySpec::Ident(id)) => {
                let ty = locals.check_typedef(id, decl_spec.loc)?;
                return Ok((sc, ty));
            }
            TypeSpecifier(TySpec::Union(fields)) => {
                return Ok((sc, parse_union_decl(&mut *locals, fields, decl_spec.loc)?))
            }
            TypeSpecifier(TySpec::Struct(fields)) => {
                return Ok((sc, parse_struct_decl(&mut *locals, fields, decl_spec.loc)?))
            }

            TypeSpecifier(TySpec::Void) => {
                return Ok((sc, TCTypeBase::Void));
            }

            TypeSpecifier(TySpec::Char) => {
                ds.char = ds.char.saturating_add(1);
            }
            TypeSpecifier(TySpec::Short) => {
                ds.short = ds.short.saturating_add(1);
            }
            TypeSpecifier(TySpec::Int) => {
                ds.int = ds.int.saturating_add(1);
            }
            TypeSpecifier(TySpec::Long) => {
                ds.long = ds.long.saturating_add(1);
            }
            TypeSpecifier(TySpec::Signed) => {
                ds.signed = ds.signed.saturating_add(1);
            }
            TypeSpecifier(TySpec::Unsigned) => {
                ds.unsigned = ds.unsigned.saturating_add(1);
            }
            TypeSpecifier(TySpec::Float) => {
                ds.float = ds.float.saturating_add(1);
            }
            TypeSpecifier(TySpec::Double) => {
                ds.double = ds.double.saturating_add(1);
            }
        }
    }

    let begin = decl_specs[0].loc;
    let end = decl_specs.last().unwrap().loc;

    let or_else = || {
        error!(
            "incorrect type specifiers",
            l_from(begin, end),
            "specifiers found here"
        )
    };
    let base = *CORRECT_TYPES.get(&ds).ok_or_else(or_else)?;

    return Ok((sc, base));
}

pub fn check_func_defn_decl(
    locals: &mut TypeEnv,
    decl: &FunctionDefinition,
) -> Result<TCFunctionDeclarator, Error> {
    let (sc, base) = parse_decl_specs(locals, decl.specifiers)?;
    let mut rtype = TCTypeOwned::new(base);

    for modifier in decl.pointer {
        // TODO warn when there are qualifiers
        rtype.mods.push(TCTypeModifier::Pointer);
    }

    let params_decl = if let Some(params) = decl.params {
        params
    } else {
        return Ok(TCFunctionDeclarator {
            is_static: let_expr!(StorageClass::Static = sc),
            return_type: rtype.to_ref(locals),
            ident: decl.ident,
            params: None,
            loc: decl.loc,
        });
    };

    let params = check_param_types(locals, params_decl.parameters)?;
    let mut out = Vec::new();
    for (idx, (ty, id)) in params.into_iter().enumerate() {
        if id == n32::NULL {
            return Err(error!(
                "missing identifier",
                params_decl.parameters[idx].loc, "parameter found here"
            ));
        } else {
            out.push(TCParamDecl {
                ty,
                ident: id.into(),
                loc: params_decl.parameters[idx].loc,
            });
        }
    }

    return Ok(TCFunctionDeclarator {
        is_static: let_expr!(StorageClass::Static = sc),
        return_type: rtype.to_ref(&*locals),
        ident: decl.ident,
        params: Some(TCParamsDeclarator {
            params: locals.add_array(out),
            varargs: params_decl.varargs,
        }),
        loc: decl.loc,
    });
}

pub fn check_decl(
    locals: &mut TypeEnv,
    base: TCTypeBase,
    decl: &Declarator,
) -> Result<(TCTypeOwned, n32), Error> {
    let (ty, id) = check_decl_rec(locals, base, decl)?;

    let mut was_array = false;
    let mut was_function = false;
    for modifier in &ty.mods {
        match modifier {
            TCTypeModifier::Array(_) | TCTypeModifier::VariableArray => {
                if was_function {
                    return Err(error!(
                        "function type returns array (functions can't return arrays in C)",
                        decl.loc, "type declaration found here"
                    ));
                }

                was_array = true;
                was_function = false;
            }
            TCTypeModifier::BeginParam(_)
            | TCTypeModifier::UnknownParams
            | TCTypeModifier::NoParams => {
                if was_array {
                    return Err(error!(
                        "type is an array of functions (functions don't have a constant size\
                            so you can't have an array of them)",
                        decl.loc, "type declaration found here"
                    ));
                }

                was_function = true;
                was_array = false;
            }
            TCTypeModifier::VarargsParam | TCTypeModifier::Param(_) => {}
            TCTypeModifier::Pointer => {
                was_array = false;
                was_function = false;
            }
        }
    }

    if was_function && ty.ignore_mods().is_array() {
        return Err(error!(
            "function type returns array (functions can't return arrays in C)",
            decl.loc, "type declaration found here"
        ));
    }

    if was_array && !ty.ignore_mods().is_complete() {
        return Err(error!(
            "can't have an array of an incomplete type",
            decl.loc, "this is an array type"
        ));
    }

    return Ok((ty, id));
}

pub fn check_param_types(
    locals: &mut TypeEnv,
    params: &[ParameterDeclaration],
) -> Result<Vec<(TCType, n32)>, Error> {
    let param = params[0];
    let (sc, param_base) = parse_decl_specs(&mut *locals, param.specifiers)?;
    let (mut param_type, id) = if let Some(decl) = param.declarator {
        let (tc_type, id) = check_decl(&mut *locals, param_base, &decl)?;
        (tc_type, id)
    } else {
        (TCTypeOwned::new(param_base), n32::NULL)
    };

    debug_assert!(let_expr!(StorageClass::Default = sc));

    if param_type.is_void() {
        if params.len() == 1 && id == n32::NULL {
            return Ok(Vec::new());
        } else {
            return Err(error!(
                "cannot have a parameter of type void",
                param.loc, "parameter found here"
            ));
        }
    }

    param_type.canonicalize_param();
    let param_type = param_type.to_ref(&*locals);

    let mut out = Vec::new();
    out.push((param_type, id));

    for param in &params[1..] {
        let (sc, base) = parse_decl_specs(&mut *locals, param.specifiers)?;
        let (mut param_type, id) = if let Some(decl) = param.declarator {
            let (tc_type, id) = check_decl(&mut *locals, base, &decl)?;
            (tc_type, id)
        } else {
            (TCTypeOwned::new(base), n32::NULL)
        };

        debug_assert!(let_expr!(StorageClass::Default = sc));

        if param_type.is_void() {
            return Err(error!(
                "cannot have a parameter of type void",
                param.loc, "parameter found here"
            ));
        }

        param_type.canonicalize_param();
        let param_type = param_type.to_ref(&*locals);
        out.push((param_type, id));
    }

    return Ok(out);
}

pub fn check_decl_rec(
    locals: &mut TypeEnv,
    base: TCTypeBase,
    decl: &Declarator,
) -> Result<(TCTypeOwned, n32), Error> {
    let (mut tc_type, ident) = match decl.kind {
        DeclaratorKind::Declarator(decl) => check_decl_rec(locals, base, decl)?,
        DeclaratorKind::Identifier(ident) => (TCTypeOwned::new(base), ident.into()),
        DeclaratorKind::Abstract => (TCTypeOwned::new(base), n32::NULL),
    };

    use DerivedDeclaratorKind as DDK;

    for derived in decl.derived {
        match derived.kind {
            DDK::Array(array_decl) => {
                if array_decl.qualifiers.len() != 0 {
                    return Err(error!(
                        "TCI doesn't handle array qualifiers right now",
                        array_decl.loc, "qualfiiers found around here"
                    ));
                }

                match array_decl.size.kind {
                    ArraySizeKind::Unknown => {
                        tc_type.mods.push(TCTypeModifier::VariableArray);
                    }
                    ArraySizeKind::VariableExpression(expr) => {
                        let expr = eval_expr(check_expr(locals, expr)?)?;
                        let loc = expr.loc;
                        let expr = match expr.kind {
                            TCExprKind::U32Lit(i) => i as u64,
                            TCExprKind::I32Lit(i) => i.try_into().map_err(neg_arr_size(loc))?,
                            TCExprKind::I64Lit(i) => i.try_into().map_err(neg_arr_size(loc))?,
                            TCExprKind::U64Lit(i) => i,
                            x => {
                                return Err(error!(
                                    "cannot use expression as array type",
                                    loc, "expression is not a constant"
                                ))
                            }
                        };

                        tc_type.mods.push(TCTypeModifier::Array(expr as u32));
                    }
                }
            }
            DDK::Function(func) => {
                let params = check_param_types(locals, func.parameters)?;
                if params.len() == 0 {
                    tc_type.mods.push(TCTypeModifier::NoParams);
                    continue;
                }

                tc_type.mods.push(TCTypeModifier::BeginParam(params[0].0));
                for param in &params[1..] {
                    tc_type.mods.push(TCTypeModifier::Param(param.0));
                }

                if func.varargs {
                    tc_type.mods.push(TCTypeModifier::VarargsParam);
                }
            }
            DDK::EmptyFunction => {
                tc_type.mods.push(TCTypeModifier::UnknownParams);
            }
            DDK::Pointer(ptr_qual) => {
                // TODO warn when there are qualifiers
                tc_type.mods.push(TCTypeModifier::Pointer);
            }
        }
    }

    return Ok((tc_type, ident));
}

pub fn check_initializer_list(
    locals: &mut TypeEnv,
    mut target: TCTypeOwned,
    init: &[Expr],
    decl_loc: CodeLoc,
) -> Result<(TCExprKind, TCType), Error> {
    let deref = target.deref().map(|a| a.to_ty_owned());
    if let Some(array_mod) = target.array_mod() {
        let elem_ty = deref.unwrap().to_ref(&*locals);

        let mut tc_exprs = Vec::new();
        for expr in init {
            let tc_expr = check_expr(&mut *locals, expr)?;
            let or_else = || conversion_error(elem_ty, decl_loc, &tc_expr);
            let tc_expr = locals
                .assign_convert(elem_ty, tc_expr, tc_expr.loc)
                .ok_or_else(or_else)?;
            tc_exprs.push((tc_expr.kind, tc_expr.loc));
        }

        let array_init = match array_mod {
            TCTypeModifier::Array(arr) => {
                tc_exprs.resize(*arr as usize, (TCExprKind::Uninit, decl_loc));
                let elems = locals.add_array(tc_exprs);

                TCExprKind::ArrayInit { elems, elem_ty }
            }
            x @ TCTypeModifier::VariableArray => {
                *x = TCTypeModifier::Array(tc_exprs.len() as u32); // TODO overflow
                let elems = locals.add_array(tc_exprs);

                TCExprKind::ArrayInit { elems, elem_ty }
            }
            _ => unreachable!(),
        };

        return Ok((array_init, target.to_ref(&*locals)));
    }

    let target = target.to_ref(&*locals);
    let or_else = || {
        error!(
            "can only use initializer lists on structs and arrays",
            decl_loc,
            format!("this has type {}", target.display(locals.symbols()))
        )
    };

    let (is_struct, id) = target.get_id_strict().ok_or_else(or_else)?;
    if !is_struct {
        return Err(or_else());
    }

    let mut written_fields = Vec::new();
    let fields = get_fields(&*locals, target).ok_or_else(or_else)?;
    let fields = locals.get_struct_fields(id).ok_or_else(or_else)?;
    let mut offset = None;
    for (field, expr) in fields.iter().zip(init.iter()) {
        if let Some(offset) = offset {
            if field.offset < offset {
                return Err(error!(
                    "can only use initializer lists on simple structs",
                    decl_loc,
                    format!("this has type {}", target.display(locals.symbols()))
                ));
            }
        }
        offset = Some(field.offset);

        let tc_expr = check_expr(&mut *locals, expr)?;
        let or_else = || conversion_error(field.ty, decl_loc, &tc_expr);
        let tc_expr = locals
            .assign_convert(field.ty, tc_expr, tc_expr.loc)
            .ok_or_else(or_else)?;
        written_fields.push(tc_expr);
    }

    let (fields, size) = (locals.add_array(written_fields), target.repr_size());
    return Ok((TCExprKind::StructLit { fields, size }, target));
}

pub fn check_declaration(
    locals: &mut TypeEnv,
    mut out: Option<&mut FuncEnv>,
    declaration: Declaration,
) -> Result<(), Error> {
    let (sc, base) = parse_decl_specs(locals, declaration.specifiers)?;

    if let StorageClass::Typedef = sc {
        debug_assert!(declaration.declarators.len() == 1);
        let init_declarator = &declaration.declarators[0];
        debug_assert!(init_declarator.initializer.is_none());

        let (ty, id) = check_decl(&mut *locals, base, &init_declarator.declarator)?;
        let (ty, ident) = (ty.to_ref(&*locals), id.into());
        let loc = declaration.loc;

        locals.add_typedef(ty, ident, loc);
        return Ok(());
    }

    for decl in declaration.declarators {
        let (ty, id) = check_decl(&mut *locals, base, &decl.declarator)?;
        let ident: u32 = id.into();
        let loc = decl.loc;

        let (init, ty) = if let Some(init) = decl.initializer {
            let (init, ty) = match init.kind {
                InitializerKind::Expr(expr) => {
                    let tc_expr = check_expr(&mut *locals, expr)?;
                    let ty = ty.to_ref(&*locals);
                    let or_else = || conversion_error(ty, decl.declarator.loc, &tc_expr);
                    let tc_expr = locals
                        .assign_convert(ty, tc_expr, decl.declarator.loc)
                        .ok_or_else(or_else)?;

                    (tc_expr.kind, ty)
                }

                // Simple form of initializer lists
                InitializerKind::List(exprs) => {
                    check_initializer_list(&mut *locals, ty, exprs, decl.declarator.loc)?
                }
            };

            let init = match sc {
                StorageClass::Extern => TCDeclInit::ExternInit(init),
                StorageClass::Default => TCDeclInit::Default(init),
                StorageClass::Static => TCDeclInit::Static(init),
                StorageClass::Typedef => unreachable!(),
            };
            (init, ty)
        } else {
            let init = match sc {
                StorageClass::Extern => TCDeclInit::Extern,
                StorageClass::Default => TCDeclInit::DefaultUninit,
                StorageClass::Static => TCDeclInit::Static(TCExprKind::Uninit),
                StorageClass::Typedef => unreachable!(),
            };

            let ty = ty.to_ref(&*locals);
            (init, ty)
        };

        if !ty.is_complete() {
            return Err(variable_incomplete_type(ty, decl.loc));
        }

        let tc_decl = TCDecl {
            ident,
            init,
            ty,
            loc,
        };
        locals.add_var(out.as_deref_mut(), &tc_decl)?;
    }

    return Ok(());
}

pub fn eval_expr(expr: TCExpr) -> Result<TCExpr, Error> {
    // TODO cmon man
    match expr.kind {
        TCExprKind::I32Lit(i) => return Ok(expr),
        TCExprKind::U32Lit(i) => return Ok(expr),
        TCExprKind::U64Lit(i) => return Ok(expr),
        TCExprKind::I64Lit(i) => return Ok(expr),
        _ => {
            return Err(error!(
                "cannot evaluate constant expression",
                expr.loc, "expression found here"
            ))
        }
    }
}

pub fn check_expr(env: &mut TypeEnv, expr: &Expr) -> Result<TCExpr, Error> {
    match expr.kind {
        ExprKind::IntLit(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::I32Lit(val),
                ty: TCType::new(TCTypeBase::I32),
                loc: expr.loc,
            });
        }
        ExprKind::LongLit(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::I64Lit(val),
                ty: TCType::new(TCTypeBase::I64),
                loc: expr.loc,
            });
        }
        ExprKind::ULit(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::U32Lit(val),
                ty: TCType::new(TCTypeBase::U32),
                loc: expr.loc,
            });
        }
        ExprKind::ULongLit(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::U64Lit(val),
                ty: TCType::new(TCTypeBase::U64),
                loc: expr.loc,
            });
        }
        ExprKind::FloatLit(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::F32Lit(val),
                ty: TCType::new(TCTypeBase::F32),
                loc: expr.loc,
            });
        }
        ExprKind::DoubleLit(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::F64Lit(val),
                ty: TCType::new(TCTypeBase::F64),
                loc: expr.loc,
            });
        }
        ExprKind::StringLit(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::StringLit(env.add_str(val)),
                ty: TCType::new_ptr(TCTypeBase::I8),
                loc: expr.loc,
            });
        }
        ExprKind::CharLit(c) => {
            return Ok(TCExpr {
                kind: TCExprKind::I8Lit(c),
                ty: TCType::new(TCTypeBase::I8),
                loc: expr.loc,
            });
        }
        ExprKind::Ident(id) => return env.ident(id, expr.loc),

        ExprKind::ParenList(exprs) => {
            let mut tc_exprs = Vec::new();
            for expr in exprs {
                tc_exprs.push(check_expr(env, expr)?);
            }

            if tc_exprs.len() == 0 {
                // happens in for loops
                return Ok(TCExpr {
                    ty: TCType::new(TCTypeBase::I8),
                    kind: TCExprKind::I8Lit(0),
                    loc: expr.loc,
                });
            }

            return Ok(TCExpr {
                ty: tc_exprs[tc_exprs.len() - 1].ty,
                kind: TCExprKind::ParenList(env.add_array(tc_exprs)),
                loc: expr.loc,
            });
        }

        ExprKind::SizeofTy(ast_ty) => {
            let base = parse_spec_quals(&mut *env, ast_ty.specifiers)?;
            let ty = if let Some(decl) = ast_ty.declarator {
                let (ty, id) = check_decl(&mut *env, base, &decl)?;
                assert!(id == n32::NULL);
                ty.to_ref(&*env)
            } else {
                TCType { base, mods: &[] }
            };

            let size = ty.size().unwrap_or_else(|| ty.repr_size());

            return Ok(TCExpr {
                kind: TCExprKind::U64Lit(size as u64),
                ty: TCType::new(TCTypeBase::U64),
                loc: expr.loc,
            });
        }
        ExprKind::SizeofExpr(e) => {
            let expr = check_expr(&mut *env, e)?;
            let size = expr.ty.size().unwrap_or_else(|| expr.ty.repr_size());

            return Ok(TCExpr {
                kind: TCExprKind::U64Lit(size as u64),
                ty: TCType::new(TCTypeBase::U64),
                loc: expr.loc,
            });
        }

        ExprKind::BinOp(op, l, r) => return check_bin_op(&mut *env, op, l, r, expr.loc),

        ExprKind::UnaryOp(op, operand) => return check_un_op(&mut *env, op, operand, expr.loc),

        ExprKind::Assign { op, to, val } => {
            let target = check_assign_target(&mut *env, to)?;
            let val = check_expr(&mut *env, val)?;

            if let AssignOp::MutAssign(op) = op {
                let or_else = || bin_assign_op_non_primitive(target.ty, target.loc);
                let op_type = target.ty.to_prim_type().ok_or_else(or_else)?;

                if op == BinOp::LShift || op == BinOp::RShift {
                    if !target.ty.is_integer() || !val.ty.is_integer() {
                        return Err(invalid_bin_op_assign(&target, &val));
                    }

                    let op_type = target.ty.to_prim_type().unwrap();
                    let or_else = || bitshift_conversion_error(env.symbols(), &val);
                    let val = env
                        .assign_convert(TCType::new(TCTypeBase::I8), val, expr.loc)
                        .ok_or_else(or_else)?;

                    let (value, ty) = (env.add(val), target.ty);

                    #[rustfmt::skip]
                    return Ok(TCExpr {
                        kind: TCExprKind::MutAssign { target, value, op_type, op },
                        ty: target.ty,
                        loc: expr.loc,
                    });
                }

                let or_else = || conversion_error(target.ty, to.loc, &val);
                let val = env
                    .assign_convert(target.ty, val, expr.loc)
                    .ok_or_else(or_else)?;
                let value = env.add(val);
                return Ok(TCExpr {
                    kind: TCExprKind::MutAssign {
                        target,
                        value,
                        op_type,
                        op,
                    },
                    ty: target.ty,
                    loc: expr.loc,
                });
            } else {
                let or_else = || conversion_error(target.ty, to.loc, &val);
                let val = env
                    .assign_convert(target.ty, val, expr.loc)
                    .ok_or_else(or_else)?;
                let value = env.add(val);

                return Ok(TCExpr {
                    kind: TCExprKind::Assign { target, value },
                    ty: target.ty,
                    loc: expr.loc,
                });
            }
        }

        ExprKind::Cast { to, from } => {
            let base = parse_spec_quals(&mut *env, to.specifiers)?;
            let ty = if let Some(decl) = to.declarator {
                let (ty, id) = check_decl(&mut *env, base, &decl)?;
                assert!(id == n32::NULL);
                ty.to_ref(&*env)
            } else {
                TCType { base, mods: &[] }
            };
            let from = check_expr(&mut *env, from)?;

            let or_else = || conversion_error(ty, to.loc, &from);
            return env.assign_convert(ty, from, expr.loc).ok_or_else(or_else);
        }

        ExprKind::Ternary {
            condition,
            if_true,
            if_false,
        } => {
            let cond = check_expr(&mut *env, condition)?;
            let or_else = || condition_non_primitive(cond.ty, cond.loc);
            let cond_ty = cond.ty.to_prim_type().ok_or_else(or_else)?;

            let if_true = check_expr(&mut *env, if_true)?;
            let if_false = check_expr(&mut *env, if_false)?;

            let (if_true, if_false) = if TCType::ty_eq(&if_true.ty, &if_false.ty) {
                (if_true, if_false)
            } else {
                let (ift, iff, _) = prim_unify(&mut *env, if_true, if_false)?;
                (ift, iff)
            };

            let (condition, if_true, if_false) = env.add((cond, if_true, if_false));

            return Ok(TCExpr {
                kind: TCExprKind::Ternary {
                    condition,
                    cond_ty,
                    if_true,
                    if_false,
                },
                ty: if_true.ty,
                loc: expr.loc,
            });
        }

        ExprKind::Member { base, member } => {
            let base = check_expr(&mut *env, base)?;
            let field = check_field_access(&mut *env, base.ty, member, expr.loc)?;

            return Ok(TCExpr {
                ty: field.ty,
                loc: expr.loc,
                kind: TCExprKind::Member {
                    base: env.add(base),
                    offset: field.offset,
                },
            });
        }
        ExprKind::PtrMember { base, member } => {
            let base = check_expr(&mut *env, base)?;
            let or_else = || not_a_struct_pointer(env.symbols(), base.ty, base.loc);
            let base_ty = base.ty.deref().ok_or_else(or_else)?;
            let field = check_field_access(&mut *env, base_ty, member, expr.loc)?;

            return Ok(TCExpr {
                ty: field.ty,
                loc: expr.loc,
                kind: TCExprKind::PtrMember {
                    base: env.add(base),
                    offset: field.offset,
                },
            });
        }

        ExprKind::Call { function, params } => {
            if let ExprKind::Ident(id) = function.kind {
                if let Some(trans) = BUILTINS.get(&id) {
                    return trans(env, expr.loc, params);
                }
            }

            let func = check_expr(&mut *env, function)?;
            let func_type = if let Some(f) = func.ty.to_func_type(&*env) {
                f
            } else {
                return Err(error!(
                    "can't call expression",
                    func.loc,
                    format!(
                        "expr found here to be type {}",
                        func.ty.display(env.symbols())
                    )
                ));
            };

            let mut tparams = Vec::new();
            let default_conversion_params = if let Some(ftype_params) = func_type.params {
                if params.len() < ftype_params.types.len()
                    || (params.len() > ftype_params.types.len() && !ftype_params.varargs)
                {
                    return Err(error!(
                        "function call has wrong number of parameters",
                        expr.loc,
                        "function called here" // TODO say what the type of the function is
                    ));
                }

                let typed_params = &params[..ftype_params.types.len()];
                for (idx, param) in typed_params.iter().enumerate() {
                    let mut expr = check_expr(&mut *env, param)?;
                    let param_type = ftype_params.types[idx];
                    let or_else = || param_conversion_error(param_type, &expr);
                    expr = env
                        .assign_convert(param_type, expr, expr.loc)
                        .ok_or_else(or_else)?;

                    tparams.push(expr);
                }

                &params[ftype_params.types.len()..]
            } else {
                params
            };

            for param in default_conversion_params {
                let mut expr = check_expr(env, param)?;
                if expr.ty.is_integer() && expr.ty.repr_size() < 4 {
                    expr = env
                        .assign_convert(TCType::new(TCTypeBase::I32), expr, expr.loc)
                        .unwrap();
                } else if expr.ty.is_floating_pt() {
                    expr = env
                        .assign_convert(TCType::new(TCTypeBase::F64), expr, expr.loc)
                        .unwrap();
                }

                tparams.push(expr);
            }

            let func = env.add(func);
            let params = env.add_array(tparams);
            return Ok(TCExpr {
                kind: TCExprKind::Call { func, params },
                ty: func_type.return_type,
                loc: expr.loc,
            });
        }
    }
}

pub fn check_bin_op(
    env: &mut TypeEnv,
    op: BinOp,
    l: &Expr,
    r: &Expr,
    loc: CodeLoc,
) -> Result<TCExpr, Error> {
    match op {
        BinOp::BoolOr => {
            let l = check_expr(&mut *env, l)?;
            let r = check_expr(&mut *env, r)?;
            let or_else = || condition_non_primitive(l.ty, l.loc);
            let cond_ty = l.ty.to_prim_type().ok_or_else(or_else)?;
            let or_else = || condition_non_primitive(r.ty, r.loc);
            let op_type = r.ty.to_prim_type().ok_or_else(or_else)?;

            let if_true = TCExpr {
                kind: TCExprKind::I8Lit(1),
                ty: TCType::new(TCTypeBase::I8),
                loc: l.loc,
            };

            let (op, operand) = (TCUnaryOp::BoolNorm, env.add(r));
            #[rustfmt::skip]
            let if_false = TCExpr {
                kind: TCExprKind::UnaryOp { op, op_type, operand, },
                ty: TCType::new(TCTypeBase::I8),
                loc: r.loc,
            };

            let (condition, if_true, if_false) = (env.add(l), env.add(if_true), env.add(if_false));

            #[rustfmt::skip]
            return Ok(TCExpr {
                kind: TCExprKind::Ternary { condition, cond_ty, if_true, if_false, },
                ty: TCType::new(TCTypeBase::I8),
                loc,
            });
        }
        BinOp::BoolAnd => {
            let l = check_expr(&mut *env, l)?;
            let r = check_expr(&mut *env, r)?;
            let or_else = || condition_non_primitive(l.ty, l.loc);
            let cond_ty = l.ty.to_prim_type().ok_or_else(or_else)?;
            let or_else = || condition_non_primitive(r.ty, r.loc);
            let op_type = r.ty.to_prim_type().ok_or_else(or_else)?;

            let if_false = TCExpr {
                kind: TCExprKind::I8Lit(0),
                ty: TCType::new(TCTypeBase::I8),
                loc: l.loc,
            };

            let (op, operand) = (TCUnaryOp::BoolNorm, env.add(r));
            #[rustfmt::skip]
            let if_true = TCExpr {
                kind: TCExprKind::UnaryOp { op, op_type, operand, },
                ty: TCType::new(TCTypeBase::I8),
                loc: r.loc,
            };

            let (condition, if_true, if_false) = (env.add(l), env.add(if_true), env.add(if_false));

            #[rustfmt::skip]
            return Ok(TCExpr {
                kind: TCExprKind::Ternary { condition, cond_ty, if_true, if_false, },
                ty: TCType::new(TCTypeBase::I8),
                loc,
            });
        }
        BinOp::Index => {
            let sum = check_bin_op(&mut *env, BinOp::Add, l, r, loc)?;
            let or_else = || error!("cannot dereference value", loc, "value found here");
            let ty = sum.ty.deref().ok_or_else(or_else)?;

            return Ok(TCExpr {
                kind: TCExprKind::Deref(env.add(sum)),
                ty,
                loc,
            });
        }
        _ => {}
    }

    let l = check_expr(&mut *env, l)?;
    let r = check_expr(&mut *env, r)?;
    let ptype_err =
        |loc: CodeLoc| move || error!("couldn't do operation on value", loc, "value found here");

    if l.ty.is_pointer() || l.ty.is_array() || r.ty.is_pointer() || r.ty.is_array() {
        // allowed operations are addition w/ integer, subtraction w/ integer/pointer

        match op {
            BinOp::Add => {
                let (ptr, int) = if r.ty.is_integer() {
                    (l, r)
                } else if l.ty.is_integer() {
                    (r, l)
                } else {
                    return Err(invalid_bin_op(&l, &r));
                };

                let stride = ptr.ty.pointer_stride();
                if stride == n32::NULL {
                    return Err(error!(
                        "cannot perform arithmetic on pointer type",
                        l.loc, "pointer found here"
                    ));
                }

                let stride: u32 = stride.into();

                let ptr_prim = ptr.ty.to_prim_type().ok_or_else(ptype_err(ptr.loc))?;
                let int_prim = int.ty.to_prim_type().ok_or_else(ptype_err(int.loc))?;

                let int = if int_prim.signed() {
                    let int = TCExpr {
                        kind: TCExprKind::Conv {
                            from: int_prim,
                            to: TCPrimType::I64,
                            expr: env.add(int),
                        },
                        ty: TCType::new(TCTypeBase::I64),
                        loc: int.loc,
                    };

                    let elem_size = TCExpr {
                        loc: l.loc,
                        kind: TCExprKind::I64Lit(stride as i64),
                        ty: TCType::new(TCTypeBase::I64),
                    };

                    TCExpr {
                        loc: int.loc,
                        kind: TCExprKind::BinOp {
                            op: BinOp::Mul,
                            op_type: TCPrimType::I64,
                            left: env.add(int),
                            right: env.add(elem_size),
                        },
                        ty: TCType::new(TCTypeBase::I64),
                    }
                } else {
                    let int = TCExpr {
                        kind: TCExprKind::Conv {
                            from: int_prim,
                            to: TCPrimType::U64,
                            expr: env.add(int),
                        },
                        ty: TCType::new(TCTypeBase::U64),
                        loc: int.loc,
                    };

                    let elem_size = TCExpr {
                        loc: l.loc,
                        kind: TCExprKind::U64Lit(stride as u64),
                        ty: TCType::new(TCTypeBase::U64),
                    };

                    TCExpr {
                        loc: int.loc,
                        kind: TCExprKind::BinOp {
                            op: BinOp::Mul,
                            op_type: TCPrimType::U64,
                            left: env.add(int),
                            right: env.add(elem_size),
                        },
                        ty: TCType::new(TCTypeBase::U64),
                    }
                };

                return Ok(TCExpr {
                    kind: TCExprKind::BinOp {
                        op: BinOp::Add,
                        op_type: TCPrimType::U64,
                        left: env.add(ptr),
                        right: env.add(int),
                    },
                    loc,
                    ty: ptr.ty,
                });
            }
            BinOp::Sub => {
                let (ptr, int) = if r.ty.is_integer() {
                    (l, r)
                } else if l.ty.is_integer() {
                    return Err(error!(
                        "can't subtract a pointer from an integer",
                        l.loc, "integer here", r.loc, "pointer here"
                    ));
                } else {
                    // pointer subtraction
                    let s = env.symbols();
                    let or_else = |e: TCExpr| move || ptr_to_incomplete_type(s, e.ty, e.loc);
                    let l_stride = l.ty.pointer_stride().ok_or_else(or_else(l))?;
                    let r_stride = r.ty.pointer_stride().ok_or_else(or_else(r))?;
                    if l_stride != r_stride {
                        let (l_td, r_td) = (l.ty.display(s), r.ty.display(s));
                        return Err(error!(
                            "pointer subtraction performed on pointers to types of different sizes",
                            l.loc,
                            format!("this has the type `{}` (size={})", l_td, l_stride),
                            r.loc,
                            format!("this has the type `{}` (size={})", r_td, r_stride)
                        ));
                    }

                    let (left, right) = (env.add(l), env.add(r));
                    let (op, op_type) = (BinOp::Sub, TCPrimType::U64);
                    let ty = TCType::new(TCTypeBase::U64);

                    #[rustfmt::skip]
                    let sub = TCExpr {
                        kind: TCExprKind::BinOp { op, op_type, left, right },
                        ty,
                        loc,
                    };

                    let divisor = TCExpr {
                        kind: TCExprKind::U64Lit(l_stride as u64),
                        ty,
                        loc,
                    };

                    let (left, right) = (env.add(sub), env.add(divisor));

                    #[rustfmt::skip]
                    let result = TCExpr {
                        kind: TCExprKind::BinOp { op: BinOp::Div, op_type, left, right },
                        ty,
                        loc,
                    };

                    return Ok(result);
                };

                let stride = ptr.ty.pointer_stride();
                if stride == n32::NULL {}

                let stride: u32 = stride.into();

                let ptr_prim = ptr.ty.to_prim_type().ok_or_else(ptype_err(ptr.loc))?;
                let int_prim = int.ty.to_prim_type().ok_or_else(ptype_err(int.loc))?;

                let (int, op_type) = if int_prim.signed() {
                    let int = TCExpr {
                        kind: TCExprKind::Conv {
                            from: int_prim,
                            to: TCPrimType::I64,
                            expr: env.add(int),
                        },
                        ty: TCType::new(TCTypeBase::I64),
                        loc: int.loc,
                    };

                    let elem_size = TCExpr {
                        loc: l.loc,
                        kind: TCExprKind::I64Lit(stride as i64),
                        ty: TCType::new(TCTypeBase::I64),
                    };

                    let int = TCExpr {
                        loc: int.loc,
                        kind: TCExprKind::BinOp {
                            op: BinOp::Mul,
                            op_type: TCPrimType::I64,
                            left: env.add(int),
                            right: env.add(elem_size),
                        },
                        ty: TCType::new(TCTypeBase::I64),
                    };

                    (int, TCPrimType::I64)
                } else {
                    let int = TCExpr {
                        kind: TCExprKind::Conv {
                            from: int_prim,
                            to: TCPrimType::U64,
                            expr: env.add(int),
                        },
                        ty: TCType::new(TCTypeBase::U64),
                        loc: int.loc,
                    };

                    let elem_size = TCExpr {
                        loc: l.loc,
                        kind: TCExprKind::U64Lit(stride as u64),
                        ty: TCType::new(TCTypeBase::U64),
                    };

                    let int = TCExpr {
                        loc: int.loc,
                        kind: TCExprKind::BinOp {
                            op: BinOp::Mul,
                            op_type: TCPrimType::U64,
                            left: env.add(int),
                            right: env.add(elem_size),
                        },
                        ty: TCType::new(TCTypeBase::U64),
                    };

                    (int, TCPrimType::U64)
                };

                return Ok(TCExpr {
                    kind: TCExprKind::BinOp {
                        op: BinOp::Sub,
                        op_type,
                        left: env.add(ptr),
                        right: env.add(int),
                    },
                    loc,
                    ty: ptr.ty,
                });
            }
            BinOp::Lt | BinOp::Gt | BinOp::Leq | BinOp::Geq => {}
            BinOp::Eq | BinOp::Neq => {}
            _ => return Err(invalid_bin_op(&l, &r)),
        }
    }

    if op == BinOp::LShift || op == BinOp::RShift {
        if !l.ty.is_integer() || !r.ty.is_integer() {
            return Err(invalid_bin_op(&l, &r));
        }

        let op_type = l.ty.to_prim_type().unwrap();
        let or_else = || bitshift_conversion_error(env.symbols(), &r);
        let r = env
            .assign_convert(TCType::new(TCTypeBase::I8), r, loc)
            .ok_or_else(or_else)?;

        let (left, right, ty) = (env.add(l), env.add(r), l.ty);

        #[rustfmt::skip]
        return Ok(TCExpr {
            kind: TCExprKind::BinOp { op, op_type, left, right },
            loc,
            ty,
        });
    }

    let (left, right, op_type) = prim_unify(env, l, r)?;

    let ty = match op {
        BinOp::Lt | BinOp::Gt | BinOp::Leq | BinOp::Geq => TCType::new(TCTypeBase::I8),
        BinOp::Eq | BinOp::Neq => TCType::new(TCTypeBase::I8),
        BinOp::BoolAnd | BinOp::BoolOr => TCType::new(TCTypeBase::I8),
        _ => left.ty,
    };

    let (left, right) = (env.add(left), env.add(right));

    #[rustfmt::skip]
    return Ok(TCExpr {
        kind: TCExprKind::BinOp { op, op_type, left, right },
        loc, ty,
    });
}

pub fn prim_unify(
    env: &TypeEnv,
    l: TCExpr,
    r: TCExpr,
) -> Result<(TCExpr, TCExpr, TCPrimType), Error> {
    use core::cmp::Ordering;

    let ptype_err =
        |loc: CodeLoc| move || error!("couldn't do operation on value", loc, "value found here");

    let l_prim = l.ty.to_prim_type().ok_or_else(ptype_err(l.loc))?;
    if l.ty == r.ty {
        return Ok((l, r, l_prim));
    }
    let r_prim = r.ty.to_prim_type().ok_or_else(ptype_err(r.loc))?;

    if l.ty.is_pointer() && r.ty.is_pointer() {
        // void*
        let l = TCExpr {
            kind: TCExprKind::Conv {
                from: l_prim,
                to: TCPrimType::Pointer {
                    stride: 1u32.into(),
                },
                expr: env.add(l),
            },
            ty: TCType::new_ptr(TCTypeBase::Void),
            loc: l.loc,
        };

        let r = TCExpr {
            kind: TCExprKind::Conv {
                from: r_prim,
                to: TCPrimType::Pointer {
                    stride: 1u32.into(),
                },
                expr: env.add(r),
            },
            ty: TCType::new_ptr(TCTypeBase::Void),
            loc: r.loc,
        };

        return Ok((l, r, l_prim));
    }

    if l_prim == r_prim {
        return Ok((l, r, l_prim));
    }

    let use_l_type = match l_prim.size().cmp(&r_prim.size()) {
        Ordering::Less => false,
        Ordering::Greater => true,
        Ordering::Equal => !l_prim.signed() && !r.ty.is_pointer(),
    };

    if use_l_type {
        let r = TCExpr {
            kind: TCExprKind::Conv {
                from: r_prim,
                to: l_prim,
                expr: env.add(r),
            },
            ty: l.ty,
            loc: r.loc,
        };

        return Ok((l, r, l_prim));
    } else {
        let l = TCExpr {
            kind: TCExprKind::Conv {
                from: l_prim,
                to: r_prim,
                expr: env.add(l),
            },
            ty: r.ty,
            loc: r.loc,
        };

        return Ok((l, r, r_prim));
    }
}

pub fn get_fields<'a>(env: &'a TypeEnv, ty: TCType) -> Option<&'a [TCStructField]> {
    let (is_struct, id) = ty.get_id_strict()?;

    if is_struct {
        env.get_struct_fields(id)
    } else {
        env.get_union_fields(id)
    }
}

pub fn check_field_access(
    env: &TypeEnv,
    ty: TCType,
    field: u32,
    loc: CodeLoc,
) -> Result<TCStructField, Error> {
    let or_else = || not_a_struct(env.symbols(), ty, loc);
    let (is_struct, id) = ty.get_id_strict().ok_or_else(or_else)?;

    let or_else = || access_incomplete_struct_type(ty, loc);
    let member_info = if is_struct {
        env.get_struct_fields(id).ok_or_else(or_else)?
    } else {
        env.get_union_fields(id).ok_or_else(or_else)?
    };

    let res = member_info.iter().find(|m| m.name == field);
    let or_else = || field_doesnt_exist(ty, loc);
    return Ok(*res.ok_or_else(or_else)?);
}

pub fn check_assign_target(env: &mut TypeEnv, expr: &Expr) -> Result<TCAssignTarget, Error> {
    match &expr.kind {
        ExprKind::Ident(id) => return env.assign_ident(*id, expr.loc),

        ExprKind::Member { base, member } => {
            let mut base = check_assign_target(&mut *env, base)?;
            let field = check_field_access(&mut *env, base.ty, *member, base.loc)?;

            base.ty = field.ty;
            base.offset += field.offset;
            base.loc = expr.loc;

            return Ok(base);
        }
        ExprKind::PtrMember { base, member } => {
            let base = check_expr(&mut *env, base)?;
            let or_else = || not_a_struct_pointer(env.symbols(), base.ty, base.loc);
            let base_ty = base.ty.deref().ok_or_else(or_else)?;
            let field = check_field_access(&mut *env, base_ty, *member, expr.loc)?;

            return Ok(TCAssignTarget {
                kind: TCAssignTargetKind::Ptr(env.add(base)),
                offset: field.offset,
                ty: field.ty,
                defn_loc: base.loc,
                loc: expr.loc,
            });
        }

        ExprKind::UnaryOp(UnaryOp::Deref, ptr) => {
            let ptr = check_expr(&mut *env, ptr)?;
            if let TCExprKind::Ref(mut target) = ptr.kind {
                target.loc = expr.loc;
                target.defn_loc = ptr.loc;
                return Ok(target);
            }

            let or_else = || error!("cannot dereference type", ptr.loc, "value found here");
            let ty = ptr.ty.deref().ok_or_else(or_else)?;

            return Ok(TCAssignTarget {
                kind: TCAssignTargetKind::Ptr(env.add(ptr)),
                loc: expr.loc,
                defn_loc: ptr.loc,
                ty,
                offset: 0,
            });
        }
        ExprKind::BinOp(BinOp::Index, ptr, offset) => {
            let sum = check_bin_op(&mut *env, BinOp::Add, ptr, offset, expr.loc)?;
            let or_else = || error!("cannot dereference value", ptr.loc, "value found here");
            let ty = sum.ty.deref().ok_or_else(or_else)?;

            return Ok(TCAssignTarget {
                kind: TCAssignTargetKind::Ptr(env.add(sum)),
                loc: expr.loc,
                defn_loc: ptr.loc,
                ty,
                offset: 0,
            });
        }

        _ => {
            return Err(error!(
                "expression is not assignable",
                expr.loc, "expression found here"
            ))
        }
    }
}

pub fn check_un_op(
    env: &mut TypeEnv,
    op: UnaryOp,
    obj: &Expr,
    loc: CodeLoc,
) -> Result<TCExpr, Error> {
    let ptype_err =
        |loc: CodeLoc| move || error!("couldn't do operation on value", loc, "value found here");

    match op {
        UnaryOp::Ref => {
            let target = check_assign_target(env, obj)?;
            let ty = TCType::new_ptr(TCTypeBase::InternalTypedef(env.add(target.ty)));

            return Ok(TCExpr {
                kind: TCExprKind::Ref(target),
                ty,
                loc: obj.loc,
            });
        }
        UnaryOp::Deref => {
            let ptr = check_expr(&mut *env, obj)?;
            let or_else = || error!("cannot dereference type", ptr.loc, "value found here");
            let ty = ptr.ty.deref().ok_or_else(or_else)?;
            return Ok(TCExpr {
                kind: TCExprKind::Deref(env.add(ptr)),
                ty,
                loc,
            });
        }

        UnaryOp::PostDecr => {
            let value = check_assign_target(env, obj)?;
            let decr_ty = value.ty.to_prim_type().ok_or_else(ptype_err(value.loc))?;

            if let TCPrimType::Pointer { stride: n32::NULL } = decr_ty {
                return Err(ptr_to_incomplete_type(env.symbols(), value.ty, loc));
            }

            return Ok(TCExpr {
                kind: TCExprKind::PostDecr { decr_ty, value },
                ty: value.ty,
                loc,
            });
        }
        UnaryOp::PostIncr => {
            let value = check_assign_target(env, obj)?;
            let incr_ty = value.ty.to_prim_type().ok_or_else(ptype_err(value.loc))?;

            if let TCPrimType::Pointer { stride: n32::NULL } = incr_ty {
                return Err(ptr_to_incomplete_type(env.symbols(), value.ty, loc));
            }

            return Ok(TCExpr {
                kind: TCExprKind::PostIncr { incr_ty, value },
                ty: value.ty,
                loc,
            });
        }

        UnaryOp::PreDecr => {
            let target = check_assign_target(env, obj)?;
            let or_else = || bin_assign_op_non_primitive(target.ty, target.loc);
            let op_type = target.ty.to_prim_type().ok_or_else(or_else)?;

            let (kind, ty) = match op_type {
                TCPrimType::I8 => (TCExprKind::I8Lit(1), TCType::new(TCTypeBase::I8)),
                TCPrimType::I32 => (TCExprKind::I32Lit(1), TCType::new(TCTypeBase::I32)),
                TCPrimType::U32 => (TCExprKind::U32Lit(1), TCType::new(TCTypeBase::U32)),
                TCPrimType::U64 => (TCExprKind::U64Lit(1), TCType::new(TCTypeBase::U64)),

                TCPrimType::Pointer { stride } => {
                    let or_else = || ptr_to_incomplete_type(env.symbols(), target.ty, loc);
                    let stride = stride.ok_or_else(or_else)? as u64;
                    (TCExprKind::U64Lit(stride), TCType::new(TCTypeBase::U64))
                }

                x => unimplemented!("predecr for {:?}", x),
            };

            let loc = obj.loc;
            let val = TCExpr { kind, ty, loc };
            let (op, value, ty) = (BinOp::Sub, env.add(val), target.ty);

            #[rustfmt::skip]
            return Ok(TCExpr {
                kind: TCExprKind::MutAssign { target, value, op_type, op },
                ty: target.ty,
                loc: obj.loc,
            });
        }
        UnaryOp::PreIncr => {
            let target = check_assign_target(env, obj)?;
            let or_else = || bin_assign_op_non_primitive(target.ty, target.loc);
            let op_type = target.ty.to_prim_type().ok_or_else(or_else)?;

            let (kind, ty) = match op_type {
                TCPrimType::I8 => (TCExprKind::I8Lit(1), TCType::new(TCTypeBase::I8)),
                TCPrimType::U8 => (TCExprKind::U8Lit(1), TCType::new(TCTypeBase::U8)),
                TCPrimType::I16 => (TCExprKind::I16Lit(1), TCType::new(TCTypeBase::I16)),
                TCPrimType::U16 => (TCExprKind::U16Lit(1), TCType::new(TCTypeBase::U16)),
                TCPrimType::I32 => (TCExprKind::I32Lit(1), TCType::new(TCTypeBase::I32)),
                TCPrimType::U32 => (TCExprKind::U32Lit(1), TCType::new(TCTypeBase::U32)),
                TCPrimType::I64 => (TCExprKind::I64Lit(1), TCType::new(TCTypeBase::I64)),
                TCPrimType::U64 => (TCExprKind::U64Lit(1), TCType::new(TCTypeBase::U64)),

                TCPrimType::Pointer { stride } => {
                    let or_else = || ptr_to_incomplete_type(env.symbols(), target.ty, loc);
                    let stride = stride.ok_or_else(or_else)? as u64;
                    (TCExprKind::U64Lit(stride), TCType::new(TCTypeBase::U64))
                }
                x => unimplemented!("preincr for {:?}", x),
            };

            let loc = obj.loc;
            let val = TCExpr { kind, ty, loc };
            let (op, value, ty) = (BinOp::Add, env.add(val), target.ty);

            #[rustfmt::skip]
            return Ok(TCExpr {
                kind: TCExprKind::MutAssign { target, value, op_type, op },
                ty: target.ty,
                loc: obj.loc,
            });
        }

        UnaryOp::BoolNot => {
            let operand = check_expr(&mut *env, obj)?;
            let op_type_o = operand.ty.to_prim_type();
            let op_type = op_type_o.ok_or_else(ptype_err(operand.loc))?;
            let operand = env.add(operand);

            return Ok(TCExpr {
                kind: TCExprKind::UnaryOp {
                    op: TCUnaryOp::BoolNot,
                    op_type,
                    operand,
                },
                ty: TCType::new(TCTypeBase::I8),
                loc,
            });
        }

        UnaryOp::Neg => {
            let operand = check_expr(&mut *env, obj)?;
            let op_type_o = operand.ty.to_prim_type();
            let op_type = op_type_o.ok_or_else(ptype_err(operand.loc))?;
            let operand = env.add(operand);

            return Ok(TCExpr {
                kind: TCExprKind::UnaryOp {
                    op: TCUnaryOp::Neg,
                    op_type,
                    operand,
                },
                ty: operand.ty,
                loc,
            });
        }

        UnaryOp::BitNot => {
            let operand = check_expr(env, obj)?;
            let op_type_o = operand.ty.to_prim_type();
            let op_type = op_type_o.ok_or_else(ptype_err(operand.loc))?;
            let operand = env.add(operand);

            return Ok(TCExpr {
                kind: TCExprKind::UnaryOp {
                    op: TCUnaryOp::BitNot,
                    op_type,
                    operand,
                },
                ty: operand.ty,
                loc,
            });
        }
    }
}

pub fn bin_assign_op_non_primitive(ty: TCType, loc: CodeLoc) -> Error {
    return error!(
        "this can only be done on primitive types",
        loc, "the type of this assignment expression is not primitive"
    );
}

pub fn not_a_struct_pointer(syms: &Symbols, ty: TCType, loc: CodeLoc) -> Error {
    return error!(
        "accessed expression using arrow that was not a struct/union pointer",
        loc,
        format!("access happened here (type is {})", ty.display(syms))
    );
}

pub fn not_a_struct(syms: &Symbols, ty: TCType, loc: CodeLoc) -> Error {
    return error!(
        "tried to access field of non-struct/union type",
        loc,
        format!("access happened here (type is {})", ty.display(syms))
    );
}

pub fn access_incomplete_struct_type(ty: TCType, loc: CodeLoc) -> Error {
    return error!(
        "tried to access field of incomplete struct type",
        loc, "access happened here"
    );
}

pub fn field_doesnt_exist(ty: TCType, loc: CodeLoc) -> Error {
    return error!(
        "tried to access field that doesn't exist",
        loc, "access happened here"
    );
}

pub fn variable_incomplete_type(ty: TCType, loc: CodeLoc) -> Error {
    return error!("declared variable of incomplete type", loc, "declared here");
}

pub fn invalid_bin_op(l: &TCExpr, r: &TCExpr) -> Error {
    return error!(
        "invalid operands to binary expression",
        l.loc, "left hand side", r.loc, "right hand side"
    );
}

pub fn invalid_bin_op_assign(l: &TCAssignTarget, r: &TCExpr) -> Error {
    return error!(
        "invalid operands to binary expression",
        l.loc, "left hand side", r.loc, "right hand side"
    );
}

pub fn param_conversion_error(ty: TCType, expr: &TCExpr) -> Error {
    return error!(
        "couldn't convert value to parameter type",
        expr.loc, "value found here"
    );
}

pub fn conversion_error(ty: TCType, loc: CodeLoc, expr: &TCExpr) -> Error {
    return error!(
        "couldn't convert value to target type",
        loc, "target type found here", expr.loc, "value found here"
    );
}

pub fn condition_non_primitive(ty: TCType, loc: CodeLoc) -> Error {
    return error!(
        "using condition of non-primitive type",
        loc, "condition found here"
    );
}

pub fn ptr_to_incomplete_type(syms: &Symbols, ty: TCType, loc: CodeLoc) -> Error {
    return error!(
        "cannot perform arithmetic on pointer type",
        loc, "pointer found here"
    );
}

pub fn bitshift_conversion_error(syms: &Symbols, expr: &TCExpr) -> Error {
    return error!(
        "couldn't use value as bitshift size",
        expr.loc, "value found here"
    );
}

pub fn neg_arr_size<T>(loc: CodeLoc) -> impl Fn(T) -> Error {
    return move |t: T| error!("array must have positive size", loc, "size found here");
}
