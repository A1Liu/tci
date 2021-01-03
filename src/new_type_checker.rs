use crate::buckets::*;
use crate::filedb::*;
use crate::new_ast::*;
use crate::new_tc_ast::*;
use crate::new_tc_structs::*;
use crate::util::*;

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

lazy_static! {
    pub static ref CORRECT_TYPES: HashMap<TypeDeclSpec, TCTypeBase> = {
        let mut map: HashMap<TypeDeclSpec, TCTypeBase> = HashMap::new();

        gen_type_decl_spec!(map, I32, int);
        gen_type_decl_spec!(map, I64, long);
        gen_type_decl_spec!(map, I8, char);
        gen_type_decl_spec!(map, Void, void);
        gen_type_decl_spec!(map, I8, signed char);
        gen_type_decl_spec!(map, U32, unsigned);
        gen_type_decl_spec!(map, U8,unsigned char);

        gen_type_decl_spec!(map, I64,long int);
        gen_type_decl_spec!(map, I64,long long int);
        gen_type_decl_spec!(map, I64,long long);

        gen_type_decl_spec!(map, U32, unsigned int);

        gen_type_decl_spec!(map, U64, unsigned long);
        gen_type_decl_spec!(map, U64, unsigned long int);
        gen_type_decl_spec!(map, U64, unsigned long long);
        gen_type_decl_spec!(map, U64, unsigned long long int);

        map
    };
}

pub fn parse_decl_specs(
    locals: &TypeEnv,
    decl_specs: &[DeclarationSpecifier],
) -> Result<(StorageClass, TCTypeBase), Error> {
    let mut sc = StorageClass::Default;
    let mut ds = TypeDeclSpec::new();

    for decl_spec in decl_specs {
        // use crate::new_ast::TypeQualifier as TyQual;
        use crate::new_ast::TypeSpecifier as TySpec;
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

            TypeQualifier(qual) => {}
            Inline => {}
            Noreturn => {}

            TypeSpecifier(TySpec::Ident(id)) => {
                let ty = locals.check_typedef(id, decl_spec.loc)?;
                return Ok((sc, ty));
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

            _ => unreachable!(),
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
    locals: &TypeEnv,
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
            out.push(TCParamDeclaration {
                ty,
                ident: id.into(),
                loc: params_decl.parameters[idx].loc,
            });
        }
    }

    return Ok(TCFunctionDeclarator {
        is_static: let_expr!(StorageClass::Static = sc),
        return_type: rtype.to_ref(locals),
        ident: decl.ident,
        params: Some(TCParamsDeclarator {
            params: locals.add_array(out),
            varargs: params_decl.varargs,
        }),
    });
}

pub fn check_decl(
    locals: &TypeEnv,
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
    locals: &TypeEnv,
    params: &[ParameterDeclaration],
) -> Result<Vec<(TCType, n32)>, Error> {
    let param = params[0];
    let (sc, param_base) = parse_decl_specs(locals, param.specifiers)?;
    let (mut param_type, id) = if let Some(decl) = param.declarator {
        let (tc_type, id) = check_decl(locals, param_base, &decl)?;
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
    let param_type = param_type.to_ref(locals);

    let mut out = Vec::new();
    out.push((param_type, id));

    for param in &params[1..] {
        let (sc, base) = parse_decl_specs(locals, param.specifiers)?;
        let (mut param_type, id) = if let Some(decl) = param.declarator {
            let (tc_type, id) = check_decl(locals, param_base, &decl)?;
            (tc_type, id)
        } else {
            (TCTypeOwned::new(param_base), n32::NULL)
        };

        debug_assert!(let_expr!(StorageClass::Default = sc));

        if let TCTypeBase::Void = param_type.base {
            if param_type.mods.len() == 0 {
                return Err(error!(
                    "cannot have a parameter of type void",
                    param.loc, "parameter found here"
                ));
            }
        }

        param_type.canonicalize_param();
        let param_type = param_type.to_ref(locals);
        out.push((param_type, id));
    }

    return Ok(out);
}

pub fn check_decl_rec(
    locals: &TypeEnv,
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
                        let (expr, loc) = if let TCExprKind::I32Literal(i) = expr.kind {
                            (i, expr.loc)
                        } else {
                            unreachable!()
                        };

                        if expr < 0 {
                            return Err(error!(
                                "array must have positive size",
                                loc, "size found here"
                            ));
                        }

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

pub fn check_tree(files: &FileDb, tree: &[GlobalStatement]) -> Result<TranslationUnit, Error> {
    let mut globals = TypeEnv::global(files);

    for decl in tree {
        match decl.kind {
            GlobalStatementKind::Declaration(decl) => {
                check_declaration(&globals, decl)?;
            }
            GlobalStatementKind::FunctionDefinition(func) => {
                // check_function_defn(&globals, func)?;
            }
            GlobalStatementKind::Pragma(pragma) => {
                if pragma == "tci enable_builtins" {
                    globals.builtins_enabled = true;
                }
            }
        }
    }

    return Ok(globals.tu());
}

pub fn assign_convert(alloc: impl Allocator<'static>, ty: TCType, expr: TCExpr) -> TCExpr {
    if TCType::ty_eq(&ty, &expr.ty) {
        return expr;
    }

    let to = ty.to_prim_type();
    let from = expr.ty.to_prim_type();
    let expr = alloc.add(expr);

    return TCExpr {
        kind: TCExprKind::Conv { from, to, expr },
        ty,
        loc: expr.loc,
    };
}

pub fn check_declaration(
    locals: &TypeEnv,
    declaration: Declaration,
) -> Result<DeclarationResult, Error> {
    let (sc, base) = parse_decl_specs(locals, declaration.specifiers)?;

    if let StorageClass::Typedef = sc {
        debug_assert!(declaration.declarators.len() == 1);
        let init_declarator = &declaration.declarators[0];
        debug_assert!(init_declarator.initializer.is_none());

        let (ty, id) = check_decl(locals, base, &init_declarator.declarator)?;
        let (ty, ident) = (ty.to_ref(locals), id.into());
        return Ok(DeclarationResult::Typedef { ty, ident });
    }

    if let StorageClass::Extern = sc {
        let mut decls = Vec::new();

        for decl in declaration.declarators {
            if let Some(init) = decl.initializer {
                return Err(error!(
                    "initializer not valid in an extern declaration",
                    init.loc, "initializer found here"
                ));
            }

            let (ty, id) = check_decl(locals, base, &decl.declarator)?;
            decls.push((ty.to_ref(locals), id.into()));
        }

        return Ok(DeclarationResult::Extern(decls));
    }

    let mut decls = Vec::new();
    for decl in declaration.declarators {
        let (ty, id) = check_decl(locals, base, &decl.declarator)?;
        let (ty, ident) = (ty.to_ref(locals), id.into());
        let expr = if let Some(init) = decl.initializer {
            match init.kind {
                InitializerKind::Expr(expr) => {
                    assign_convert(locals, ty, check_expr(locals, expr)?)
                }
                InitializerKind::List(exprs) => {
                    panic!("don't support initializer lists yet")
                }
            }
        } else {
            TCExpr {
                kind: TCExprKind::Uninit,
                ty,
                loc: NO_FILE,
            }
        };

        decls.push(TCDecl { ty, ident, expr });
    }

    if let StorageClass::Static = sc {
        return Ok(DeclarationResult::Static(decls));
    } else {
        return Ok(DeclarationResult::Default(decls));
    }
}

pub fn eval_expr(expr: TCExpr) -> Result<TCExpr, Error> {
    // TODO cmon man
    if let TCExprKind::I32Literal(i) = expr.kind {
        return Ok(expr);
    } else {
        return Err(error!(
            "cannot evaluate constant expression",
            expr.loc, "expression found here"
        ));
    }
}

pub fn check_expr(env: &TypeEnv, expr: &Expr) -> Result<TCExpr, Error> {
    match expr.kind {
        ExprKind::IntLiteral(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::I32Literal(val),
                ty: TCType::new(TCTypeBase::I32),
                loc: expr.loc,
            });
        }
        ExprKind::StringLiteral(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::StringLiteral(env.add_str(val)),
                ty: TCType::new_ptr(TCTypeBase::I8),
                loc: expr.loc,
            });
        }
        ExprKind::CharLiteral(c) => {
            return Ok(TCExpr {
                kind: TCExprKind::I8Literal(c),
                ty: TCType::new(TCTypeBase::I8),
                loc: expr.loc,
            });
        }
        ExprKind::Ident(id) => {
            return env.ident(id, expr.loc);
        }

        ExprKind::ParenList(exprs) => {
            let mut tc_exprs = Vec::new();
            for expr in exprs {
                tc_exprs.push(check_expr(env, expr)?);
            }

            if tc_exprs.len() == 0 {
                // happens in for loops
                return Ok(TCExpr {
                    ty: TCType::new(TCTypeBase::I8),
                    kind: TCExprKind::I8Literal(0),
                    loc: expr.loc,
                });
            }

            return Ok(TCExpr {
                ty: tc_exprs[tc_exprs.len() - 1].ty,
                kind: TCExprKind::ParenList(env.add_array(tc_exprs)),
                loc: expr.loc,
            });
        }

        ExprKind::BinOp(op, l, r) => return check_bin_op(env, op, l, r),

        ExprKind::UnaryOp(op, operand) => {
            unimplemented!()
            // let operand = check_expr(env, local_env, operand)?;
            // let prim_operand = env.to_prim_type(operand.expr_type, operand.loc)?;

            // let key = (op, prim_operand.discriminant());
            // let un_op = match OVERLOADS.unary_op.get(&key) {
            //     Some(un_op) => *un_op,
            //     None => {
            //         return Err(error!(
            //             "invalid operation to unary operand",
            //             operand.loc,
            //             format!(
            //                 "operand found here with type {}",
            //                 operand.expr_type.display(env.files)
            //             )
            //         ))
            //     }
            // };

            // return Ok(un_op(env.buckets, operand, expr.loc));
        }

        // ExprKind::Member { base, member } => {
        //     let base = check_expr(env, local_env, base)?;
        //     let member_info = env.check_struct_member(base.expr_type, base.loc, member)?;

        //     return Ok(TCExpr {
        //         expr_type: member_info.decl_type,
        //         loc: expr.loc,
        //         kind: TCExprKind::Member {
        //             base: env.buckets.add(base),
        //             offset: member_info.offset,
        //         },
        //     });
        // }
        // ExprKind::PtrMember { base, member } => {
        //     let base = check_expr(env, base)?;

        //     let deref_type = base.expr_type.deref(base.loc)?;
        //     if deref_type.pointer_count != 0 {
        //         return Err(ptr_member_of_poly_pointer(base.loc, &deref_type));
        //     }

        //     let member_info = env.check_struct_member(deref_type, base.loc, member)?;

        //     return Ok(TCExpr {
        //         expr_type: member_info.decl_type,
        //         loc: expr.loc,
        //         kind: TCExprKind::PtrMember {
        //             base: env.add(base),
        //             offset: member_info.offset,
        //         },
        //     });
        // }
        //ExprKind::Call { function, params } => {
        //    let func_id = if let ExprKind::Ident(id) = function.kind {
        //        id
        //    } else {
        //        return Err(error!(
        //            "calling an expression that isn't a function",
        //            function.loc, "called here"
        //        ));
        //    };

        //    let func_type = if let Some(func_type) = env.func_types.get(&func_id) {
        //        func_type
        //    } else {
        //        if env.types.builtins_enabled.map(|idx| idx < env.decl_idx) == Some(true) {
        //            if let Some(trans) = BUILTINS.get(env.files.symbol_to_str(func_id)) {
        //                let (builtin, expr_type) = trans(env, local_env, expr.loc, params)?;
        //                return Ok(TCExpr {
        //                    kind: TCExprKind::Builtin(builtin),
        //                    loc: expr.loc,
        //                    expr_type,
        //                });
        //            }
        //        }

        //        return Err(error!("function doesn't exist", expr.loc, "called here"));
        //    };

        //    if func_type.decl_idx > env.decl_idx {
        //        return Err(error!(
        //            "function hasn't been declared yet (declaration order matters in C)",
        //            expr.loc, "function called here", func_type.loc, "function declared here"
        //        ));
        //    }

        //    if params.len() < func_type.params.len()
        //        || (params.len() > func_type.params.len() && !func_type.varargs)
        //    {
        //        return Err(error!(
        //            "function call has wrong number of parameters",
        //            expr.loc, "function called here", func_type.loc, "function declared here"
        //        ));
        //    }

        //    let mut tparams = Vec::new();
        //    for (idx, param) in params.iter().enumerate() {
        //        let mut expr = check_expr(env, local_env, param)?;
        //        if idx < func_type.params.len() {
        //            let param_type = &func_type.params[idx];
        //            expr = env.param_convert(&param_type.0, param_type.1, expr)?;
        //        }

        //        tparams.push(expr);
        //    }

        //    return Ok(TCExpr {
        //        kind: TCExprKind::Call {
        //            func: func_id,
        //            params: env.buckets.add_array(tparams),
        //            named_count: params.len() as u32,
        //        },
        //        expr_type: func_type.return_type,
        //        loc: expr.loc,
        //    });
        //}
        x => panic!("{:?} is unimplemented", x),
    }
}

pub fn check_bin_op(env: &TypeEnv, op: BinOp, l: &Expr, r: &Expr) -> Result<TCExpr, Error> {
    let l = check_expr(env, l)?;
    let r = check_expr(env, r)?;

    if l.ty.is_pointer() {
        let stride = l.ty.pointer_stride();
        if stride == n32::NULL {
            return Err(error!(
                "cannot perform arithmetic on pointer type",
                l.loc, "pointer found here"
            ));
        }
        let stride: u32 = stride.into();

        // allowed operations are addition w/ integer, subtraction w/ integer/pointer, index with
        // integer
        match op {
            BinOp::Add => {
                if !r.ty.is_integer() {
                    return Err(error!(
                        "invalid operands to binary expression",
                        l.loc, "left hand side", r.loc, "right hand side"
                    ));
                }

                let l_prim = l.ty.to_prim_type();
                let r_prim = r.ty.to_prim_type();

                let r = if r_prim.signed() {
                    let r = TCExpr {
                        kind: TCExprKind::Conv {
                            from: r_prim,
                            to: TCPrimType::I64,
                            expr: env.add(r),
                        },
                        ty: TCType::new(TCTypeBase::I64),
                        loc: r.loc,
                    };

                    let elem_size = TCExpr {
                        loc: l.loc,
                        kind: TCExprKind::I64Literal(stride as i64),
                        ty: TCType::new(TCTypeBase::I64),
                    };

                    let r = TCExpr {
                        loc: r.loc,
                        kind: TCExprKind::BinOp {
                            op: BinOp::Mul,
                            op_type: TCPrimType::I64,
                            left: env.add(r),
                            right: env.add(elem_size),
                        },
                        ty: TCType::new(TCTypeBase::I64),
                    };

                    TCExpr {
                        kind: TCExprKind::Conv {
                            from: l_prim,
                            to: TCPrimType::I64,
                            expr: env.add(l),
                        },
                        ty: TCType::new(TCTypeBase::I64),
                        loc: r.loc,
                    }
                } else {
                    let r = TCExpr {
                        kind: TCExprKind::Conv {
                            from: r_prim,
                            to: TCPrimType::U64,
                            expr: env.add(r),
                        },
                        ty: TCType::new(TCTypeBase::U64),
                        loc: r.loc,
                    };

                    let elem_size = TCExpr {
                        loc: l.loc,
                        kind: TCExprKind::U64Literal(stride as u64),
                        ty: TCType::new(TCTypeBase::U64),
                    };

                    TCExpr {
                        loc: r.loc,
                        kind: TCExprKind::BinOp {
                            op: BinOp::Mul,
                            op_type: TCPrimType::U64,
                            left: env.add(r),
                            right: env.add(elem_size),
                        },
                        ty: TCType::new(TCTypeBase::U64),
                    }
                };

                return Ok(TCExpr {
                    loc: l_from(l.loc, r.loc),
                    kind: TCExprKind::BinOp {
                        op: BinOp::Add,
                        op_type: TCPrimType::U64,
                        left: env.add(l),
                        right: env.add(r),
                    },
                    ty: l.ty,
                });
            }
            BinOp::Sub => {
                unimplemented!()
            }
            BinOp::Index => {
                unimplemented!()
            }
            _ => {
                return Err(error!(
                    "invalid operands to binary expression",
                    l.loc, "left operand", r.loc, "right operand"
                ))
            }
        }
    }

    if r.ty.is_pointer() { // valid operations are addition with integer
    }

    let (left, right, op_type) = prim_unify(env, l, r)?;
    let ty = match op {
        BinOp::Lt | BinOp::Gt | BinOp::Leq | BinOp::Geq => TCType::new(TCTypeBase::I8),
        BinOp::Eq | BinOp::Neq => TCType::new(TCTypeBase::I8),
        BinOp::Index => {
            return Err(error!(
                "cannot index non-pointer type",
                l.loc, "indexing here"
            ))
        }
        _ => left.ty,
    };

    let (left, right) = (env.add(left), env.add(right));
    let loc = l_from(left.loc, right.loc);

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

    let l_prim = l.ty.to_prim_type();
    if l.ty == r.ty {
        return Ok((l, r, l_prim));
    }
    let r_prim = l.ty.to_prim_type();

    if l.ty.is_pointer() && r.ty.is_pointer() {
        // void*
        let l = TCExpr {
            kind: TCExprKind::Conv {
                from: l_prim,
                to: TCPrimType::Pointer { stride: 1.into() },
                expr: env.add(l),
            },
            ty: TCType::new_ptr(TCTypeBase::Void),
            loc: l.loc,
        };

        let r = TCExpr {
            kind: TCExprKind::Conv {
                from: r_prim,
                to: TCPrimType::Pointer { stride: 1.into() },
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
