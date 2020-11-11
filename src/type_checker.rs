use crate::ast::*;
use crate::buckets::*;
use crate::filedb::*;
use crate::util::*;
use std::collections::{HashMap, HashSet};

type BinOpTransform = for<'b> fn(BucketListRef<'b>, TCExpr<'b>, TCExpr<'b>) -> TCExpr<'b>;
type Transform = for<'b> fn(BucketListRef<'b>, TCExpr<'b>) -> TCExpr<'b>;

// Implicit Transforms

pub type BinOpOverloads = HashMap<(BinOp, TCShallowType, TCShallowType), BinOpTransform>;
pub type BinOpValids = HashSet<(BinOp, TCShallowType)>;
pub type AssignOL = HashMap<(TCTypeKind, TCTypeKind), Transform>;

pub struct Overloads {
    pub bin_op: BinOpOverloads,
    pub left_op: BinOpValids,
    pub right_op: BinOpValids,
    pub expr_to_type: AssignOL,
}

pub static OVERLOADS: LazyStatic<Overloads> = lazy_static!(overloads, Overloads, {
    let mut bin_op: BinOpOverloads = HashMap::new();
    let mut left_op: BinOpValids = HashSet::new();
    let mut right_op: BinOpValids = HashSet::new();
    let mut expr_to_type: AssignOL = HashMap::new();

    macro_rules! add_op_ol {
        ($op:ident, $left:ident, $right:ident, $func:expr) => {{
            bin_op.insert(
                (BinOp::$op, TCShallowType::$left, TCShallowType::$right),
                $func,
            );
            left_op.insert((BinOp::$op, TCShallowType::$left));
            right_op.insert((BinOp::$op, TCShallowType::$right));
        }};
    }

    macro_rules! add_assign_ol {
        ($left:ident, $right:ident, $func:expr) => {{
            expr_to_type.insert((TCTypeKind::$left, TCTypeKind::$right), $func);
        }};
    }

    add_op_ol!(Add, I32, I32, |buckets, l, r| {
        let result_type = TCType {
            kind: TCTypeKind::I32,
            pointer_count: 0,
        };

        return TCExpr {
            loc: l_from(l.loc, r.loc),
            kind: TCExprKind::AddI32(buckets.add(l), buckets.add(r)),
            expr_type: result_type,
        };
    });

    add_op_ol!(Add, I32, Char, |buckets, l, r| {
        let result_type = TCType {
            kind: TCTypeKind::I32,
            pointer_count: 0,
        };

        let r = TCExpr {
            loc: l.loc,
            kind: TCExprKind::SConv8To32(buckets.add(r)),
            expr_type: result_type,
        };

        return TCExpr {
            loc: l_from(l.loc, r.loc),
            kind: TCExprKind::AddI32(buckets.add(l), buckets.add(r)),
            expr_type: result_type,
        };
    });

    add_op_ol!(Add, Char, I32, |buckets, l, r| {
        let result_type = TCType {
            kind: TCTypeKind::I32,
            pointer_count: 0,
        };

        let l = TCExpr {
            loc: l.loc,
            kind: TCExprKind::SConv8To32(buckets.add(l)),
            expr_type: result_type,
        };

        return TCExpr {
            loc: l_from(l.loc, r.loc),
            kind: TCExprKind::AddI32(buckets.add(l), buckets.add(r)),
            expr_type: result_type,
        };
    });

    add_op_ol!(Sub, I32, I32, |buckets, l, r| {
        let result_type = TCType {
            kind: TCTypeKind::I32,
            pointer_count: 0,
        };

        return TCExpr {
            loc: l_from(l.loc, r.loc),
            kind: TCExprKind::SubI32(buckets.add(l), buckets.add(r)),
            expr_type: result_type,
        };
    });

    add_op_ol!(Geq, I32, I32, |buckets, l, r| {
        let result_type = TCType {
            kind: TCTypeKind::Char,
            pointer_count: 0,
        };

        return TCExpr {
            loc: l_from(l.loc, r.loc),
            kind: TCExprKind::GeqI32(buckets.add(l), buckets.add(r)),
            expr_type: result_type,
        };
    });

    add_op_ol!(Neq, I32, I32, |buckets, l, r| {
        let result_type = TCType {
            kind: TCTypeKind::Char,
            pointer_count: 0,
        };

        return TCExpr {
            loc: l_from(l.loc, r.loc),
            kind: TCExprKind::NeqI32(buckets.add(l), buckets.add(r)),
            expr_type: result_type,
        };
    });

    add_op_ol!(Lt, I32, I32, |buckets, l, r| {
        let result_type = TCType {
            kind: TCTypeKind::Char,
            pointer_count: 0,
        };

        return TCExpr {
            loc: l_from(l.loc, r.loc),
            kind: TCExprKind::LtI32(buckets.add(l), buckets.add(r)),
            expr_type: result_type,
        };
    });

    add_op_ol!(Eq, I32, I32, |buckets, l, r| {
        let result_type = TCType {
            kind: TCTypeKind::Char,
            pointer_count: 0,
        };

        return TCExpr {
            loc: l_from(l.loc, r.loc),
            kind: TCExprKind::EqI32(buckets.add(l), buckets.add(r)),
            expr_type: result_type,
        };
    });

    add_assign_ol!(Char, I32, |buckets, e| {
        let result_type = TCType {
            kind: TCTypeKind::I32,
            pointer_count: 0,
        };

        return TCExpr {
            loc: e.loc,
            kind: TCExprKind::SConv8To32(buckets.add(e)),
            expr_type: result_type,
        };
    });

    Overloads {
        bin_op,
        left_op,
        right_op,
        expr_to_type,
    }
});

fn get_overload(env: CheckEnv, op: BinOp, l: &TCExpr, r: &TCExpr) -> Result<BinOpTransform, Error> {
    let key = (op, l.expr_type.to_shallow(), r.expr_type.to_shallow());
    match OVERLOADS.bin_op.get(&key) {
        Some(bin_op) => return Ok(*bin_op),
        None => return Err(invalid_operands_bin_expr(env, op, l, r)),
    }
}

pub fn invalid_operands_bin_expr(env: CheckEnv, op: BinOp, l: &TCExpr, r: &TCExpr) -> Error {
    let lkey = (op, l.expr_type.to_shallow());
    let rkey = (op, r.expr_type.to_shallow());

    if OVERLOADS.left_op.get(&lkey).is_none() {
        return error!(
            "invalid operands to binary expression (left expression is not valid for this operand)",
            l.loc,
            format!("this has type {:?}", l.expr_type.display(env.files)),
            r.loc,
            format!("this has type {:?}", r.expr_type.display(env.files))
        );
    }

    if OVERLOADS.right_op.get(&rkey).is_none() {
        return error!(
            "invalid operands to binary expression (right expression is not valid for this operand)",
            l.loc,
            format!("this has type {:?}", l.expr_type),
            r.loc,
            format!("this has type {:?}", r.expr_type)
            );
    }

    return error!(
        "invalid operands to binary expression",
        l.loc,
        format!("this has type {:?}", l.expr_type),
        r.loc,
        format!("this has type {:?}", r.expr_type)
    );
}

pub struct LocalTypeEnv {
    pub symbols: HashMap<u32, TCVar>,
    pub return_type: TCType,
    pub rtype_loc: CodeLoc,
    pub parent: *const LocalTypeEnv,
    pub decl_idx: i16,
}

impl LocalTypeEnv {
    pub fn new(return_type: TCType, rtype_loc: CodeLoc) -> Self {
        Self {
            symbols: HashMap::new(),
            return_type,
            rtype_loc,
            parent: core::ptr::null(),
            decl_idx: 0,
        }
    }

    pub fn child(&self) -> Self {
        if self.symbols.is_empty() {
            // for the case of chained if-else
            Self {
                symbols: HashMap::new(),
                return_type: self.return_type,
                rtype_loc: self.rtype_loc,
                parent: self.parent,
                decl_idx: self.decl_idx,
            }
        } else {
            Self {
                symbols: HashMap::new(),
                return_type: self.return_type,
                rtype_loc: self.rtype_loc,
                decl_idx: self.decl_idx,
                parent: self,
            }
        }
    }

    pub fn var(&self, id: u32) -> Option<&TCVar> {
        if let Some(var_type) = self.symbols.get(&id) {
            return Some(var_type);
        }

        if self.parent.is_null() {
            return None;
        }

        return unsafe { &*self.parent }.var(id);
    }

    pub fn add_var(&mut self, ident: u32, tc_value: TCVar) -> Result<(), Error> {
        let tc_loc = tc_value.loc;
        if let Some(var_type) = self.symbols.insert(ident, tc_value) {
            return Err(error!(
                "name redefined in scope",
                var_type.loc, "first declaration defined here", tc_loc, "redecaration defined here"
            ));
        }

        return Ok(());
    }

    pub fn add_local(&mut self, ident: u32, decl_type: TCType, loc: CodeLoc) -> Result<(), Error> {
        let tc_var = TCVar {
            decl_type,
            var_offset: self.decl_idx,
            loc,
        };

        self.decl_idx += 1;

        return self.add_var(ident, tc_var);
    }
}

pub struct TypeEnv<'a> {
    pub structs: HashMap<u32, TCStruct<'a>>,
}

impl<'a> TypeEnv<'a> {
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
        }
    }

    pub fn check_type(
        &self,
        decl_idx: u32,
        ast_type: &ASTType,
        pointer_count: u32,
    ) -> Result<TCType, Error> {
        let kind = match &ast_type.kind {
            ASTTypeKind::Int => TCTypeKind::I32,
            ASTTypeKind::Char => TCTypeKind::Char,
            ASTTypeKind::Void => TCTypeKind::Void,
            &ASTTypeKind::Struct { ident } => {
                let sa = self.check_struct_type(ident, decl_idx, pointer_count, ast_type.loc)?;
                TCTypeKind::Struct { ident, sa }
            }
        };

        return Ok(TCType {
            kind,
            pointer_count,
        });
    }

    pub fn deref(&self, tc_type: &TCType, value_loc: CodeLoc) -> Result<TCType, Error> {
        if tc_type.pointer_count == 0 {
            return Err(dereference_of_non_pointer(value_loc, tc_type));
        }

        if let TCTypeKind::Struct {
            sa: TC_UNKNOWN_SA, ..
        } = tc_type.kind
        {
            return Err(error!(
                "cannot dereference pointer to struct of unknown size",
                value_loc,
                format!("value has type {:?}, which cannot be dereferenced", tc_type)
            ));
        }

        let mut other = *tc_type;
        other.pointer_count -= 1;
        return Ok(other);
    }

    // TODO make this work with implicit type conversions
    pub fn implicit_convert<'b>(
        &self,
        buckets: BucketListRef<'b>,
        files: &FileDb,
        assign_type: &TCType,
        assign_loc: CodeLoc,
        assign_loc_is_defn: bool,
        expr: TCExpr<'b>,
    ) -> Result<TCExpr<'b>, Error> {
        if let TCTypeKind::Uninit { .. } = expr.expr_type.kind {
            return Ok(TCExpr {
                kind: TCExprKind::Uninit,
                expr_type: *assign_type,
                loc: expr.loc,
            });
        }

        if assign_type == &expr.expr_type {
            return Ok(expr);
        } else if let Some(converter) = OVERLOADS
            .expr_to_type
            .get(&(expr.expr_type.kind, assign_type.kind))
        {
            return Ok(converter(buckets, expr));
        } else {
            if assign_loc_is_defn {
                return Err(error!(
                    "value cannot be converted to target type",
                    expr.loc,
                    format!("this has type `{}`", expr.expr_type.display(files)),
                    assign_loc,
                    "target type defined here"
                ));
            }

            return Err(error!(
                "value cannot be converted to target type",
                expr.loc,
                format!("this has type `{}`", expr.expr_type.display(files)),
                assign_loc,
                format!("this has type `{}`", assign_type.display(files))
            ));
        }
    }

    pub fn cast_convert<'b>(
        &self,
        buckets: BucketListRef<'b>,
        cast_to: &TCType,
        cast_to_loc: CodeLoc,
        expr: TCExpr<'b>,
    ) -> Result<TCExpr<'b>, Error> {
        return Err(error!(
            "not implemented",
            cast_to_loc, "casts aren't implemented yet"
        ));
    }

    pub fn check_struct_type(
        &self,
        struct_ident: u32,
        decl_idx: u32,
        pointer_count: u32,
        loc: CodeLoc,
    ) -> Result<SizeAlign, Error> {
        let no_struct = || error!("referenced struct doesn't exist", loc, "struct used here");

        let struct_type = self.structs.get(&struct_ident).ok_or_else(no_struct)?;
        if struct_type.decl_idx > decl_idx {
            return Err(error!(
                "used type declared later in file",
                struct_type.decl_loc, "type is declared here", loc, "type is used here"
            ));
        }

        if let Some(defn) = &struct_type.defn {
            if pointer_count == 0 && defn.defn_idx > decl_idx {
                return Err(error!(
                    "used type defined later in file",
                    defn.loc, "type is defined here", loc, "type is used here"
                ));
            }

            return Ok(defn.sa);
        } else if pointer_count == 0 {
            return Err(error!(
                "reference incomplete type without pointer indirection",
                struct_type.decl_loc, "incomplete type declared here", loc, "type used here"
            ));
        } else {
            // type incomplete but we have a pointer to it
            return Ok(TC_UNKNOWN_SA);
        }
    }

    pub fn check_struct_member(
        &self,
        struct_ident: u32,
        decl_idx: u32,
        loc: CodeLoc,
        member_ident: u32,
    ) -> Result<&TCStructMember, Error> {
        let struct_info = self.structs.get(&struct_ident).unwrap();
        let defn = if let Some(defn) = &struct_info.defn {
            defn
        } else {
            return Err(error!(
                "tried to get member of struct that's not defined",
                loc, "member access here"
            ));
        };

        for member in defn.members {
            if member.ident == member_ident {
                return Ok(member);
            }
        }

        return Err(error!(
            "couldn't find member in struct definition",
            defn.loc, "struct defined here", loc, "member accessed here"
        ));
    }
}

#[derive(Clone, Copy)]
pub struct CheckEnv<'a, 'b> {
    pub buckets: BucketListRef<'b>,
    pub types: &'a TypeEnv<'b>,
    pub func_types: &'a HashMap<u32, TCFuncType<'b>>,
    pub files: &'a FileDb,
    pub decl_idx: u32,
}

impl<'a, 'b> CheckEnv<'a, 'b> {
    pub fn new(
        buckets: BucketListRef<'b>,
        types: &'a TypeEnv<'b>,
        func_types: &'a HashMap<u32, TCFuncType<'b>>,
        files: &'a FileDb,
        decl_idx: u32,
    ) -> Self {
        Self {
            buckets,
            types,
            func_types,
            files,
            decl_idx,
        }
    }

    #[inline]
    pub fn check_type(&self, ast_type: &ASTType, pointer_count: u32) -> Result<TCType, Error> {
        self.types
            .check_type(self.decl_idx, ast_type, pointer_count)
    }

    #[inline]
    pub fn param_convert(
        &self,
        asgn_type: &TCType,
        asgn_loc: CodeLoc,
        expr: TCExpr<'b>,
    ) -> Result<TCExpr<'b>, Error> {
        self.types
            .implicit_convert(self.buckets, self.files, asgn_type, asgn_loc, true, expr)
    }

    #[inline]
    pub fn return_convert(
        &self,
        asgn_type: &TCType,
        asgn_loc: CodeLoc,
        expr: TCExpr<'b>,
    ) -> Result<TCExpr<'b>, Error> {
        self.types
            .implicit_convert(self.buckets, self.files, asgn_type, asgn_loc, true, expr)
    }

    #[inline]
    pub fn assign_convert(
        &self,
        asgn_type: &TCType,
        asgn_loc: CodeLoc,
        expr: TCExpr<'b>,
    ) -> Result<TCExpr<'b>, Error> {
        self.types
            .implicit_convert(self.buckets, self.files, asgn_type, asgn_loc, false, expr)
    }

    #[inline]
    pub fn cast_convert(
        &self,
        cast_to: &TCType,
        cast_to_loc: CodeLoc,
        expr: TCExpr<'b>,
    ) -> Result<TCExpr<'b>, Error> {
        self.types
            .cast_convert(self.buckets, cast_to, cast_to_loc, expr)
    }

    #[inline]
    pub fn check_struct_member(
        &self,
        struct_ident: u32,
        loc: CodeLoc,
        member_ident: u32,
    ) -> Result<&TCStructMember, Error> {
        self.types
            .check_struct_member(struct_ident, self.decl_idx, loc, member_ident)
    }
}

pub struct TypedFuncs<'a> {
    pub types: TypeEnv<'a>,
    pub functions: HashMap<u32, TCFunc<'a>>,
}

pub fn check_file<'a>(
    buckets: BucketListRef<'a>,
    program: ASTProgram,
    files: &FileDb,
) -> Result<TypedFuncs<'a>, Error> {
    let mut types = TypeEnv::new();

    struct UncheckedStructDefn {
        defn_idx: u32,
        members: Vec<(u32, TCType, CodeLoc)>,
        loc: CodeLoc,
    }

    struct UncheckedStruct {
        decl_idx: u32,
        decl_loc: CodeLoc,
        defn: Option<UncheckedStructDefn>,
    }

    // Add all types to the type table
    let mut unchecked_types: HashMap<u32, UncheckedStruct> = HashMap::new();
    for (decl_idx, stmt) in program.stmts.iter().enumerate() {
        let decl_type = match &stmt.kind {
            GlobalStmtKind::StructDecl(decl_type) => decl_type,
            _ => continue,
        };

        let defn_idx = decl_idx as u32;
        let mut decl_loc = decl_type.loc;
        let mut decl_idx = decl_idx as u32;
        if let Some(original) = unchecked_types.get(&decl_type.ident) {
            match (&original.defn, &decl_type.members) {
                (Some(_), Some(_)) => {
                    return Err(error!(
                        "redefinition of struct",
                        original.decl_loc,
                        "original definition here",
                        decl_type.loc,
                        "second definition here"
                    ));
                }
                _ => {}
            }

            decl_idx = original.decl_idx;
            decl_loc = original.decl_loc;
        }

        let mut unchecked_struct = UncheckedStruct {
            decl_idx,
            decl_loc,
            defn: None,
        };

        let members = match decl_type.members {
            Some(members) => members,
            None => {
                unchecked_types.insert(decl_type.ident, unchecked_struct);
                continue;
            }
        };

        let mut names = HashMap::new();
        let mut semi_typed_members = Vec::new();
        for member in members {
            let kind = match &member.decl_type.kind {
                ASTTypeKind::Int => TCTypeKind::I32,
                ASTTypeKind::Char => TCTypeKind::Char,
                ASTTypeKind::Void => TCTypeKind::Void,
                &ASTTypeKind::Struct { ident } => TCTypeKind::Struct {
                    ident,
                    sa: sa(TC_UNKNOWN_SIZE, 0),
                },
            };
            let member_type = TCType {
                kind,
                pointer_count: member.recv.pointer_count,
            };

            semi_typed_members.push((member.recv.ident, member_type, member.loc));
            if let Some(original_loc) = names.insert(member.recv.ident, member.loc) {
                return Err(error!(
                    "name redefined in struct",
                    original_loc, "first use of name here", member.loc, "second use here"
                ));
            }
        }

        let struct_defn = UncheckedStructDefn {
            defn_idx,
            loc: decl_type.loc,
            members: semi_typed_members,
        };
        unchecked_struct.defn = Some(struct_defn);
        unchecked_types.insert(decl_type.ident, unchecked_struct);
    }

    // return type meaning is (defn_idx, defn_loc, sa), where defn_idx is the
    // decl_idx if sa is not known, and same goes for defn_loc
    fn check_type<'b>(
        buckets: BucketListRef<'b>,
        visited: &mut HashSet<u32>,
        types: &mut TypeEnv<'b>,
        unchecked_types: &HashMap<u32, UncheckedStruct>,
        current_ident: u32,
        type_decl: &UncheckedStruct,
    ) -> Result<(u32, CodeLoc, SizeAlign), Error> {
        if !visited.insert(current_ident) {
            if let Some(found) = types.structs.get(&current_ident) {
                if let Some(defn) = &found.defn {
                    return Ok((defn.defn_idx, defn.loc, defn.sa));
                }
                return Ok((found.decl_idx, found.decl_loc, TC_UNKNOWN_SA));
            } else {
                return Err(error!(
                    "struct heirarchy contains cycle",
                    type_decl.decl_loc, "found cycle while solving this type"
                ));
            }
        }

        let defn = if let Some(defn) = &type_decl.defn {
            defn
        } else {
            types.structs.insert(
                current_ident,
                TCStruct {
                    decl_idx: type_decl.decl_idx,
                    decl_loc: type_decl.decl_loc,
                    defn: None,
                },
            );

            return Ok((type_decl.decl_idx, type_decl.decl_loc, TC_UNKNOWN_SA));
        };

        let mut size: u32 = 0;
        let mut align: u32 = 0;
        let mut typed_members = Vec::new();
        for (m_ident, m_type, m_loc) in defn.members.iter() {
            let offset = size;
            let mut m_type = *m_type;

            // m prefix to mean member's size align (msa), tprefix to mean target (tsa)
            let m_sa = if let TCTypeKind::Struct { ident, sa: t_sa } = &mut m_type.kind {
                if let Some(m_type_decl) = unchecked_types.get(m_ident) {
                    if m_type.pointer_count == 0 {
                        let (m_defn_idx, m_defn_loc, m_sa) = check_type(
                            buckets,
                            visited,
                            types,
                            unchecked_types,
                            *ident,
                            m_type_decl,
                        )?;

                        if m_sa == TC_UNKNOWN_SA {
                            return Err(error!(
                                "struct has incomplete type",
                                *m_loc, "struct here"
                            ));
                        }

                        if m_defn_idx > defn.defn_idx {
                            return Err(error!(
                                "struct is defined later in the file (order matters in C)",
                                m_defn_loc, "struct defined here", *m_loc, "struct referenced here"
                            ));
                        }

                        *t_sa = m_sa;
                        m_sa
                    } else if m_type_decl.decl_idx > type_decl.decl_idx {
                        return Err(error!(
                            "struct is declared later in the file (order matters in C)",
                            m_type_decl.decl_loc,
                            "struct declared here",
                            *m_loc,
                            "struct referenced here"
                        ));
                    } else {
                        sa(8, 8)
                    }
                } else {
                    return Err(error!(
                        "struct does not not exist",
                        *m_loc, "struct referenced here"
                    ));
                }
            } else {
                sa(m_type.size(), m_type.align())
            };

            size = align_u32(size, m_sa.align) + m_sa.size;
            align = u32::max(m_sa.align, align);

            typed_members.push(TCStructMember {
                ident: *m_ident,
                decl_type: m_type,
                loc: *m_loc,
                offset,
            });
        }

        let sa = sa(align_u32(size, align), align);
        let checked_defn = TCStructDefn {
            defn_idx: defn.defn_idx,
            loc: defn.loc,
            sa,
            members: buckets.add_array(typed_members),
        };

        types.structs.insert(
            current_ident,
            TCStruct {
                decl_idx: type_decl.decl_idx,
                decl_loc: type_decl.decl_loc,
                defn: Some(checked_defn),
            },
        );

        return Ok((defn.defn_idx, defn.loc, sa));
    }

    let mut visited = HashSet::new();
    for (ident, unchecked) in unchecked_types.iter() {
        check_type(
            buckets,
            &mut visited,
            &mut types,
            &unchecked_types,
            *ident,
            unchecked,
        )?;
    }

    let mut func_types: HashMap<u32, TCFuncType<'a>> = HashMap::new();

    struct UncheckedFunction<'a> {
        defn_idx: u32,
        rtype_loc: CodeLoc,
        loc: CodeLoc,
        body: &'a [Stmt<'a>],
    }

    let mut unchecked_functions = HashMap::new();
    for (decl_idx, stmt) in program.stmts.iter().enumerate() {
        let (rtype, rpointer_count, ident, params, func_body) = match &stmt.kind {
            GlobalStmtKind::FuncDecl {
                return_type,
                ident,
                pointer_count,
                params,
            } => (return_type, pointer_count, ident, params, None),
            GlobalStmtKind::Func {
                return_type,
                ident,
                pointer_count,
                params,
                body,
            } => (return_type, pointer_count, ident, params, Some(body)),
            _ => continue,
        };

        let decl_idx = decl_idx as u32;
        let rtype_loc = rtype.loc;
        let return_type = types.check_type(decl_idx, rtype, *rpointer_count)?;

        let mut names = HashMap::new();
        let mut typed_params = Vec::new();
        let mut varargs = None;
        for param in params.iter() {
            if let Some(loc) = varargs {
                return Err(error!(
                    "function parameter after vararg",
                    loc, "vararg indicator here", param.loc, "parameter here"
                ));
            }

            let (decl_type, recv) = match &param.kind {
                ParamKind::Vararg => {
                    varargs = Some(param.loc);
                    continue;
                }
                ParamKind::StructLike { decl_type, recv } => (decl_type, *recv),
            };

            if let Some(original) = names.insert(recv.ident, param.loc) {
                println!("{}", ident);
                return Err(error!(
                    "redeclaration of function parameter",
                    original, "original declaration here", param.loc, "second declaration here"
                ));
            }

            let param_type = types.check_type(decl_idx, decl_type, recv.pointer_count)?;

            typed_params.push(TCFuncParam {
                decl_type: param_type,
                ident: recv.ident,
                loc: param.loc,
            });
        }

        let typed_params = buckets.add_array(typed_params);
        let tc_func_type = TCFuncType {
            return_type,
            loc: stmt.loc,
            params: typed_params,
            decl_idx,
            varargs: varargs.is_some(),
        };

        if let Some(prev_tc_func_type) = func_types.get(ident) {
            if prev_tc_func_type != &tc_func_type {
                return Err(func_decl_mismatch(prev_tc_func_type.loc, tc_func_type.loc));
            }

            if let Some((ftype, Some(fbody))) = unchecked_functions.get(ident) {
                if let Some(body) = func_body {
                    return Err(func_redef(prev_tc_func_type.loc, tc_func_type.loc));
                }
            }
        } else {
            func_types.insert(*ident, tc_func_type);
        }

        if let Some(body) = func_body {
            let unchecked_func = UncheckedFunction {
                defn_idx: decl_idx,
                rtype_loc,
                loc: stmt.loc,
                body,
            };
            unchecked_functions.insert(*ident, (tc_func_type, Some(unchecked_func)));
        } else {
            unchecked_functions.insert(*ident, (tc_func_type, None));
        }
    }

    if let Some((ftype, func)) = unchecked_functions.get(&INIT_SYMS.translate["main"]) {
        if ftype.return_type.pointer_count != 0 {
            return Err(main_return_type(ftype.loc));
        }

        match ftype.return_type.kind {
            TCTypeKind::Void | TCTypeKind::I32 => {}
            _ => {
                return Err(main_return_type(ftype.loc));
            }
        }

        let int_type = TCType {
            kind: TCTypeKind::I32,
            pointer_count: 0,
        };
        let char_ss_type = TCType {
            kind: TCTypeKind::Char,
            pointer_count: 2,
        };

        if ftype.params.len() == 2 {
            if ftype.params[0].decl_type != int_type {
                return Err(main_param_types(ftype.params[0].loc));
            } else if ftype.params[1].decl_type != char_ss_type {
                return Err(main_param_types(ftype.loc));
            }
        } else if ftype.params.len() != 0 {
            return Err(main_param_types(ftype.loc));
        }
    }

    let mut functions = HashMap::new();
    for (func_name, (ftype, func)) in unchecked_functions.into_iter() {
        let mut tc_func = TCFunc {
            func_type: ftype,
            defn: None,
        };

        let func = match func {
            Some(func) => func,
            None => {
                functions.insert(func_name, tc_func);
                continue;
            }
        };

        let mut local_env = LocalTypeEnv::new(ftype.return_type, func.rtype_loc);
        let param_count = if ftype.varargs {
            ftype.params.len() + 1
        } else {
            ftype.params.len()
        };

        for (idx, param) in ftype.params.iter().enumerate() {
            let var_offset = idx as i16 - param_count as i16;
            let tc_value = TCVar {
                decl_type: param.decl_type,
                var_offset,
                loc: param.loc,
            };

            local_env.add_var(param.ident, tc_value).unwrap();
        }

        let env = CheckEnv::new(buckets, &types, &func_types, files, ftype.decl_idx);

        let gstmts = check_stmts(env, &mut local_env, func.body, None)?;

        tc_func.defn = Some(TCFuncDefn {
            defn_idx: func.defn_idx,
            loc: func.loc,
            stmts: env.buckets.add_array(gstmts),
        });

        functions.insert(func_name, tc_func);
    }

    return Ok(TypedFuncs { types, functions });
}

fn check_stmts<'b>(
    env: CheckEnv<'_, 'b>,
    local_env: &mut LocalTypeEnv,
    stmts: &[Stmt],
    cblock: Option<TCStmt<'b>>,
) -> Result<Vec<TCStmt<'b>>, Error> {
    let mut tstmts = Vec::new();
    for stmt in stmts {
        match &stmt.kind {
            StmtKind::RetVal(expr) => {
                let expr = check_expr(env, local_env, expr)?;
                let rtype = local_env.return_type;
                if rtype.pointer_count == 0
                    && rtype.kind == TCTypeKind::Void
                    && rtype != expr.expr_type
                {
                    return Err(error!(
                        "void function should not return a value",
                        expr.loc, "value is here"
                    ));
                }

                let expr = env.return_convert(&local_env.return_type, local_env.rtype_loc, expr)?;

                tstmts.push(TCStmt {
                    loc: stmt.loc,
                    kind: TCStmtKind::RetVal(expr),
                });
            }
            StmtKind::Ret => {
                let rtype = local_env.return_type;
                if rtype.pointer_count != 0 || rtype.kind != TCTypeKind::Void {
                    return Err(error!(
                        "expected value in return statement (return type is not void)",
                        local_env.rtype_loc,
                        "target type is here".to_string(),
                        stmt.loc,
                        "return statement is here".to_string()
                    ));
                }

                tstmts.push(TCStmt {
                    loc: stmt.loc,
                    kind: TCStmtKind::Ret,
                });
            }

            StmtKind::Expr(expr) => {
                let expr = check_expr(env, local_env, expr)?;
                tstmts.push(TCStmt {
                    loc: expr.loc,
                    kind: TCStmtKind::Expr(expr),
                });
            }

            StmtKind::Decl { decl_type, decls } => {
                for Decl { recv, loc, expr } in *decls {
                    let decl_type = env.check_type(decl_type, recv.pointer_count)?;
                    if decl_type == VOID {
                        return Err(void_variable(*loc));
                    }

                    let expr = check_expr(env, local_env, &expr)?;
                    local_env.add_local(recv.ident, decl_type, *loc)?;
                    let expr = env.assign_convert(&decl_type, recv.loc, expr)?;
                    tstmts.push(TCStmt {
                        kind: TCStmtKind::Decl {
                            symbol: recv.ident,
                            init: expr,
                        },
                        loc: *loc,
                    });
                }
            }

            StmtKind::Nop => {}

            StmtKind::Branch {
                if_cond,
                if_body,
                else_body,
            } => {
                let cond = check_expr(env, local_env, if_cond)?;
                if let TCTypeKind::Struct { .. } = cond.expr_type.kind {
                    return Err(truth_value_of_struct(cond.loc));
                }

                let mut if_env = local_env.child();
                let tc_if_body = check_stmts(env, &mut if_env, if_body.stmts, cblock)?;

                let mut else_env = local_env.child();
                let tc_else_body = check_stmts(env, &mut else_env, else_body.stmts, cblock)?;

                let tc_if_body = env.buckets.add_array(tc_if_body);
                let tc_else_body = env.buckets.add_array(tc_else_body);

                tstmts.push(TCStmt {
                    kind: TCStmtKind::Branch {
                        cond,
                        if_body: TCBlock {
                            stmts: tc_if_body,
                            loc: if_body.loc,
                        },
                        else_body: TCBlock {
                            stmts: tc_else_body,
                            loc: else_body.loc,
                        },
                    },
                    loc: stmt.loc,
                });
            }

            StmtKind::For {
                at_start,
                condition,
                post_expr,
                body,
            } => {
                let mut block_stmts = Vec::new();
                let at_start = check_expr(env, local_env, at_start)?;
                block_stmts.push(TCStmt {
                    loc: at_start.loc,
                    kind: TCStmtKind::Expr(at_start),
                });

                let cond = check_expr(env, local_env, condition)?;
                if let TCTypeKind::Struct { .. } = cond.expr_type.kind {
                    return Err(truth_value_of_struct(cond.loc));
                }

                let post = check_expr(env, local_env, post_expr)?;
                let post = TCStmt {
                    loc: post.loc,
                    kind: TCStmtKind::Expr(post),
                };

                let mut for_env = local_env.child();
                let mut loop_stmts = check_stmts(env, &mut for_env, body.stmts, Some(post))?;

                loop_stmts.push(post);

                loop_stmts.push(TCStmt {
                    loc: condition.loc,
                    kind: TCStmtKind::Branch {
                        if_body: TCBlock {
                            stmts: &[],
                            loc: condition.loc,
                        },
                        else_body: TCBlock {
                            stmts: env.buckets.add_array(vec![TCStmt {
                                kind: TCStmtKind::Break,
                                loc: condition.loc,
                            }]),
                            loc: condition.loc,
                        },
                        cond,
                    },
                });

                loop_stmts.rotate_right(1);

                block_stmts.push(TCStmt {
                    loc: body.loc,
                    kind: TCStmtKind::Loop(TCBlock {
                        loc: body.loc,
                        stmts: env.buckets.add_array(loop_stmts),
                    }),
                });

                tstmts.push(TCStmt {
                    kind: TCStmtKind::Block(TCBlock {
                        loc: stmt.loc,
                        stmts: env.buckets.add_array(block_stmts),
                    }),
                    loc: stmt.loc,
                });
            }
            StmtKind::ForDecl {
                at_start_decl_type,
                at_start,
                condition,
                post_expr,
                body,
            } => {
                let mut block_stmts = Vec::new();
                let mut for_env = local_env.child();

                for decl in *at_start {
                    let decl_type = env.check_type(at_start_decl_type, decl.recv.pointer_count)?;
                    if decl_type == VOID {
                        return Err(void_variable(decl.loc));
                    }

                    let expr = check_expr(env, &mut for_env, &decl.expr)?;
                    let expr = env.assign_convert(&decl_type, decl.loc, expr)?;
                    for_env.add_local(decl.recv.ident, decl_type, decl.loc)?;
                    block_stmts.push(TCStmt {
                        kind: TCStmtKind::Decl {
                            symbol: decl.recv.ident,
                            init: expr,
                        },
                        loc: decl.loc,
                    });
                }

                let cond = check_expr(env, &for_env, condition)?;
                if let TCTypeKind::Struct { .. } = cond.expr_type.kind {
                    return Err(truth_value_of_struct(cond.loc));
                }

                let post = check_expr(env, &for_env, post_expr)?;
                let post = TCStmt {
                    loc: post.loc,
                    kind: TCStmtKind::Expr(post),
                };

                let mut loop_stmts = check_stmts(env, &mut for_env, body.stmts, Some(post))?;

                loop_stmts.push(post);

                loop_stmts.push(TCStmt {
                    loc: condition.loc,
                    kind: TCStmtKind::Branch {
                        if_body: TCBlock {
                            stmts: &[],
                            loc: condition.loc,
                        },
                        else_body: TCBlock {
                            stmts: env.buckets.add_array(vec![TCStmt {
                                kind: TCStmtKind::Break,
                                loc: condition.loc,
                            }]),
                            loc: condition.loc,
                        },
                        cond,
                    },
                });

                loop_stmts.rotate_right(1);

                block_stmts.push(TCStmt {
                    loc: body.loc,
                    kind: TCStmtKind::Loop(TCBlock {
                        loc: body.loc,
                        stmts: env.buckets.add_array(loop_stmts),
                    }),
                });

                tstmts.push(TCStmt {
                    kind: TCStmtKind::Block(TCBlock {
                        loc: stmt.loc,
                        stmts: env.buckets.add_array(block_stmts),
                    }),
                    loc: stmt.loc,
                });
            }

            StmtKind::While { condition, body } => {
                let cond = check_expr(env, local_env, condition)?;
                if let TCTypeKind::Struct { .. } = cond.expr_type.kind {
                    return Err(truth_value_of_struct(cond.loc));
                }

                let mut while_env = local_env.child();
                let mut loop_stmts = check_stmts(env, &mut while_env, body.stmts, None)?;

                loop_stmts.push(TCStmt {
                    loc: condition.loc,
                    kind: TCStmtKind::Branch {
                        if_body: TCBlock {
                            stmts: &[],
                            loc: condition.loc,
                        },
                        else_body: TCBlock {
                            stmts: env.buckets.add_array(vec![TCStmt {
                                kind: TCStmtKind::Break,
                                loc: condition.loc,
                            }]),
                            loc: condition.loc,
                        },
                        cond,
                    },
                });

                loop_stmts.rotate_right(1);

                tstmts.push(TCStmt {
                    kind: TCStmtKind::Loop(TCBlock {
                        loc: body.loc,
                        stmts: env.buckets.add_array(loop_stmts),
                    }),
                    loc: stmt.loc,
                });
            }

            StmtKind::Block(block) => {
                let mut block_env = local_env.child();
                let block_stmts = check_stmts(env, &mut block_env, block.stmts, cblock)?;
                tstmts.push(TCStmt {
                    kind: TCStmtKind::Block(TCBlock {
                        loc: stmt.loc,
                        stmts: env.buckets.add_array(block_stmts),
                    }),
                    loc: stmt.loc,
                });
            } // x => panic!("{:?} is unimplemented", x),

            StmtKind::Continue => {
                if let Some(cblock) = cblock {
                    tstmts.push(cblock);
                }

                tstmts.push(TCStmt {
                    kind: TCStmtKind::Continue,
                    loc: stmt.loc,
                });
            }
            StmtKind::Break => {
                tstmts.push(TCStmt {
                    kind: TCStmtKind::Break,
                    loc: stmt.loc,
                });
            }
        }
    }

    return Ok(tstmts);
}

fn check_expr<'b>(
    env: CheckEnv<'_, 'b>,
    local_env: &LocalTypeEnv,
    expr: &Expr,
) -> Result<TCExpr<'b>, Error> {
    match expr.kind {
        ExprKind::Uninit => {
            return Ok(TCExpr {
                kind: TCExprKind::Uninit,
                expr_type: TCType {
                    kind: TCTypeKind::Uninit { size: 0 },
                    pointer_count: 0,
                },
                loc: expr.loc,
            });
        }
        ExprKind::IntLiteral(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::IntLiteral(val),
                expr_type: TCType {
                    kind: TCTypeKind::I32,
                    pointer_count: 0,
                },
                loc: expr.loc,
            });
        }
        ExprKind::StringLiteral(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::StringLiteral(env.buckets.add_str(val)),
                expr_type: TCType {
                    kind: TCTypeKind::Char,
                    pointer_count: 1,
                },
                loc: expr.loc,
            });
        }
        ExprKind::Ident(id) => {
            let tc_var = match local_env.var(id) {
                Some(tc_var) => tc_var,
                None => {
                    return Err(error!("couldn't find name", expr.loc, "identifier here"));
                }
            };

            return Ok(TCExpr {
                kind: TCExprKind::LocalIdent {
                    var_offset: tc_var.var_offset,
                },
                expr_type: tc_var.decl_type,
                loc: expr.loc,
            });
        }

        ExprKind::SizeofType {
            sizeof_type,
            pointer_count,
        } => {
            let tc_sizeof_type = env
                .types
                .check_type(env.decl_idx, &sizeof_type, pointer_count)?;
            if tc_sizeof_type == VOID {
                return Err(error!(
                    "sizeof called on void type (this doesn't make sense because void doesn't have a size)",
                    expr.loc, "called here"
                ));
            }

            return Ok(TCExpr {
                kind: TCExprKind::IntLiteral(tc_sizeof_type.size() as i32), // TODO change this to unsigned long
                expr_type: TCType {
                    kind: TCTypeKind::I32,
                    pointer_count: 0,
                },
                loc: expr.loc,
            });
        }
        ExprKind::SizeofExpr(sizeof_expr) => {
            let tc_expr = check_expr(env, local_env, sizeof_expr)?;

            return Ok(TCExpr {
                kind: TCExprKind::IntLiteral(tc_expr.expr_type.size() as i32), // TODO change this to unsigned long
                expr_type: TCType {
                    kind: TCTypeKind::I32,
                    pointer_count: 0,
                },
                loc: expr.loc,
            });
        }

        ExprKind::BraceList(exprs) => {
            let mut tc_exprs = Vec::new();
            for expr in exprs {
                tc_exprs.push(check_expr(env, local_env, expr)?);
            }

            return Ok(TCExpr {
                expr_type: BRACE_LIST,
                kind: TCExprKind::BraceList(env.buckets.add_array(tc_exprs)),
                loc: expr.loc,
            });
        }
        ExprKind::ParenList(exprs) => {
            let mut tc_exprs = Vec::new();
            for expr in exprs {
                tc_exprs.push(check_expr(env, local_env, expr)?);
            }

            return Ok(TCExpr {
                expr_type: tc_exprs[tc_exprs.len() - 1].expr_type,
                kind: TCExprKind::ParenList(env.buckets.add_array(tc_exprs)),
                loc: expr.loc,
            });
        }

        ExprKind::Assign(target, value) => {
            let target = check_assign_target(env, local_env, target)?;
            let value = check_expr(env, local_env, value)?;

            let value = env.assign_convert(&target.target_type, target.target_loc, value)?;

            let value = env.buckets.add(value);

            return Ok(TCExpr {
                expr_type: target.target_type,
                loc: expr.loc,
                kind: TCExprKind::Assign { target, value },
            });
        }

        ExprKind::BinOp(op, l, r) => {
            let l = check_expr(env, local_env, l)?;
            let r = check_expr(env, local_env, r)?;

            let bin_op = get_overload(env, op, &l, &r)?;
            return Ok(bin_op(env.buckets, l, r));
        }

        ExprKind::Member { base, member } => {
            let base = check_expr(env, local_env, base)?;

            let struct_id = if let TCTypeKind::Struct { ident, .. } = base.expr_type.kind {
                ident
            } else {
                return Err(member_of_non_struct(base.loc));
            };

            let member_info = env.check_struct_member(struct_id, base.loc, member)?;

            return Ok(TCExpr {
                expr_type: member_info.decl_type,
                loc: expr.loc,
                kind: TCExprKind::Member {
                    base: env.buckets.add(base),
                    offset: member_info.offset,
                },
            });
        }
        ExprKind::PtrMember { base, member } => {
            let base = check_expr(env, local_env, base)?;

            let struct_id = if let TCTypeKind::Struct { ident, .. } = base.expr_type.kind {
                ident
            } else {
                return Err(member_of_non_struct(base.loc));
            };

            let deref_type = env.types.deref(&base.expr_type, base.loc)?;
            if deref_type.pointer_count != 0 {
                return Err(ptr_member_of_poly_pointer(base.loc, &deref_type));
            }

            let member_info = env.check_struct_member(struct_id, base.loc, member)?;

            return Ok(TCExpr {
                expr_type: member_info.decl_type,
                loc: expr.loc,
                kind: TCExprKind::PtrMember {
                    base: env.buckets.add(base),
                    offset: member_info.offset,
                },
            });
        }

        ExprKind::Deref(ptr) => {
            let value = check_expr(env, local_env, ptr)?;

            let expr_type = env.types.deref(&value.expr_type, value.loc)?;
            return Ok(TCExpr {
                expr_type,
                loc: expr.loc,
                kind: TCExprKind::Deref(env.buckets.add(value)),
            });
        }
        ExprKind::Ref(target) => {
            let target = check_assign_target(env, local_env, target)?;
            let mut expr_type = target.target_type;
            expr_type.pointer_count += 1;
            return Ok(TCExpr {
                expr_type,
                loc: expr.loc,
                kind: TCExprKind::Ref(target),
            });
        }

        ExprKind::Call { function, params } => {
            let func_id = if let ExprKind::Ident(id) = function.kind {
                id
            } else {
                return Err(error!(
                    "calling an expression that isn't a function",
                    function.loc, "called here"
                ));
            };

            let func_type = if let Some(func_type) = env.func_types.get(&func_id) {
                func_type
            } else {
                return Err(error!("function doesn't exist", expr.loc, "called here"));
            };

            if func_type.decl_idx > env.decl_idx {
                return Err(error!(
                    "function hasn't been declared yet (declaration order matters in C)",
                    expr.loc, "function called here", func_type.loc, "function declared here"
                ));
            }

            if params.len() < func_type.params.len()
                || (params.len() > func_type.params.len() && !func_type.varargs)
            {
                return Err(error!(
                    "function call has wrong number of parameters",
                    expr.loc, "function called here", func_type.loc, "function declared here"
                ));
            }

            let mut tparams = Vec::new();
            for (idx, param) in params.iter().enumerate() {
                let mut expr = check_expr(env, local_env, param)?;
                if idx < func_type.params.len() {
                    let param_type = &func_type.params[idx];
                    expr = env.param_convert(&param_type.decl_type, func_type.loc, expr)?;
                }

                tparams.push(expr);
            }

            return Ok(TCExpr {
                kind: TCExprKind::Call {
                    func: func_id,
                    params: env.buckets.add_array(tparams),
                    varargs: func_type.varargs,
                },
                expr_type: func_type.return_type,
                loc: expr.loc,
            });
        }

        ExprKind::Cast {
            cast_to,
            cast_to_loc,
            pointer_count,
            expr,
        } => {
            let cast_to = env.check_type(&cast_to, pointer_count)?;
            let expr = check_expr(env, local_env, expr)?;
            return env.cast_convert(&cast_to, cast_to_loc, expr);
        }

        x => panic!("{:?} is unimplemented", x),
    }
}

fn check_assign_target<'b>(
    env: CheckEnv<'_, 'b>,
    local_env: &LocalTypeEnv,
    expr: &Expr,
) -> Result<TCAssignTarget<'b>, Error> {
    match &expr.kind {
        ExprKind::Ident(id) => {
            let tc_var = match local_env.var(*id) {
                Some(tc_var) => tc_var,
                None => {
                    return Err(ident_not_found(&env.types, expr.loc));
                }
            };

            let kind = TCAssignKind::LocalIdent {
                var_offset: tc_var.var_offset,
            };

            return Ok(TCAssignTarget {
                kind,
                defn_loc: Some(tc_var.loc),
                target_loc: expr.loc,
                target_type: tc_var.decl_type,
                offset: 0,
            });
        }

        ExprKind::Member { base, member } => {
            let base_loc = base.loc;
            let base = check_assign_target(env, local_env, base)?;

            let struct_id = if let TCTypeKind::Struct { ident, .. } = base.target_type.kind {
                ident
            } else {
                return Err(member_of_non_struct(base.target_loc));
            };

            let member_info = env.check_struct_member(struct_id, base.target_loc, *member)?;

            return Ok(TCAssignTarget {
                kind: base.kind,
                defn_loc: Some(member_info.loc),
                target_loc: expr.loc,
                target_type: member_info.decl_type,
                offset: member_info.offset,
            });
        }
        ExprKind::PtrMember { base, member } => {
            let base_loc = base.loc;
            let base = check_expr(env, local_env, base)?;

            let struct_id = if let TCTypeKind::Struct { ident, .. } = base.expr_type.kind {
                ident
            } else {
                return Err(member_of_non_struct(base.loc));
            };

            let deref_type = env.types.deref(&base.expr_type, base.loc)?;
            if deref_type.pointer_count != 0 {
                return Err(ptr_member_of_poly_pointer(base.loc, &deref_type));
            }

            let member_info = env.check_struct_member(struct_id, base.loc, *member)?;

            return Ok(TCAssignTarget {
                kind: TCAssignKind::Ptr(env.buckets.add(base)),
                defn_loc: Some(member_info.loc),
                target_loc: expr.loc,
                target_type: member_info.decl_type,
                offset: member_info.offset,
            });
        }

        ExprKind::Deref(ptr) => {
            let ptr = check_expr(env, local_env, ptr)?;

            let target_type = env.types.deref(&ptr.expr_type, ptr.loc)?;
            return Ok(TCAssignTarget {
                kind: TCAssignKind::Ptr(env.buckets.add(ptr)),
                target_loc: expr.loc,
                defn_loc: None,
                target_type,
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

pub fn main_param_types(loc: CodeLoc) -> Error {
    return error!(
        "can only have param types of (int, char**) or no params for the main function",
        loc, "invalid param types found here"
    );
}

pub fn main_return_type(loc: CodeLoc) -> Error {
    return error!(
        "can only have return type of void or int for the main function",
        loc, "invalid return type found here"
    );
}

pub fn void_variable(loc: CodeLoc) -> Error {
    return error!(
        "cannot define a variable of type void",
        loc, "incorrect variable definition here"
    );
}

pub fn truth_value_of_struct(loc: CodeLoc) -> Error {
    error!(
        "tried to check truth value of struct",
        loc, "this is a struct, when it should be a number or pointer"
    )
}

pub fn dereference_of_non_pointer(value_loc: CodeLoc, value_type: &TCType) -> Error {
    return error!(
        "cannot dereference values that aren't pointers",
        value_loc,
        format!(
            "value has type {:?}, which cannot be dereferenced",
            value_type
        )
    );
}

pub fn ptr_member_of_poly_pointer(ptr_loc: CodeLoc, ptr_type: &TCType) -> Error {
    error!(
        "need to dereference pointer before you can access its members",
        ptr_loc,
        format!(
            "this points to an object of type {:?}, which doesn't have any members",
            ptr_type
        )
    )
}

pub fn member_of_non_struct(loc: CodeLoc) -> Error {
    error!(
        "cannot access member of non-struct",
        loc, "access happened here"
    )
}

pub fn ident_not_found(env: &TypeEnv, loc: CodeLoc) -> Error {
    return error!("couldn't find name", loc, "identifier here");
}

pub fn func_decl_mismatch(original: CodeLoc, new: CodeLoc) -> Error {
    return error!(
        "function declaration type doesn't match previous declaration",
        original, "original declaration here", new, "second declaration here"
    );
}

pub fn func_redef(original: CodeLoc, redef: CodeLoc) -> Error {
    return error!(
        "redefinition of function",
        original, "original definition here", redef, "second definition here"
    );
}
