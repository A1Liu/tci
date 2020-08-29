use crate::ast::*;
use crate::buckets::{BucketList, BucketListRef};
use crate::util::*;
use crate::*;
use std::collections::{HashMap, HashSet};

type BinOpTransform = for<'a, 'b> fn(&'a BucketList<'b>, TCExpr<'b>, TCExpr<'b>) -> TCExpr<'b>;

fn add_int_int<'a, 'b>(buckets: &'a BucketList<'b>, l: TCExpr<'b>, r: TCExpr<'b>) -> TCExpr<'b> {
    let result_type = TCType {
        kind: TCTypeKind::I32,
        pointer_count: 0,
    };

    return TCExpr {
        range: r_from(l.range, r.range),
        kind: TCExprKind::AddI32(buckets.add(l), buckets.add(r)),
        expr_type: result_type,
    };
}

fn add_int_char<'a, 'b>(buckets: &'a BucketList<'b>, l: TCExpr<'b>, r: TCExpr<'b>) -> TCExpr<'b> {
    let result_type = TCType {
        kind: TCTypeKind::I32,
        pointer_count: 0,
    };

    let r = TCExpr {
        range: l.range,
        kind: TCExprKind::SConv8To32(buckets.add(r)),
        expr_type: result_type,
    };

    return TCExpr {
        range: r_from(l.range, r.range),
        kind: TCExprKind::AddI32(buckets.add(l), buckets.add(r)),
        expr_type: result_type,
    };
}

fn add_char_int<'a, 'b>(buckets: &'a BucketList<'b>, l: TCExpr<'b>, r: TCExpr<'b>) -> TCExpr<'b> {
    let result_type = TCType {
        kind: TCTypeKind::I32,
        pointer_count: 0,
    };

    let l = TCExpr {
        range: l.range,
        kind: TCExprKind::SConv8To32(buckets.add(l)),
        expr_type: result_type,
    };

    return TCExpr {
        range: r_from(l.range, r.range),
        kind: TCExprKind::AddI32(buckets.add(l), buckets.add(r)),
        expr_type: result_type,
    };
}

fn sub_int_int<'a, 'b>(buckets: &'a BucketList<'b>, l: TCExpr<'b>, r: TCExpr<'b>) -> TCExpr<'b> {
    let result_type = TCType {
        kind: TCTypeKind::I32,
        pointer_count: 0,
    };

    return TCExpr {
        range: r_from(l.range, r.range),
        kind: TCExprKind::SubI32(buckets.add(l), buckets.add(r)),
        expr_type: result_type,
    };
}

lazy_static! {
    pub static ref BIN_OP_OVERLOADS: HashMap<(BinOp, TCShallowType, TCShallowType), BinOpTransform> = {
        use TCShallowType::*;
        let mut m: HashMap<(BinOp, TCShallowType, TCShallowType), BinOpTransform> = HashMap::new();
        m.insert((BinOp::Add, I32, I32), add_int_int);
        m.insert((BinOp::Add, I32, Char), add_int_char);
        m.insert((BinOp::Add, Char, I32), add_char_int);
        m.insert((BinOp::Sub, I32, I32), sub_int_int);
        m
    };
    pub static ref BIN_LEFT_OVERLOADS: HashSet<(BinOp, TCShallowType)> = {
        use TCShallowType::*;
        let mut m = HashSet::new();
        m.insert((BinOp::Add, I32));
        m.insert((BinOp::Add, Char));
        m.insert((BinOp::Sub, I32));
        m
    };
    pub static ref BIN_RIGHT_OVERLOADS: HashSet<(BinOp, TCShallowType)> = {
        use TCShallowType::*;
        let mut m = HashSet::new();
        m.insert((BinOp::Add, I32));
        m.insert((BinOp::Add, Char));
        m.insert((BinOp::Sub, I32));
        m
    };
}

fn get_overload(env: &TypeEnv, op: BinOp, l: &TCExpr, r: &TCExpr) -> Result<BinOpTransform, Error> {
    let key = (op, l.expr_type.to_shallow(), r.expr_type.to_shallow());
    match BIN_OP_OVERLOADS.get(&key) {
        Some(bin_op) => return Ok(*bin_op),
        None => return Err(invalid_operands_bin_expr(env, op, l, r)),
    }
}

pub fn invalid_operands_bin_expr(env: &TypeEnv, op: BinOp, l: &TCExpr, r: &TCExpr) -> Error {
    let lkey = (op, l.expr_type.to_shallow());
    let rkey = (op, r.expr_type.to_shallow());

    if BIN_LEFT_OVERLOADS.get(&lkey).is_none() {
        return error!(
            "invalid operands to binary expression (left expression is not valid for this operand)",
            l.range,
            env.file,
            format!("this has type {:?}", l.expr_type),
            r.range,
            env.file,
            format!("this has type {:?}", r.expr_type)
        );
    }

    if BIN_RIGHT_OVERLOADS.get(&rkey).is_none() {
        return error!(
            "invalid operands to binary expression (right expression is not valid for this operand)",
            l.range,
            env.file,
            format!("this has type {:?}", l.expr_type),
            r.range,
            env.file,
            format!("this has type {:?}", r.expr_type)
            );
    }

    return error!(
        "invalid operands to binary expression",
        l.range,
        env.file,
        format!("this has type {:?}", l.expr_type),
        r.range,
        env.file,
        format!("this has type {:?}", r.expr_type)
    );
}

pub struct LocalTypeEnv {
    pub symbols: HashMap<u32, TCVar>,
    pub return_type: TCType,
    pub rtype_range: Range,
    pub parent: *const LocalTypeEnv,
    pub decl_idx: i16,
}

impl LocalTypeEnv {
    pub fn new(return_type: TCType, rtype_range: Range) -> Self {
        Self {
            symbols: HashMap::new(),
            return_type,
            rtype_range,
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
                rtype_range: self.rtype_range,
                parent: self.parent,
                decl_idx: self.decl_idx,
            }
        } else {
            Self {
                symbols: HashMap::new(),
                return_type: self.return_type,
                rtype_range: self.rtype_range,
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
                var_type.loc.range,
                var_type.loc.file,
                "first declaration defined here",
                tc_loc.range,
                tc_loc.file,
                "redecaration defined here"
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
    file: u32,
    structs: HashMap<u32, TCStruct<'a>>,
}

impl<'a> TypeEnv<'a> {
    pub fn new(file: u32) -> Self {
        Self {
            file,
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
                let sa = self.check_struct_type(ident, decl_idx, pointer_count, ast_type.range)?;
                TCTypeKind::Struct { ident, sa }
            }
        };

        return Ok(TCType {
            kind,
            pointer_count,
        });
    }

    pub fn deref(&self, tc_type: &TCType, value_range: Range) -> Result<TCType, Error> {
        if tc_type.pointer_count == 0 {
            return Err(dereference_of_non_pointer(value_range, self.file, tc_type));
        }

        if let TCTypeKind::Struct {
            sa: TC_UNKNOWN_SA, ..
        } = tc_type.kind
        {
            return Err(error!(
                "cannot dereference pointer to struct of unknown size",
                value_range,
                self.file,
                format!("value has type {:?}, which cannot be dereferenced", tc_type)
            ));
        }

        let mut other = *tc_type;
        other.pointer_count -= 1;
        return Ok(other);
    }

    // TODO make this work with implicit type conversions
    pub fn assign_convert<'b>(
        &self,
        buckets: BucketListRef<'b>,
        assign_to: &TCType,
        assign_to_loc: Option<CodeLoc>,
        expr: TCExpr<'b>,
    ) -> Result<TCExpr<'b>, Error> {
        if let TCTypeKind::Uninit { .. } = expr.expr_type.kind {
            return Ok(TCExpr {
                kind: TCExprKind::Uninit,
                expr_type: *assign_to,
                range: expr.range,
            });
        }

        if assign_to == &expr.expr_type {
            return Ok(expr);
        } else if let Some(assign_loc) = assign_to_loc {
            return Err(error!(
                "value cannot be converted to target type",
                expr.range,
                self.file,
                "value found here",
                assign_loc.range,
                assign_loc.file,
                "expected type defined here"
            ));
        } else {
            return Err(error!(
                "value cannot be converted to target type",
                expr.range, self.file, "value found here"
            ));
        }
    }

    pub fn check_struct_type(
        &self,
        struct_ident: u32,
        decl_idx: u32,
        pointer_count: u32,
        range: Range,
    ) -> Result<SizeAlign, Error> {
        let no_struct = || {
            error!(
                "referenced struct doesn't exist",
                range.cloc(self.file),
                "struct used here"
            )
        };

        let struct_type = self.structs.get(&struct_ident).ok_or_else(no_struct)?;
        if struct_type.decl_idx > decl_idx {
            return Err(error!(
                "used type declared later in file",
                struct_type.decl_loc,
                "type is declared here",
                range.cloc(self.file),
                "type is used here"
            ));
        }

        if let Some(defn) = &struct_type.defn {
            if pointer_count == 0 && defn.defn_idx > decl_idx {
                return Err(error!(
                    "used type defined later in file",
                    defn.loc,
                    "type is defined here",
                    range.cloc(self.file),
                    "type is used here"
                ));
            }

            return Ok(defn.sa);
        } else if pointer_count == 0 {
            return Err(error!(
                "reference incomplete type without pointer indirection",
                struct_type.decl_loc,
                "incomplete type declared here",
                range.cloc(self.file),
                "type used here"
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
        range: Range,
        member_ident: u32,
    ) -> Result<&TCStructMember, Error> {
        let struct_info = self.structs.get(&struct_ident).unwrap();
        let defn = if let Some(defn) = &struct_info.defn {
            defn
        } else {
            return Err(error!(
                "tried to get member of struct that's not defined",
                range, self.file, "member access here"
            ));
        };

        for member in defn.members {
            if member.ident == member_ident {
                return Ok(member);
            }
        }

        return Err(error!(
            "couldn't find member in struct definition",
            defn.loc,
            "struct defined here",
            range.cloc(self.file),
            "member accessed here"
        ));
    }
}

struct TypeEnvInterim<'b> {
    pub types: TypeEnv<'b>,
    pub func_types: HashMap<u32, TCFuncType<'b>>,
}

impl<'b> TypeEnvInterim<'b> {
    #[inline]
    pub fn check_type(
        &self,
        decl_idx: u32,
        ast_type: &ASTType,
        pointer_count: u32,
    ) -> Result<TCType, Error> {
        return self.types.check_type(decl_idx, ast_type, pointer_count);
    }

    #[inline]
    pub fn deref(&self, tc_type: &TCType, value_range: Range) -> Result<TCType, Error> {
        self.types.deref(tc_type, value_range)
    }

    #[inline]
    pub fn assign_convert<'a>(
        &self,
        buckets: BucketListRef<'a>,
        assign_to: &TCType,
        assign_to_loc: Option<CodeLoc>,
        expr: TCExpr<'a>,
    ) -> Result<TCExpr<'a>, Error> {
        return self
            .types
            .assign_convert(buckets, assign_to, assign_to_loc, expr);
    }

    #[inline]
    pub fn check_struct_member(
        &self,
        struct_ident: u32,
        decl_idx: u32,
        range: Range,
        member_ident: u32,
    ) -> Result<&TCStructMember, Error> {
        self.types
            .check_struct_member(struct_ident, decl_idx, range, member_ident)
    }
}

pub struct TypedFuncs<'a> {
    pub structs: TypeEnv<'a>,
    pub functions: HashMap<u32, TCFunc<'a>>,
}

pub fn check_file<'a>(
    buckets: BucketListRef<'a>,
    file: u32,
    stmts: &[GlobalStmt],
) -> Result<TypedFuncs<'a>, Error> {
    let mut types = TypeEnv::new(file);

    struct UncheckedStructDefn {
        defn_idx: u32,
        members: Vec<(u32, TCType, Range)>,
        range: Range,
    }

    struct UncheckedStruct {
        decl_idx: u32,
        decl_range: Range,
        defn: Option<UncheckedStructDefn>,
    }

    // Add all types to the type table
    let mut unchecked_types: HashMap<u32, UncheckedStruct> = HashMap::new();
    for (decl_idx, stmt) in stmts.iter().enumerate() {
        let decl_type = match &stmt.kind {
            GlobalStmtKind::StructDecl(decl_type) => decl_type,
            _ => continue,
        };

        let defn_idx = decl_idx as u32;
        let mut decl_range = decl_type.range;
        let mut decl_idx = decl_idx as u32;
        if let Some(original) = unchecked_types.get(&decl_type.ident) {
            match (&original.defn, &decl_type.members) {
                (Some(_), Some(_)) => {
                    return Err(error!(
                        "redefinition of struct",
                        original.decl_range.cloc(types.file),
                        "original definition here",
                        decl_type.range.cloc(types.file),
                        "second definition here"
                    ));
                }
                _ => {}
            }

            decl_idx = original.decl_idx;
            decl_range = original.decl_range;
        }

        let mut unchecked_struct = UncheckedStruct {
            decl_idx,
            decl_range,
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
                pointer_count: member.pointer_count,
            };

            semi_typed_members.push((member.ident, member_type, member.range));
            if let Some(original_range) = names.insert(member.ident, member.range) {
                return Err(error!(
                    "name redefined in struct",
                    original_range.cloc(types.file),
                    "first use of name here",
                    member.range.cloc(types.file),
                    "second use here"
                ));
            }
        }

        let struct_defn = UncheckedStructDefn {
            defn_idx,
            range: decl_type.range,
            members: semi_typed_members,
        };
        unchecked_struct.defn = Some(struct_defn);
        unchecked_types.insert(decl_type.ident, unchecked_struct);
    }

    // return type meaning is (defn_idx, defn_range, sa), where defn_idx is the
    // decl_idx if sa is not known, and same goes for defn_range
    fn check_type<'b>(
        buckets: BucketListRef<'b>,
        visited: &mut HashSet<u32>,
        types: &mut TypeEnv<'b>,
        unchecked_types: &HashMap<u32, UncheckedStruct>,
        current_ident: u32,
        type_decl: &UncheckedStruct,
    ) -> Result<(u32, Range, SizeAlign), Error> {
        if !visited.insert(current_ident) {
            if let Some(found) = types.structs.get(&current_ident) {
                if let Some(defn) = &found.defn {
                    return Ok((defn.defn_idx, defn.loc.range, defn.sa));
                }
                return Ok((found.decl_idx, found.decl_loc.range, TC_UNKNOWN_SA));
            } else {
                return Err(error!(
                    "struct heirarchy contains cycle",
                    type_decl.decl_range, types.file, "found cycle while solving this type"
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
                    decl_loc: type_decl.decl_range.cloc(types.file),
                    defn: None,
                },
            );

            return Ok((type_decl.decl_idx, type_decl.decl_range, TC_UNKNOWN_SA));
        };

        let mut size: u32 = 0;
        let mut align: u32 = 0;
        let mut typed_members = Vec::new();
        for (m_ident, m_type, m_range) in defn.members.iter() {
            let offset = size;
            let mut m_type = *m_type;

            // m prefix to mean member's size align (msa), tprefix to mean target (tsa)
            let m_sa = if let TCTypeKind::Struct { ident, sa: t_sa } = &mut m_type.kind {
                if let Some(m_type_decl) = unchecked_types.get(m_ident) {
                    if m_type.pointer_count == 0 {
                        let (m_defn_idx, m_defn_range, m_sa) = check_type(
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
                                *m_range, types.file, "struct here"
                            ));
                        }

                        if m_defn_idx > defn.defn_idx {
                            return Err(error!(
                                "struct is defined later in the file (order matters in C)",
                                m_defn_range.cloc(types.file),
                                "struct defined here",
                                m_range.cloc(types.file),
                                "struct referenced here"
                            ));
                        }

                        *t_sa = m_sa;
                        m_sa
                    } else if m_type_decl.decl_idx > type_decl.decl_idx {
                        return Err(error!(
                            "struct is declared later in the file (order matters in C)",
                            m_type_decl.decl_range.cloc(types.file),
                            "struct declared here",
                            m_range.cloc(types.file),
                            "struct referenced here"
                        ));
                    } else {
                        sa(8, 8)
                    }
                } else {
                    // TODO check imported structs
                    return Err(error!(
                        "struct does not not exist",
                        *m_range, types.file, "struct referenced here"
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
                loc: m_range.cloc(types.file),
                offset,
            });
        }

        let sa = sa(align_u32(size, align), align);
        let checked_defn = TCStructDefn {
            defn_idx: defn.defn_idx,
            loc: defn.range.cloc(types.file),
            sa,
            members: buckets.add_array(typed_members),
        };

        types.structs.insert(
            current_ident,
            TCStruct {
                decl_idx: type_decl.decl_idx,
                decl_loc: type_decl.decl_range.cloc(types.file),
                defn: Some(checked_defn),
            },
        );

        return Ok((defn.defn_idx, defn.range, sa));
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

    let mut env = TypeEnvInterim {
        types,
        func_types: HashMap::new(),
    };

    struct UncheckedFunction<'a> {
        defn_idx: u32,
        rtype_range: Range,
        range: Range,
        body: &'a [Stmt<'a>],
    }

    let mut unchecked_functions = HashMap::new();
    for (decl_idx, stmt) in stmts.iter().enumerate() {
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
        let rtype_range = rtype.range;
        let struct_env = &env.types;
        let return_type = struct_env.check_type(decl_idx, rtype, *rpointer_count)?;

        let mut names = HashMap::new();
        let mut typed_params = Vec::new();
        let mut varargs = None;
        for param in params.iter() {
            if let Some(range) = varargs {
                return Err(error!(
                    "function parameter after vararg",
                    range, file, "vararg indicator here", param.range, file, "parameter here"
                ));
            }

            let (decl_type, ppointer_count, ident) = match &param.kind {
                ParamKind::Vararg => {
                    varargs = Some(param.range);
                    continue;
                }
                ParamKind::StructLike {
                    decl_type,
                    pointer_count,
                    ident,
                } => (decl_type, *pointer_count, *ident),
            };

            if let Some(original) = names.insert(ident, param.range) {
                return Err(error!(
                    "redeclaration of function parameter",
                    original,
                    file,
                    "original declaration here",
                    param.range,
                    file,
                    "second declaration here"
                ));
            }

            let param_type = struct_env.check_type(decl_idx, decl_type, ppointer_count)?;

            typed_params.push(TCFuncParam {
                decl_type: param_type,
                ident: ident,
                range: param.range,
            });
        }

        let typed_params = buckets.add_array(typed_params);
        let tc_func_type = TCFuncType {
            return_type,
            loc: stmt.range.cloc(file),
            params: typed_params,
            decl_idx,
            varargs: varargs.is_some(),
        };

        if let Some(prev_tc_func_type) = env.func_types.get(ident) {
            if prev_tc_func_type != &tc_func_type {
                return Err(func_decl_mismatch(prev_tc_func_type.loc, tc_func_type.loc));
            }

            if let Some((ftype, Some(fbody))) = unchecked_functions.get(ident) {
                if let Some(body) = func_body {
                    return Err(func_redef(prev_tc_func_type.loc, tc_func_type.loc));
                }
            }
        } else {
            env.func_types.insert(*ident, tc_func_type);
        }

        if let Some(body) = func_body {
            let unchecked_func = UncheckedFunction {
                defn_idx: decl_idx,
                rtype_range,
                range: stmt.range,
                body,
            };
            unchecked_functions.insert(*ident, (tc_func_type, Some(unchecked_func)));
        } else {
            unchecked_functions.insert(*ident, (tc_func_type, None));
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

        let mut local_env = LocalTypeEnv::new(ftype.return_type, func.rtype_range);
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
                loc: param.range.cloc(file),
            };

            local_env.add_var(param.ident, tc_value).unwrap();
        }

        let gstmts = check_stmts(buckets, &mut env, &mut local_env, ftype.decl_idx, func.body)?;

        tc_func.defn = Some(TCFuncDefn {
            defn_idx: func.defn_idx,
            loc: func.range.cloc(file),
            stmts: buckets.add_array(gstmts),
        });

        functions.insert(func_name, tc_func);
    }

    return Ok(TypedFuncs {
        structs: env.types,
        functions,
    });
}

fn check_stmts<'b>(
    buckets: BucketListRef<'b>,
    env: &TypeEnvInterim<'b>,
    local_env: &mut LocalTypeEnv,
    decl_idx: u32,
    stmts: &[Stmt],
) -> Result<Vec<TCStmt<'b>>, Error> {
    let mut tstmts = Vec::new();
    for stmt in stmts {
        match &stmt.kind {
            StmtKind::RetVal(expr) => {
                let expr = check_expr(buckets, &env, &local_env, decl_idx, expr)?;
                let rtype = local_env.return_type;
                if rtype.pointer_count == 0
                    && rtype.kind == TCTypeKind::Void
                    && rtype != expr.expr_type
                {
                    return Err(error!(
                        "void function should not return a value",
                        expr.range, env.types.file, "value is here"
                    ));
                }

                let expr = env.assign_convert(
                    buckets,
                    &local_env.return_type,
                    Some(local_env.rtype_range.cloc(env.types.file)),
                    expr,
                )?;

                tstmts.push(TCStmt {
                    range: stmt.range,
                    kind: TCStmtKind::RetVal(expr),
                });
            }
            StmtKind::Ret => {
                let rtype = local_env.return_type;
                if rtype.pointer_count != 0 || rtype.kind != TCTypeKind::Void {
                    return Err(error!(
                        "expected value in return statement (return type is not void)",
                        local_env.rtype_range,
                        env.types.file,
                        "target type is here".to_string(),
                        stmt.range,
                        env.types.file,
                        "return statement is here".to_string()
                    ));
                }

                tstmts.push(TCStmt {
                    range: stmt.range,
                    kind: TCStmtKind::Ret,
                });
            }

            StmtKind::Expr(expr) => {
                let expr = check_expr(buckets, env, local_env, decl_idx, expr)?;
                tstmts.push(TCStmt {
                    range: expr.range,
                    kind: TCStmtKind::Expr(expr),
                });
            }

            StmtKind::Decl { decl_type, decls } => {
                for Decl {
                    pointer_count,
                    ident,
                    range,
                    expr,
                } in *decls
                {
                    let decl_type = env.check_type(decl_idx, decl_type, *pointer_count)?;
                    let expr = check_expr(buckets, env, local_env, decl_idx, &expr)?;
                    local_env.add_local(*ident, decl_type, range.cloc(env.types.file))?;
                    let expr = env.assign_convert(
                        buckets,
                        &decl_type,
                        Some(range.cloc(env.types.file)),
                        expr,
                    )?;
                    tstmts.push(TCStmt {
                        range: *range,
                        kind: TCStmtKind::Decl { init: expr },
                    });
                }
            }

            StmtKind::Nop => {}

            _ => panic!("unimplemented"),
        }
    }

    return Ok(tstmts);
}

fn check_expr<'b>(
    buckets: BucketListRef<'b>,
    env: &TypeEnvInterim<'b>,
    local_env: &LocalTypeEnv,
    decl_idx: u32,
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
                range: expr.range,
            });
        }
        ExprKind::IntLiteral(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::IntLiteral(val),
                expr_type: TCType {
                    kind: TCTypeKind::I32,
                    pointer_count: 0,
                },
                range: expr.range,
            });
        }
        ExprKind::StringLiteral(val) => {
            return Ok(TCExpr {
                kind: TCExprKind::StringLiteral(buckets.add_str(val)),
                expr_type: TCType {
                    kind: TCTypeKind::Char,
                    pointer_count: 1,
                },
                range: expr.range,
            });
        }
        ExprKind::Ident(id) => {
            let tc_var = match local_env.var(id) {
                Some(tc_var) => tc_var,
                None => {
                    return Err(error!(
                        "couldn't find name",
                        expr.range, env.types.file, "identifier here"
                    ));
                }
            };

            return Ok(TCExpr {
                kind: TCExprKind::LocalIdent {
                    var_offset: tc_var.var_offset,
                },
                expr_type: tc_var.decl_type,
                range: expr.range,
            });
        }

        ExprKind::BinOp(BinOp::Assign, target, value) => {
            let target = check_assign_target(buckets, env, local_env, decl_idx, target)?;
            let value = check_expr(buckets, env, local_env, decl_idx, value)?;

            let value =
                env.types
                    .assign_convert(buckets, &target.target_type, target.defn_loc, value)?;

            let value = buckets.add(value);

            return Ok(TCExpr {
                expr_type: target.target_type,
                range: expr.range,
                kind: TCExprKind::Assign { target, value },
            });
        }

        ExprKind::BinOp(op, l, r) => {
            let l = check_expr(buckets, env, local_env, decl_idx, l)?;
            let r = check_expr(buckets, env, local_env, decl_idx, r)?;

            let bin_op = get_overload(&env.types, op, &l, &r)?;
            return Ok(bin_op(&buckets, l, r));
        }

        ExprKind::Member { base, member } => {
            let base = check_expr(buckets, env, local_env, decl_idx, base)?;

            let struct_id = if let TCTypeKind::Struct { ident, .. } = base.expr_type.kind {
                ident
            } else {
                return Err(member_of_non_struct(base.range, env.types.file));
            };

            let member_info = env.check_struct_member(struct_id, decl_idx, base.range, member)?;

            return Ok(TCExpr {
                expr_type: member_info.decl_type,
                range: expr.range,
                kind: TCExprKind::Member {
                    base: buckets.add(base),
                    offset: member_info.offset,
                },
            });
        }
        ExprKind::PtrMember { base, member } => {
            let base = check_expr(buckets, env, local_env, decl_idx, base)?;

            let struct_id = if let TCTypeKind::Struct { ident, .. } = base.expr_type.kind {
                ident
            } else {
                return Err(member_of_non_struct(base.range, env.types.file));
            };

            let deref_type = env.deref(&base.expr_type, base.range)?;
            if deref_type.pointer_count != 0 {
                return Err(ptr_member_of_poly_pointer(
                    base.range,
                    env.types.file,
                    &deref_type,
                ));
            }

            let member_info = env.check_struct_member(struct_id, decl_idx, base.range, member)?;

            return Ok(TCExpr {
                expr_type: member_info.decl_type,
                range: expr.range,
                kind: TCExprKind::PtrMember {
                    base: buckets.add(base),
                    offset: member_info.offset,
                },
            });
        }

        ExprKind::Deref(ptr) => {
            let value = check_expr(buckets, env, local_env, decl_idx, ptr)?;

            let expr_type = env.deref(&value.expr_type, value.range)?;
            return Ok(TCExpr {
                expr_type,
                range: expr.range,
                kind: TCExprKind::Deref(buckets.add(value)),
            });
        }
        ExprKind::Ref(target) => {
            let target = check_assign_target(buckets, env, local_env, decl_idx, target)?;
            let mut expr_type = target.target_type;
            expr_type.pointer_count += 1;
            return Ok(TCExpr {
                expr_type,
                range: expr.range,
                kind: TCExprKind::Ref(target),
            });
        }

        ExprKind::Call { function, params } => {
            let func_id = if let ExprKind::Ident(id) = function.kind {
                id
            } else {
                return Err(error!(
                    "calling an expression that isn't a function",
                    function.range, env.types.file, "called here"
                ));
            };

            let func_type = if let Some(func_type) = env.func_types.get(&func_id) {
                func_type
            } else {
                return Err(error!(
                    "function doesn't exist",
                    expr.range, env.types.file, "called here"
                ));
            };

            if func_type.decl_idx > decl_idx {
                return Err(error!(
                    "function hasn't been declared yet (declaration order matters in C)",
                    expr.range,
                    env.types.file,
                    "function called here",
                    func_type.loc.range,
                    func_type.loc.file,
                    "function declared here"
                ));
            }

            if params.len() < func_type.params.len()
                || (params.len() > func_type.params.len() && !func_type.varargs)
            {
                return Err(error!(
                    "function call has wrong number of parameters",
                    expr.range,
                    env.types.file,
                    "function called here",
                    func_type.loc.range,
                    func_type.loc.file,
                    "function declared here"
                ));
            }

            let mut tparams = Vec::new();
            for (idx, param) in params.iter().enumerate() {
                let mut expr = check_expr(buckets, &env, &local_env, decl_idx, param)?;
                if idx < func_type.params.len() {
                    let param_type = &func_type.params[idx];
                    expr = env.assign_convert(
                        buckets,
                        &param_type.decl_type,
                        Some(func_type.loc),
                        expr,
                    )?;
                }

                tparams.push(expr);
            }

            return Ok(TCExpr {
                kind: TCExprKind::Call {
                    func: func_id,
                    params: buckets.add_array(tparams),
                    varargs: func_type.varargs,
                },
                expr_type: func_type.return_type,
                range: expr.range,
            });
        }
        _ => panic!("unimplemented"),
    }
}

fn check_assign_target<'b>(
    buckets: BucketListRef<'b>,
    env: &TypeEnvInterim<'b>,
    local_env: &LocalTypeEnv,
    decl_idx: u32,
    expr: &Expr,
) -> Result<TCAssignTarget<'b>, Error> {
    match &expr.kind {
        ExprKind::Ident(id) => {
            let tc_var = match local_env.var(*id) {
                Some(tc_var) => tc_var,
                None => {
                    return Err(ident_not_found(&env.types, expr.range));
                }
            };

            let kind = TCAssignKind::LocalIdent {
                var_offset: tc_var.var_offset,
            };

            return Ok(TCAssignTarget {
                kind,
                defn_loc: Some(tc_var.loc),
                target_range: expr.range,
                target_type: tc_var.decl_type,
                offset: 0,
            });
        }

        ExprKind::Member { base, member } => {
            let base_range = base.range;
            let base = check_assign_target(buckets, env, local_env, decl_idx, base)?;

            let struct_id = if let TCTypeKind::Struct { ident, .. } = base.target_type.kind {
                ident
            } else {
                return Err(member_of_non_struct(base.target_range, env.types.file));
            };

            let member_info =
                env.check_struct_member(struct_id, decl_idx, base.target_range, *member)?;

            return Ok(TCAssignTarget {
                kind: base.kind,
                defn_loc: Some(member_info.loc),
                target_range: expr.range,
                target_type: member_info.decl_type,
                offset: member_info.offset,
            });
        }
        ExprKind::PtrMember { base, member } => {
            let base_range = base.range;
            let base = check_expr(buckets, env, local_env, decl_idx, base)?;

            let struct_id = if let TCTypeKind::Struct { ident, .. } = base.expr_type.kind {
                ident
            } else {
                return Err(member_of_non_struct(base.range, env.types.file));
            };

            let deref_type = env.deref(&base.expr_type, base.range)?;
            if deref_type.pointer_count != 0 {
                return Err(ptr_member_of_poly_pointer(
                    base.range,
                    env.types.file,
                    &deref_type,
                ));
            }

            let member_info = env.check_struct_member(struct_id, decl_idx, base.range, *member)?;

            return Ok(TCAssignTarget {
                kind: TCAssignKind::Ptr(buckets.add(base)),
                defn_loc: Some(member_info.loc),
                target_range: expr.range,
                target_type: member_info.decl_type,
                offset: member_info.offset,
            });
        }

        ExprKind::Deref(ptr) => {
            let ptr = check_expr(buckets, env, local_env, decl_idx, ptr)?;

            let target_type = env.deref(&ptr.expr_type, ptr.range)?;
            return Ok(TCAssignTarget {
                kind: TCAssignKind::Ptr(buckets.add(ptr)),
                target_range: expr.range,
                defn_loc: None,
                target_type,
                offset: 0,
            });
        }
        _ => {
            return Err(error!(
                "expression is not assignable",
                expr.range, env.types.file, "expression found here"
            ))
        }
    }
}

pub fn dereference_of_non_pointer(value_range: Range, file: u32, value_type: &TCType) -> Error {
    return error!(
        "cannot dereference values that aren't pointers",
        value_range,
        file,
        format!(
            "value has type {:?}, which cannot be dereferenced",
            value_type
        )
    );
}

pub fn ptr_member_of_poly_pointer(ptr_range: Range, file: u32, ptr_type: &TCType) -> Error {
    error!(
        "need to dereference pointer before you can access its members",
        ptr_range.cloc(file),
        format!(
            "this points to an object of type {:?}, which doesn't have any members",
            ptr_type
        )
    )
}

pub fn member_of_non_struct(range: Range, file: u32) -> Error {
    error!(
        "cannot access member of non-struct",
        range, file, "access happened here"
    )
}

pub fn ident_not_found(env: &TypeEnv, range: Range) -> Error {
    return error!("couldn't find name", range, env.file, "identifier here");
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
