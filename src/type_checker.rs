use crate::ast::*;
use crate::buckets::{BucketList, BucketListRef};
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

lazy_static! {
    pub static ref BIN_OP_OVERLOADS: HashMap<(BinOp, TCShallowType, TCShallowType), BinOpTransform> = {
        use TCShallowType::*;
        let mut m: HashMap<(BinOp, TCShallowType, TCShallowType), BinOpTransform> = HashMap::new();
        m.insert((BinOp::Add, I32, I32), add_int_int);
        m.insert((BinOp::Add, I32, Char), add_int_char);
        m.insert((BinOp::Add, Char, I32), add_char_int);
        m
    };
    pub static ref BIN_LEFT_OVERLOADS: HashSet<(BinOp, TCShallowType)> = {
        use TCShallowType::*;
        let mut m = HashSet::new();
        m.insert((BinOp::Add, I32));
        m.insert((BinOp::Add, Char));
        m
    };
    pub static ref BIN_RIGHT_OVERLOADS: HashSet<(BinOp, TCShallowType)> = {
        use TCShallowType::*;
        let mut m = HashSet::new();
        m.insert((BinOp::Add, I32));
        m.insert((BinOp::Add, Char));
        m
    };
}

fn get_overload(
    env: &StructEnv,
    op: BinOp,
    l: &TCExpr,
    r: &TCExpr,
) -> Result<BinOpTransform, Error> {
    let key = (op, l.expr_type.to_shallow(), r.expr_type.to_shallow());
    match BIN_OP_OVERLOADS.get(&key) {
        Some(bin_op) => return Ok(*bin_op),
        None => return Err(invalid_operands_bin_expr(env, op, l, r)),
    }
}

pub fn invalid_operands_bin_expr(env: &StructEnv, op: BinOp, l: &TCExpr, r: &TCExpr) -> Error {
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
    pub return_type_range: Range,
    pub parent: *const LocalTypeEnv,
}

impl LocalTypeEnv {
    pub fn new(return_type: TCType, return_type_range: Range) -> Self {
        Self {
            symbols: HashMap::new(),
            return_type,
            return_type_range,
            parent: core::ptr::null(),
        }
    }

    pub fn child(&self) -> Self {
        if self.symbols.is_empty() {
            // for the case of chained if-else
            Self {
                symbols: HashMap::new(),
                return_type: self.return_type.clone(),
                return_type_range: self.return_type_range,
                parent: self.parent,
            }
        } else {
            Self {
                symbols: HashMap::new(),
                return_type: self.return_type.clone(),
                return_type_range: self.return_type_range,
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

    // pub fn add_var(&mut self, decl_type: &ASTType, decl: &Decl) -> Result<(), Error> {
    //     let tc_value = TCVar {
    //         decl_type: convert_type(decl_type, decl.pointer_count),
    //         range: decl.range,
    //     };
    //     if let Some(var_type) = self.symbols.insert(decl.ident, tc_value) {
    //         return Err(error!(
    //             "name redefined in struct",
    //             &var_type.range,
    //             self.file_id,
    //             "first_definition here" & decl.range
    //         ));
    //     }

    //     return Ok(());
    // }
}

pub struct StructEnv {
    file: u32,
}

impl StructEnv {
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
            &ASTTypeKind::Struct { ident } => TCTypeKind::Struct { ident, size: 0 },
        };

        let mut unchecked_type = TCType {
            kind,
            pointer_count,
        };

        if let TCTypeKind::Struct { ident, size } = &mut unchecked_type.kind {
            *size = self.check_struct_type(*ident, decl_idx, pointer_count, ast_type.range)?;
        }

        return Ok(unchecked_type);
    }

    pub fn check_struct_type(
        &self,
        struct_ident: u32,
        decl_idx: u32,
        pointer_count: u32,
        range: Range,
    ) -> Result<u32, Error> {
        // if let Some(struct_type) = self.struct_types.get(&struct_ident) {
        //     if let Some((type_defn_idx, _)) = struct_type.defn {
        //         if pointer_count == 0 && type_defn_idx > decl_idx {
        //             return Err(Error::struct_misordered_type(struct_type, &range));
        //         }
        //     } else if pointer_count == 0 {
        //         return Err(Error::struct_incomplete_type(struct_type, &range));
        //     }
        // } else {
        //     return Err(Error::struct_doesnt_exist(&range));
        // }

        return Ok(0);
    }
}

// internal
pub struct UncheckedFunction<'a> {
    defn_idx: u32,
    return_type_range: Range,
    range: Range,
    body: &'a [Stmt<'a>],
}

// internal
pub struct TypeEnvInterim<'b> {
    pub structs: StructEnv,
    pub func_types: HashMap<u32, TCFuncType<'b>>,
}

pub struct TypeEnv<'a> {
    pub structs: StructEnv,
    pub functions: HashMap<u32, TCFunc<'a>>,
}

pub fn check_types<'a>(
    buckets: BucketListRef<'a>,
    file: u32,
    stmts: &[GlobalStmt],
) -> Result<TypeEnv<'a>, Error> {
    let struct_env = StructEnv { file };

    let mut env = TypeEnvInterim {
        structs: struct_env,
        func_types: HashMap::new(),
    };

    let mut unchecked_functions = HashMap::new();
    for (decl_idx, stmt) in stmts.iter().enumerate() {
        let (return_type, pointer_count, ident, params, func_body) = match &stmt.kind {
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
            _ => panic!("unimplemented"),
        };

        let decl_idx = decl_idx as u32;
        let return_type_range = return_type.range;
        let struct_env = &env.structs;
        let return_type = struct_env.check_type(decl_idx, return_type, *pointer_count)?;

        let mut names = HashMap::new();
        let mut typed_params = Vec::new();
        for param in params.iter() {
            if let Some(original) = names.insert(param.ident, param.range.clone()) {
                return Err(param_redeclaration(struct_env.file, original, param.range));
            }

            let param_type = struct_env.check_type(decl_idx, &param.decl_type, *pointer_count)?;

            typed_params.push(TCFuncParam {
                decl_type: param_type,
                ident: param.ident,
                range: param.range,
            });
        }

        let typed_params = buckets.add_array(typed_params);
        let tc_func_type = TCFuncType {
            return_type,
            loc: stmt.range.cloc(env.structs.file),
            params: typed_params,
            decl_idx,
        };

        if let Some(prev_tc_func_type) = env.func_types.get(ident) {
            if prev_tc_func_type != &tc_func_type {
                return Err(func_decl_mismatch(prev_tc_func_type.loc, tc_func_type.loc));
            }

            if let Some(body) = unchecked_functions.get(ident) {
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
                return_type_range,
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

        let mut local_env = LocalTypeEnv::new(ftype.return_type, func.return_type_range);
        let gstmts = check_stmts(buckets, &mut env, &mut local_env, ftype.decl_idx, func.body)?;

        tc_func.defn = Some(TCFuncDefn {
            defn_idx: func.defn_idx,
            loc: func.range.cloc(env.structs.file),
            stmts: buckets.add_array(gstmts),
        });

        functions.insert(func_name, tc_func);
    }

    let env = TypeEnv {
        structs: env.structs,
        functions,
    };
    return Ok(env);
}

pub fn check_stmts<'b>(
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
                let expr = check_expr(buckets, &env, &local_env, expr)?;
                tstmts.push(TCStmt {
                    range: expr.range,
                    kind: TCStmtKind::RetVal(expr),
                });
            }

            StmtKind::Nop => {}

            _ => panic!("unimplemented"),
        }
    }

    return Ok(tstmts);
}

pub fn check_expr<'b>(
    buckets: BucketListRef<'b>,
    env: &TypeEnvInterim<'b>,
    local_env: &LocalTypeEnv,
    expr: &Expr,
) -> Result<TCExpr<'b>, Error> {
    match expr.kind {
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
        ExprKind::BinOp(BinOp::Add, l, r) => {
            let l = check_expr(buckets, env, local_env, l)?;
            let r = check_expr(buckets, env, local_env, r)?;

            let bin_op = get_overload(&env.structs, BinOp::Add, &l, &r)?;
            return Ok(bin_op(&buckets, l, r));
        }
        _ => panic!("unimplemented"),
    }
}

pub fn param_redeclaration(file: u32, original_range: Range, range: Range) -> Error {
    return error!(
        "redeclaration of function parameter",
        original_range, file, "original declaration here", range, file, "second declaration here"
    );
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

// pub struct TypeChecker<'b> {
//     pub buckets: BucketListRef<'b>,
//     pub func_types: HashMap<u32, TCFuncType<'b>>,
//     pub functions: HashMap<u32, TCFunc<'b>>,
// }
//
// impl<'b> TypeChecker<'b> {
//     #[inline]
//     pub fn type_width(&self, tc_type: &TCType) -> u32 {
//         if tc_type.pointer_count > 0 {
//             return 8;
//         }
//
//         match tc_type.kind {
//             TCTypeKind::I32 => 4,
//             TCTypeKind::U64 => 8,
//             TCTypeKind::Char => 1,
//             TCTypeKind::Void => 0,
//             TCTypeKind::Struct { ident } => panic!("unimplemented"),
//         }
//     }
//
//     #[inline]
//     pub fn type_is_numeric(&self, tc_type: &TCType) -> bool {
//         if tc_type.pointer_count > 0 {
//             return true;
//         }
//
//         match tc_type.kind {
//             TCTypeKind::I32 | TCTypeKind::U64 | TCTypeKind::Char => true,
//             TCTypeKind::Void | TCTypeKind::Struct { .. } => false,
//         }
//     }
// }

/*
        // Add all types to the type table
        for (decl_idx, stmt) in stmts.iter().enumerate() {
            let decl_type = match &stmt.kind {
                GlobalStmtKind::StructDecl(decl_type) => decl_type,
                GlobalStmtKind::Decl { decl_type, decls } => {
                    return err!(
                        "global declarations not supported yet",
                        r_from(decl_type.range, decls.last().unwrap().range),
                        "declaration here"
                    )
                }
                _ => continue,
            };

            let defn_idx = decl_idx as u32;
            let mut decl_idx = decl_idx as u32;
            if let Some(original) = self.env.struct_types.get(&decl_type.ident) {
                Error::struct_redefinition(original, decl_type)?;
                decl_idx = original.decl_idx;
            }

            let mut tc_struct = TCStruct {
                decl_idx,
                ident_range: decl_type.ident_range,
                range: decl_type.range,
                defn: None,
            };

            let members = match decl_type.members {
                None => {
                    self.env.struct_types.insert(decl_type.ident, tc_struct);
                    continue;
                }
                Some(members) => members,
            };

            let mut typed_members = Vec::new();
            let mut names = HashMap::new();

            for member in members {
                if let Some(original_range) = names.get(&member.ident) {
                    return Err(Error::struct_member_redefinition(
                        original_range,
                        &member.range,
                    ));
                } else {
                    names.insert(member.ident, member.range);
                }

                let member_type = convert_type(&member.decl_type, member.pointer_count);
                typed_members.push(TCStructMember {
                    decl_type: member_type,
                    ident: member.ident,
                    range: member.range,
                });
            }

            tc_struct.defn = Some((defn_idx, self.env.buckets.add_array(typed_members)));
            self.env.struct_types.insert(decl_type.ident, tc_struct);
        }

        // Check all types are valid
        for (current_struct, type_defn) in self.env.struct_types.iter() {
            if let Some((defn_idx, members)) = type_defn.defn {
                let filter_map_struct_idents = |member: &TCStructMember| {
                    if let TCTypeKind::Struct { ident } = member.decl_type.kind {
                        Some((ident, member.range, member.decl_type.pointer_count))
                    } else {
                        None
                    }
                };

                let inner_struct_members = members.iter().filter_map(filter_map_struct_idents);
                for (member_struct, range, pointer_count) in inner_struct_members {
                    if *current_struct == member_struct && pointer_count == 0 {
                        return Err(Error::new(
                            "recursive struct",
                            vec![
                                (type_defn.ident_range, "struct here".to_string()),
                                (range, "recursive member here".to_string()),
                            ],
                        ));
                    }

                    self.check_struct_type(member_struct, defn_idx, pointer_count, range)?;
                }
            }
        }


*/
