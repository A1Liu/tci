use crate::ast::*;
use crate::buckets::{BucketList, BucketListRef};
use crate::errors::Error;
use crate::*;
use std::collections::HashMap;

macro_rules! err {
    ($msg:expr) => {
        Err(Error::new($msg, vec![]))
    };

    ($msg:expr, $range1:expr, $msg1:expr) => {
        Err(Error::new($msg, vec![($range1, $msg1.to_string())]))
    };
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

    pub fn add_var(&mut self, decl_type: &ASTType, decl: &Decl) -> Result<(), Error> {
        let tc_value = TCVar {
            decl_type: convert_type(decl_type, decl.pointer_count),
            range: decl.range,
        };
        if let Some(var_type) = self.symbols.insert(decl.ident, tc_value) {
            return Err(Error::variable_redefinition(&var_type.range, &decl.range));
        }

        return Ok(());
    }
}

pub struct TypeChecker<'b> {
    pub _buckets: BucketListRef<'b>,
    pub func_types: HashMap<u32, TCFuncType<'b>>,
    pub functions: HashMap<u32, TCFunc<'b>>,
}

impl<'b> TypeChecker<'b> {
    pub fn new() -> Self {
        Self {
            _buckets: BucketList::new(),
            func_types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn check_global_stmts(&mut self, stmts: &[GlobalStmt]) -> Result<(), Error> {
        struct UncheckedFunction<'a, 'b> {
            defn_idx: u32,
            ident: u32,
            func_type: TCFuncType<'b>,
            return_type_range: Range,
            range: Range,
            body: &'a [Stmt<'a>],
        };

        let mut functions: Vec<UncheckedFunction> = Vec::new();
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
            let return_type = convert_type(return_type, *pointer_count);
            if let TCTypeKind::Struct { ident } = return_type.kind {
                self.check_struct_type(ident, decl_idx, *pointer_count, return_type_range)?;
            }

            let mut names = HashMap::new();
            let mut typed_params = Vec::new();
            for param in params.iter() {
                if let Some(original) = names.insert(param.ident, param.range.clone()) {
                    return Err(Error::parameter_redeclaration(&original, &param.range));
                }

                let param_type = self.check_type(decl_idx, &param.decl_type, *pointer_count)?;

                typed_params.push(TCFuncParam {
                    decl_type: param_type,
                    ident: param.ident,
                    range: param.range,
                });
            }

            let typed_params = self._buckets.add_array(typed_params);
            let tc_func_type = TCFuncType {
                return_type,
                range: stmt.range,
                params: typed_params,
                decl_idx,
            };

            if let Some(prev_tc_func_type) = self.func_types.get(ident) {
                if prev_tc_func_type != &tc_func_type {
                    return Err(Error::function_declaration_mismatch(
                        &prev_tc_func_type.range,
                        &tc_func_type.range,
                    ));
                }

                if let Some(body) = self.functions.get(&ident) {
                    if let Some(body) = func_body {
                        return Err(Error::function_redefinition(
                            &prev_tc_func_type.range,
                            &tc_func_type.range,
                        ));
                    }
                }
            } else {
                self.func_types.insert(*ident, tc_func_type);
            }

            if let Some(body) = func_body {
                functions.push(UncheckedFunction {
                    defn_idx: decl_idx,
                    func_type: tc_func_type,
                    ident: *ident,
                    return_type_range,
                    range: stmt.range,
                    body,
                });
            }
        }

        for func in functions.into_iter() {
            let ftype = func.func_type;
            let mut env = LocalTypeEnv::new(ftype.return_type, func.return_type_range);
            let gstmts = self.check_stmts(ftype.decl_idx, &mut env, func.body)?;
            self.functions.insert(
                func.ident,
                TCFunc {
                    func_type: ftype,
                    defn_idx: func.defn_idx,
                    range: func.range,
                    stmts: gstmts,
                },
            );
        }

        return Ok(());
    }

    pub fn check_stmts(
        &self,
        decl_idx: u32,
        env: &mut LocalTypeEnv,
        stmts: &[Stmt],
    ) -> Result<&'b [TCStmt<'b>], Error> {
        let mut tstmts = Vec::new();
        for stmt in stmts {
            match &stmt.kind {
                StmtKind::RetVal(expr) => {
                    let expr = self.check_expr(&env, expr)?;
                    tstmts.push(TCStmt {
                        range: expr.range,
                        kind: TCStmtKind::RetVal(expr),
                    });
                }

                StmtKind::Nop => {}

                _ => panic!("unimplemented"),
            }
        }

        return Ok(self._buckets.add_array(tstmts));
    }

    pub fn check_expr(&self, env: &LocalTypeEnv, expr: &Expr) -> Result<TCExpr<'b>, Error> {
        match expr.kind {
            ExprKind::IntLiteral(val) => {
                return Ok(TCExpr {
                    kind: TCExprKind::IntLiteral(val),
                    expr_type: TCType {
                        kind: TCTypeKind::Int,
                        pointer_count: 0,
                    },
                    range: expr.range,
                });
            }
            // ExprKind::Add(l, r) => {
            //     let l = self.check_expr(env, l)?;
            //     let r = self.check_expr(env, r)?;

            //     return Ok();
            // }
            _ => panic!("unimplemented"),
        }
    }

    // pub fn binary_op(l: TCExpr<'b>, r: TCExpr<'b>) -> Result<(TCExpr<'b>, TCExpr<'b>), Error> {
    //     if l.expr_type == r.expr_type {
    //         return Ok((l, r));
    //     }
    // }

    pub fn check_type(
        &self,
        decl_idx: u32,
        ast_type: &ASTType,
        pointer_count: u32,
    ) -> Result<TCType, Error> {
        let unchecked_type = convert_type(&ast_type, pointer_count);
        if let TCTypeKind::Struct { ident } = unchecked_type.kind {
            self.check_struct_type(ident, decl_idx, pointer_count, ast_type.range)?;
        }

        return Ok(unchecked_type);
    }

    pub fn check_struct_type(
        &self,
        struct_ident: u32,
        decl_idx: u32,
        pointer_count: u32,
        range: Range,
    ) -> Result<(), Error> {
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

        return Ok(());
    }
}

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

            tc_struct.defn = Some((defn_idx, self.env._buckets.add_array(typed_members)));
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
