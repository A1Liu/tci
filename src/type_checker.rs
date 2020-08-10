use crate::ast::*;
use crate::ast_typed::*;
use crate::buckets::{BucketList, BucketListRef};
use crate::errors::Error;
use crate::lexer::Token;
use crate::*;
use std::collections::HashMap;

pub struct TypeEnv<'b> {
    pub _buckets: BucketListRef<'b>,
    pub struct_types: HashMap<u32, TCStruct<'b>>,
    pub symbols: HashMap<u32, TCGlobalValue>,
    pub func_types: HashMap<u32, TCFunc<'b>>,
}

pub struct LocalTypeEnv {
    pub symbols: HashMap<u32, TCValue>,
    pub return_type: TCType,
    pub parent: *const LocalTypeEnv,
}

impl LocalTypeEnv {
    pub fn new(return_type: TCType) -> Self {
        Self {
            symbols: HashMap::new(),
            return_type,
            parent: core::ptr::null(),
        }
    }

    pub fn child(&self) -> Self {
        if self.symbols.is_empty() {
            // for the case of chained if-else
            Self {
                symbols: HashMap::new(),
                return_type: self.return_type.clone(),
                parent: self.parent,
            }
        } else {
            Self {
                symbols: HashMap::new(),
                return_type: self.return_type.clone(),
                parent: self,
            }
        }
    }

    pub fn var(&self, id: u32) -> Option<&TCValue> {
        if let Some(var_type) = self.symbols.get(&id) {
            return Some(var_type);
        }

        if self.parent.is_null() {
            return None;
        }

        return unsafe { &*self.parent }.var(id);
    }

    pub fn add_var(&mut self, decl_type: &ASTType, decl: &Decl) -> Result<(), Error> {
        let tc_value = TCValue {
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
    pub env: TypeEnv<'b>,
    pub functions: HashMap<u32, &'b [Token]>,
    pub values: HashMap<u32, &'b [Token]>,
}

impl<'b> TypeChecker<'b> {
    pub fn new() -> Self {
        Self {
            env: TypeEnv {
                _buckets: BucketList::new(),
                struct_types: HashMap::new(),
                symbols: HashMap::new(),
                func_types: HashMap::new(),
            },
            functions: HashMap::new(),
            values: HashMap::new(),
        }
    }

    pub fn check_global_stmts(&mut self, stmts: &[GlobalStmt]) -> Result<(), Error> {
        // Add all types to the type table
        for (decl_idx, stmt) in stmts.iter().enumerate() {
            let decl_type = match &stmt.kind {
                GlobalStmtKind::StructDecl(decl_type) => decl_type,
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

        let mut values = HashMap::new();
        for (decl_idx, stmt) in stmts.iter().enumerate() {
            let (decl_type, decls) = match &stmt.kind {
                GlobalStmtKind::Decl { decl_type, decls } => (decl_type, decls),
                _ => continue,
            };

            for decl in decls.iter() {
                let decl_idx = decl_idx as u32;
                let decl_type = convert_type(decl_type, decl.pointer_count);
                if let TCTypeKind::Struct { ident } = decl_type.kind {
                    self.check_struct_type(ident, decl_idx, decl_type.pointer_count, decl.range)?;
                }

                let value_type = TCGlobalValue {
                    decl_type,
                    decl_idx,
                    range: decl.range,
                };

                if let Some(original_value_type) = self.env.symbols.insert(decl.ident, value_type) {
                    return Err(Error::variable_redefinition(
                        &original_value_type.range,
                        &decl.range,
                    ));
                }

                values.insert(decl.ident, &decl.expr);
            }
        }

        let mut functions = HashMap::new();
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
                _ => continue,
            };

            let decl_idx = decl_idx as u32;
            let type_range = return_type.range;
            let return_type = convert_type(return_type, *pointer_count);
            if let TCTypeKind::Struct { ident } = return_type.kind {
                self.check_struct_type(ident, decl_idx, *pointer_count, type_range)?;
            }

            let mut names = HashMap::new();
            let mut typed_params = Vec::new();
            for param in params.iter() {
                if let Some(original_range) = names.get(&param.ident) {
                    return Err(Error::parameter_redeclaration(original_range, &param.range));
                } else {
                    names.insert(param.ident, param.range.clone());
                }

                let param_type = convert_type(&param.decl_type, *pointer_count);
                if let TCTypeKind::Struct { ident } = return_type.kind {
                    self.check_struct_type(
                        ident,
                        decl_idx,
                        return_type.pointer_count,
                        type_range.clone(),
                    )?;
                }

                typed_params.push(TCFuncParam {
                    decl_type: param_type,
                    ident: param.ident,
                    range: param.range.clone(),
                });
            }

            let typed_params = self.env._buckets.add_array(typed_params);
            let tc_func_type = TCFunc {
                return_type,
                range: stmt.range.clone(),
                params: typed_params,
                decl_idx,
            };

            if let Some(prev_tc_func_type) = self.env.func_types.get(&ident) {
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
            }

            self.env.func_types.insert(*ident, tc_func_type);
            if let Some(body) = func_body {
                functions.insert(*ident, body);
            }
        }

        return Ok(());
    }

    pub fn check_stmts(
        &self,
        decl_idx: u32,
        env: &mut LocalTypeEnv,
        stmts: &[Stmt],
    ) -> Result<(), Error> {
        for stmt in stmts {
            match &stmt.kind {
                StmtKind::ForDecl {
                    at_start_decl_type,
                    at_start,
                    condition,
                    post_expr,
                    body,
                } => {
                    let mut local_env = env.child();
                    for decl in at_start.iter() {
                        local_env.add_var(at_start_decl_type, decl)?;
                    }

                    Error::truth_value_of_struct(
                        condition,
                        &self.check_expr(&local_env, condition)?,
                    )?;

                    self.check_expr(&local_env, post_expr)?;
                    self.check_stmts(decl_idx, &mut local_env, body)?;
                }
                StmtKind::For {
                    at_start,
                    condition,
                    post_expr,
                    body,
                } => {
                    let mut local_env = env.child();
                    self.check_expr(&local_env, at_start)?;
                    Error::truth_value_of_struct(
                        condition,
                        &self.check_expr(&local_env, condition)?,
                    )?;

                    if let TCType {
                        kind: TCTypeKind::Struct { .. },
                        ..
                    } = self.check_expr(&local_env, condition)?
                    {}

                    self.check_expr(&local_env, post_expr)?;
                    self.check_stmts(decl_idx, &mut local_env, body)?;
                }

                StmtKind::Ret => {}

                StmtKind::Branch {
                    if_cond,
                    if_body,
                    else_body,
                } => {
                    Error::truth_value_of_struct(if_cond, &self.check_expr(env, if_cond)?)?;

                    let mut local_env = env.child();
                    self.check_stmts(decl_idx, &mut local_env, if_body)?;

                    let mut local_env = env.child();
                    if let Some(else_body) = else_body {
                        self.check_stmts(decl_idx, &mut local_env, else_body)?;
                    }
                }

                StmtKind::Expr(expr) => {
                    self.check_expr(&env, expr)?;
                }
                StmtKind::Block(block) => {
                    let mut local_env = env.child();
                    self.check_stmts(decl_idx, &mut local_env, block)?;
                }
                StmtKind::Nop => {}

                _ => panic!("unimplemented"),
            }
        }

        return Ok(());
    }

    pub fn check_expr(&self, env: &LocalTypeEnv, expr: &Expr) -> Result<TCType, Error> {
        return Err(Error::new("", vec![]));
    }

    pub fn check_struct_type(
        &self,
        struct_ident: u32,
        decl_idx: u32,
        pointer_count: u32,
        range: Range,
    ) -> Result<(), Error> {
        if let Some(struct_type) = self.env.struct_types.get(&struct_ident) {
            if let Some((type_defn_idx, _)) = struct_type.defn {
                if pointer_count == 0 && type_defn_idx > decl_idx {
                    return Err(Error::struct_misordered_type(struct_type, &range));
                }
            } else if pointer_count == 0 {
                return Err(Error::struct_incomplete_type(struct_type, &range));
            }
        } else {
            return Err(Error::struct_doesnt_exist(&range));
        }

        return Ok(());
    }
}
