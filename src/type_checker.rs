use crate::ast::*;
use crate::buckets::BucketList;
use crate::errors::Error;
use crate::lexer::Token;
use core::ops::Range;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TCStructMember {
    decl_type: TCType,
    ident: u32,
    range: Range<u32>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TCTypeKind {
    Int,
    Char,
    Void,
    Struct { ident: u32 },
}

#[derive(Debug)]
pub struct TCStruct<'a> {
    pub decl_idx: u32,
    pub defn: Option<(u32, &'a [TCStructMember])>,
    pub ident_range: Range<u32>,
    pub range: Range<u32>,
}

#[derive(Debug, Clone, Copy)]
pub struct TCType {
    kind: TCTypeKind,
    pointer_count: u32,
}

#[derive(Debug, Clone)]
pub struct TCValue {
    decl_type: TCType,
    range: Range<u32>,
    decl_idx: u32,
}

#[derive(Debug, Clone)]
pub struct TCFuncParam {
    decl_type: TCType,
    ident: u32,
    range: Range<u32>,
}

#[derive(Debug, Clone)]
pub struct TCFunc<'a> {
    return_type: TCType,
    params: &'a [TCFuncParam],
    range: Range<u32>,
    decl_idx: u32,
}

impl PartialEq for TCFuncParam {
    fn eq(&self, other: &Self) -> bool {
        return self.decl_type == other.decl_type;
    }
}

impl PartialEq for TCType {
    fn eq(&self, other: &Self) -> bool {
        return self.kind == other.kind && self.pointer_count == other.pointer_count;
    }
}

impl<'a> PartialEq for TCFunc<'a> {
    fn eq(&self, other: &Self) -> bool {
        return self.return_type == other.return_type && self.params == other.params;
    }
}

pub struct TypeEnv<'a, 'b> {
    pub _buckets: &'a mut BucketList<'b>,
    pub struct_types: HashMap<u32, TCStruct<'b>>,
    pub symbols: HashMap<u32, TCValue>,
    pub func_types: HashMap<u32, TCFunc<'b>>,
}

pub struct TypeChecker1<'a, 'b> {
    pub env: TypeEnv<'a, 'b>,
    pub functions: HashMap<u32, &'b [Token]>,
    pub values: HashMap<u32, &'b [Token]>,
}

impl<'a, 'b> TypeChecker1<'a, 'b> {
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
                ident_range: decl_type.ident_range.clone(),
                range: decl_type.range.clone(),
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
                    names.insert(member.ident, member.range.clone());
                }

                let member_type = self.convert_type(&member.decl_type, member.pointer_count);
                typed_members.push(TCStructMember {
                    decl_type: member_type,
                    ident: member.ident,
                    range: member.range.clone(),
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
                        Some((ident, member.range.clone(), member.decl_type.pointer_count))
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
                                (type_defn.ident_range.clone(), "struct here".to_string()),
                                (range.clone(), "recursive member here".to_string()),
                            ],
                        ));
                    }

                    self.check_struct_type(member_struct, defn_idx, pointer_count, range)?;
                }
            }
        }

        // for (decl_idx, stmt) in stmts.iter().enumerate() {
        //     let (decl_type, ident, range, value) = match &stmt.kind {
        //         GlobalStmtKind::Decl(decl) => match &decl.kind {
        //             DeclKind::Uninit { decl_type, ident } => {
        //                 (decl_type, ident, decl.range.clone(), None)
        //             }
        //             DeclKind::WithValue {
        //                 decl_type,
        //                 ident,
        //                 value,
        //             } => (decl_type, ident, decl.range.clone(), Some(*value)),
        //         },
        //         _ => continue,
        //     };

        //     let decl_idx = decl_idx as u32;
        //     let decl_type = self.convert_type(decl_type);
        //     if let TCTypeKind::Struct { ident } = decl_type.kind {
        //         self.check_struct_type(ident, decl_idx, decl_type.pointer_count, range.clone())?;
        //     }

        //     let value_type = TCValue {
        //         decl_type,
        //         decl_idx,
        //         range: range.clone(),
        //     };

        //     if let Some(original_value_type) = self.env.symbols.insert(*ident, value_type) {
        //         return Err(Error::variable_redefinition(
        //             &original_value_type.range,
        //             &range,
        //         ));
        //     }

        //     if let Some(value) = value {
        //         self.values
        //             .insert(*ident, self.env._buckets.add_slice(value));
        //     }
        // }

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
            let type_range = return_type.range.clone();
            let return_type = self.convert_type(return_type, *pointer_count);
            if let TCTypeKind::Struct { ident } = return_type.kind {
                self.check_struct_type(ident, decl_idx, *pointer_count, type_range.clone())?;
            }

            let mut names = HashMap::new();
            let mut typed_params = Vec::new();
            for param in params.iter() {
                if let Some(original_range) = names.get(&param.ident) {
                    return Err(Error::parameter_redeclaration(original_range, &param.range));
                } else {
                    names.insert(param.ident, param.range.clone());
                }

                let param_type = self.convert_type(&param.decl_type, *pointer_count);
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
                self.functions
                    .insert(*ident, self.env._buckets.add_slice(body));
            }
        }

        return Ok(());
    }

    pub fn check_struct_type(
        &self,
        struct_ident: u32,
        decl_idx: u32,
        pointer_count: u32,
        range: Range<u32>,
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

    pub fn convert_type(&mut self, type_node: &ASTType, pointer_count: u32) -> TCType {
        let mut out = TCType {
            kind: TCTypeKind::Int,
            pointer_count: pointer_count,
        };

        match &type_node.kind {
            ASTTypeKind::Int => {
                out.kind = TCTypeKind::Int;
                return out;
            }
            ASTTypeKind::Char => {
                out.kind = TCTypeKind::Char;
                return out;
            }
            ASTTypeKind::Void => {
                out.kind = TCTypeKind::Void;
                return out;
            }
            ASTTypeKind::Struct { ident } => {
                out.kind = TCTypeKind::Struct { ident: *ident };
                return out;
            }
        }
    }
}
