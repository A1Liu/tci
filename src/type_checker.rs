use crate::ast::*;
use crate::buckets::BucketList;
use crate::errors::Error;
use crate::lexer::Token;
use core::ops::Range;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TCStructMember {
    decl_type: TCType,
    ident: Option<u32>,
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
pub struct TCFunc<'a> {
    return_type: TCType,
    params: &'a [TCType],
    range: Range<u32>,
    decl_idx: u32,
}

impl PartialEq for TCStructMember {
    fn eq(&self, other: &Self) -> bool {
        return self.decl_type == other.decl_type && self.ident == other.ident;
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
    pub symbols: HashMap<u32, TCType>,
    pub func_types: HashMap<u32, TCFunc<'b>>,
}

pub struct TypeChecker1<'a, 'b> {
    pub env: TypeEnv<'a, 'b>,
    pub functions: HashMap<u32, &'b [Token]>,
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
        }
    }

    pub fn check_global_stmts(&mut self, stmts: &[GlobalStmt<'b>]) -> Result<(), Error> {
        // Add all types to the type table
        for (decl_idx, stmt) in stmts.iter().enumerate() {
            let decl_type = match &stmt.kind {
                GlobalStmtKind::StructDecl(decl_type) => decl_type,
                GlobalStmtKind::Decl(decl) => match &decl.kind {
                    DeclKind::Uninit { decl_type, .. } => continue,
                    DeclKind::WithValue { decl_type, .. } => continue,
                },
                GlobalStmtKind::Func { return_type, .. } => continue,
                GlobalStmtKind::FuncDecl { return_type, .. } => continue,
            };

            let defn_idx = decl_idx as u32;
            let mut decl_idx = decl_idx as u32;
            if let Some(original) = self.env.struct_types.get(&decl_type.ident) {
                Error::struct_redefinition(original, decl_type)?;
                decl_idx = original.decl_idx;
            }

            if let Some(members) = decl_type.members {
                let mut typed_members = Vec::new();
                let mut names = HashMap::new();

                for member in members {
                    let kind = match member.decl_type.kind {
                        ASTTypeKind::Int => TCTypeKind::Int,
                        ASTTypeKind::Char => TCTypeKind::Char,
                        ASTTypeKind::Void => TCTypeKind::Void,
                        ASTTypeKind::Struct { ident } => TCTypeKind::Struct { ident },
                    };

                    if let Some(original_range) = names.get(&member.ident) {
                        return Err(Error::struct_member_redefinition(
                            original_range,
                            &member.range,
                        ));
                    } else {
                        names.insert(member.ident, member.range.clone());
                    }

                    typed_members.push(TCStructMember {
                        decl_type: TCType {
                            kind,
                            pointer_count: member.decl_type.pointer_count,
                        },
                        ident: Some(member.ident),
                        range: member.range.clone(),
                    });
                }

                self.env.struct_types.insert(
                    decl_type.ident,
                    TCStruct {
                        decl_idx,
                        ident_range: decl_type.ident_range.clone(),
                        range: decl_type.range.clone(),
                        defn: Some((defn_idx, self.env._buckets.add_array(typed_members))),
                    },
                );
            } else {
                self.env.struct_types.insert(
                    decl_type.ident,
                    TCStruct {
                        decl_idx,
                        ident_range: decl_type.ident_range.clone(),
                        range: decl_type.range.clone(),
                        defn: None,
                    },
                );
            }
        }

        for (struct_ident, type_defn) in self.env.struct_types.iter() {
            if let Some((defn_idx, members)) = type_defn.defn {
                let filter_map_struct_idents = |member: &TCStructMember| {
                    if let TCTypeKind::Struct { ident } = member.decl_type.kind {
                        Some((ident, member.range.clone(), member.decl_type.pointer_count))
                    } else {
                        None
                    }
                };

                let inner_struct_members = members.iter().filter_map(filter_map_struct_idents);
                for (member_struct_ident, member_range, member_pointer_count) in
                    inner_struct_members
                {
                    // If the member type is the same as the current type, and there's no pointer
                    if *struct_ident == member_struct_ident && member_pointer_count == 0 {
                        return Err(Error::new(
                            "recursive struct",
                            vec![
                                (type_defn.ident_range.clone(), "struct here".to_string()),
                                (member_range.clone(), "recursive member here".to_string()),
                            ],
                        ));
                    }

                    if let Some(member_type) = self.env.struct_types.get(&member_struct_ident) {
                        if let Some((member_type_defn_idx, _)) = member_type.defn {
                            if member_pointer_count == 0 && member_type_defn_idx > defn_idx {
                                return Err(Error::struct_member_misordered_type(
                                    member_type,
                                    &member_range,
                                ));
                            }
                        } else if member_pointer_count == 0 {
                            return Err(Error::struct_member_incomplete_type(
                                member_type,
                                &member_range,
                            ));
                        }
                    }
                }
            }
        }

        for (decl_idx, stmt) in stmts.iter().enumerate() {}

        return Ok(());
    }

    pub fn convert_type(&mut self, type_node: &ASTType) -> Result<TCType, Error> {
        let mut out = TCType {
            kind: TCTypeKind::Int,
            pointer_count: type_node.pointer_count,
        };

        match &type_node.kind {
            ASTTypeKind::Int => {
                out.kind = TCTypeKind::Int;
                return Ok(out);
            }
            ASTTypeKind::Char => {
                out.kind = TCTypeKind::Char;
                return Ok(out);
            }
            ASTTypeKind::Void => {
                out.kind = TCTypeKind::Void;
                return Ok(out);
            }
            ASTTypeKind::Struct { ident } => {
                out.kind = TCTypeKind::Struct { ident: *ident };
                return Ok(out);
            }
        }
    }
}
