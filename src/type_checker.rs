use crate::ast::*;
use crate::buckets::BucketList;
use crate::errors::Error;
use crate::lexer::Token;
use core::ops::Range;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TCDecl<'a> {
    decl_type: TCType<'a>,
    ident: Option<u32>,
    range: Range<u32>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TCTypeKind<'a> {
    Int,
    Char,
    Void,
    Struct {
        members: &'a [TCDecl<'a>],
        range: Range<u32>,
        decl_idx: u32,
    },
    IncompleteStruct {
        range: Range<u32>,
        decl_idx: u32,
    },
}

#[derive(Debug, Clone)]
pub struct TCType<'a> {
    kind: TCTypeKind<'a>,
    pointer_count: u32,
}

#[derive(Debug, Clone)]
pub struct TCFunc<'a> {
    return_type: TCType<'a>,
    params: &'a [TCType<'a>],
    range: Range<u32>,
    decl_idx: u32,
}

impl<'a> PartialEq for TCDecl<'a> {
    fn eq(&self, other: &Self) -> bool {
        return self.decl_type == other.decl_type && self.ident == other.ident;
    }
}

impl<'a> PartialEq for TCType<'a> {
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
    pub struct_types: HashMap<u32, TCType<'b>>,
    pub types: HashMap<u32, TCType<'b>>,
    pub symbols: HashMap<u32, TCType<'b>>,
    pub func_types: HashMap<u32, TCFunc<'b>>,
}

pub struct TypeChecker1<'a, 'b> {
    pub env: TypeEnv<'a, 'b>,
    pub functions: HashMap<u32, &'b [Token]>,
    pub decl_idx: u32,
}

impl<'a, 'b> TypeChecker1<'a, 'b> {
    pub fn new() -> Self {
        Self {
            env: TypeEnv {
                _buckets: BucketList::new(),
                struct_types: HashMap::new(),
                types: HashMap::new(),
                symbols: HashMap::new(),
                func_types: HashMap::new(),
            },
            functions: HashMap::new(),
            decl_idx: 1,
        }
    }

    pub fn check_global_stmts(&mut self, stmts: &[GlobalStmt<'b>]) -> Result<(), Error> {
        for stmt in stmts {
            let decl_type = match &stmt.kind {
                GlobalStmtKind::StructDecl(decl_type) => decl_type,
                GlobalStmtKind::Decl(decl) => match &decl.kind {
                    DeclKind::Uninit { decl_type, .. } => continue,

                    DeclKind::WithValue { decl_type, .. } => continue,
                },
                GlobalStmtKind::Func { return_type, .. } => continue,
                GlobalStmtKind::FuncDecl { return_type, .. } => continue,
            };
        }

        return Err(Error::new("", vec![]));
    }

    /*
           pub fn convert_add_type_decl(&mut self, type_node: &ASTType) -> Result<TCType<'b>, Error> {
           let mut out = TCType {
           kind: TCTypeKind::Int,
           pointer_count: type_node.pointer_count,
           };

           match type_node.kind {
           ASTTypeKind::Struct { ident } => {
           if let Some(t) = self.env.struct_types.get(&ident) {
           match t.kind {
           TCTypeKind::Struct { .. } => return Ok(t.clone()),
           TCTypeKind::IncompleteStruct { range, .. } => {
           return Err(Error::new(
           "type is not complete yet",
           vec![(range, "type is not complete yet".to_string())],
           ))
           }
           _ => panic!("found incorrect type {:?} in struct_types", t),
           }
           }

           out.kind = TCTypeKind::IncompleteStruct {
           range: type_node.range,
           decl_idx: self.decl_idx,
           };
           self.decl_idx += 1;

           self.env.struct_types.insert(ident, out.clone());
           return Ok(out);
           }
           ASTTypeKind::StructDefn { .. } => return self.convert_add_type(type_node),
           _ => {
           return Err(Error::new(
           "Missing variable name in declaration",
           vec![(type_node.range.clone(), "declaration here".to_string())],
           ));
           }
           }
           }

           pub fn convert_add_type(&mut self, type_node: &ASTType) -> Result<TCType<'b>, Error> {
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
           ASTTypeKind::StructDefn {
           ident,
           members,
           ident_range,
           } => {
           let decl_idx = self.decl_idx;
           self.decl_idx += 1;

           out.kind = TCTypeKind::IncompleteStruct {
           range: type_node.range.clone(),
           decl_idx,
    };

    let mut typed_members = Vec::new();

    if let Some(id) = ident {
        self.env.struct_types.insert(*id, out.clone());
    }

    for member in *members {
        let (decl_type, ident) = match &member.kind {
            DeclKind::Type(decl_type) => (self.convert_add_type_decl(decl_type)?, None),
            DeclKind::Uninit { decl_type, ident } => {
                (self.convert_add_type(decl_type)?, Some(*ident))
            }
            DeclKind::WithValue { value, .. } => {
                return Err(Error::new(
                        "default values for structs aren't supported",
                        vec![(value.range, "value here".to_string())],
                        ))
            }
        };

        typed_members.push(TCDecl {
            decl_type,
            ident: ident,
            range: member.range.clone(),
        });
    }

    out.kind = TCTypeKind::Struct {
        members: self.env._buckets.add_array(typed_members),
        range: type_node.range.clone(),
        decl_idx,
    };

    if let Some(id) = ident {
        self.env.struct_types.insert(*id, out.clone());
    }

    return Ok(out);
    }
    ASTTypeKind::Struct { ident } => {
        if let Some(t) = self.env.struct_types.get(ident) {
            match t.kind {
                TCTypeKind::Struct {
                    members,
                    range,
                    decl_idx,
                } => {}
            }
            if let TCTypeKind::Struct { members } = t.kind {
                if !complete && t.pointer_count == 0 {
                    return Err(Error::new(
                            "type is not complete yet",
                            vec![(t.range.clone(), "type is not complete yet".to_string())],
                            ));
                }
                return Ok(t.clone());
            }

            panic!("found incorrect type {:?} in struct_types", t);
        }

        return Err(Error::new(
                "unrecognized type name",
                vec![(
                    type_node.range.clone(),
                    "couldn't find this type name".to_string(),
                    )],
                ));
    }
    ASTTypeKind::Ident(id) => {
        if let Some(t) = self.env.types.get(id) {
            return Ok(t.clone());
        }

        return Err(Error::new(
                "unrecognized type name",
                vec![(
                    type_node.range.clone(),
                    "couldn't find this type name".to_string(),
                    )],
                ));
    }
    }
    }

    pub fn add_decl(&mut self, stmt: &GlobalStmt) -> Result<(), Error> {
        match &stmt.kind {
            GlobalStmtKind::FuncDecl {
                return_type,
                ident,
                params,
            } => {
                let return_type = self.convert_add_type(return_type)?;
                let mut type_params = Vec::new();
                for param in *params {
                    type_params.push(self.convert_add_type(param.decl_type())?);
                }

                let type_params = self.env._buckets.add_array(type_params);
                let tc_func = TCFunc {
                    return_type,
                    params: type_params,
                    range: stmt.range.clone(),
                    decl_idx: self.decl_idx,
                };
                self.decl_idx += 1;

                if let Some(func) = self.env.func_types.get(ident) {
                    if &tc_func != func {
                        return Err(Error::new(
                                "function declaration doesn't match previous declaration",
                                vec![
                                (func.range.clone(), "previous declaration here".to_string()),
                                (
                                    stmt.range.clone(),
                                    "mismatched declaration here".to_string(),
                                ),
                                ],
                                ));
                    }
                }

                self.env.func_types.insert(*ident, tc_func);
                return Ok(());
            }
            GlobalStmtKind::Func {
                return_type,
                ident,
                params,
                body,
            } => {
                let return_type = self.convert_add_type(return_type)?;
                let mut type_params = Vec::new();
                for param in *params {
                    type_params.push(self.convert_add_type(param.decl_type())?);
                }

                let type_params = self.env._buckets.add_array(type_params);
                let tc_func = TCFunc {
                    return_type,
                    params: type_params,
                    range: stmt.range.clone(),
                    decl_idx: self.decl_idx,
                };
                self.decl_idx += 1;

                if let Some(func) = self.env.func_types.get(ident) {
                    if let Some(_) = self.functions.get(ident) {
                        return Err(Error::new(
                                "function already defined",
                                vec![
                                (func.range.clone(), "function defined here".to_string()),
                                (stmt.range.clone(), "second definition here".to_string()),
                                ],
                                ));
                    }

                    if &tc_func != func {
                        return Err(Error::new(
                                "function definition doesn't match previous declaration",
                                vec![
                                (func.range.clone(), "previous declaration here".to_string()),
                                (stmt.range.clone(), "mismatched definition here".to_string()),
                                ],
                                ));
                    }
                }

                self.env.func_types.insert(*ident, tc_func);
                self.functions
                    .insert(*ident, self.env._buckets.add_slice(body));
                return Ok(());
            }
            GlobalStmtKind::Decl(decl) => {
                match &decl.kind {
                    DeclKind::Type(decl_type) => {
                        self.convert_add_type_decl(decl_type)?;
                    }
                    DeclKind::Uninit { decl_type, ident } => {
                        let decl_type = self.convert_add_type(decl_type)?;
                        self.env.symbols.insert(*ident, decl_type);
                    }
                    DeclKind::WithValue {
                        decl_type,
                        ident,
                        value,
                    } => {
                        let decl_type = self.convert_add_type(decl_type)?;
                        self.env.symbols.insert(*ident, decl_type);
                        panic!("not implemented yet");
                    }
                }

                return Ok(());
            }
        }
    }
    */
}
