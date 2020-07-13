use crate::ast::*;
use crate::errors::Error;
use crate::lexer::Token;
use crate::parser_1::Parser1;
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
        complete: bool,
    },
}

#[derive(Debug, Clone)]
pub struct TCType<'a> {
    kind: TCTypeKind<'a>,
    range: Range<u32>,
    pointer_count: u32,
    decl_idx: u32,
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

pub struct TypeChecker1<'a> {
    parser: Parser1<'a>,
    struct_types: HashMap<u32, TCType<'a>>,
    types: HashMap<u32, TCType<'a>>,
    symbols: HashMap<u32, TCType<'a>>,
    func_types: HashMap<u32, TCFunc<'a>>,
    functions: HashMap<u32, &'a [Token]>,
    decl_idx: u32,
}

impl<'a> TypeChecker1<'a> {
    pub fn new(data: &'a str) -> Self {
        Self {
            parser: Parser1::new(data),
            struct_types: HashMap::new(),
            types: HashMap::new(),
            symbols: HashMap::new(),
            func_types: HashMap::new(),
            functions: HashMap::new(),
            decl_idx: 1,
        }
    }

    pub fn convert_add_type(&mut self, type_node: &ASTType<'a>) -> Result<TCType<'a>, Error> {
        let mut out = TCType {
            kind: TCTypeKind::Int,
            decl_idx: 0,
            range: type_node.range.clone(),
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
            ASTTypeKind::StructDefn { ident, members } => {
                out.kind = TCTypeKind::Struct {
                    members: self.parser.buckets.add_array(Vec::new()),
                    complete: false,
                };

                let mut typed_members = Vec::new();
                out.decl_idx = self.decl_idx;
                self.decl_idx += 1;

                if let Some(id) = ident {
                    self.struct_types.insert(*id, out.clone());
                }

                for member in *members {
                    let decl_type = self.convert_add_type(&member.decl_type)?;
                    typed_members.push(TCDecl {
                        decl_type,
                        ident: member.ident,
                        range: member.range.clone(),
                    });
                }

                out.kind = TCTypeKind::Struct {
                    members: self.parser.buckets.add_array(typed_members),
                    complete: true,
                };

                if let Some(id) = ident {
                    self.struct_types.insert(*id, out.clone());
                }

                return Ok(out);
            }
            ASTTypeKind::Struct { ident } => {
                if let Some(t) = self.struct_types.get(ident) {
                    if let TCTypeKind::Struct { members, complete } = t.kind {
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
                if let Some(t) = self.types.get(id) {
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

    pub fn add_decl(&mut self, stmt: &GlobalStmt<'a>) -> Result<(), Error> {
        match &stmt.kind {
            GlobalStmtKind::FuncDecl {
                return_type,
                ident,
                params,
            } => {
                let return_type = self.convert_add_type(return_type)?;
                let mut type_params = Vec::new();
                for param in *params {
                    type_params.push(self.convert_add_type(&param.decl_type)?);
                }

                let type_params = self.parser.buckets.add_array(type_params);
                let tc_func = TCFunc {
                    return_type,
                    params: type_params,
                    range: stmt.range.clone(),
                    decl_idx: self.decl_idx,
                };
                self.decl_idx += 1;

                if let Some(func) = self.func_types.get(ident) {
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

                self.func_types.insert(*ident, tc_func);
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
                    type_params.push(self.convert_add_type(&param.decl_type)?);
                }

                let type_params = self.parser.buckets.add_array(type_params);
                let tc_func = TCFunc {
                    return_type,
                    params: type_params,
                    range: stmt.range.clone(),
                    decl_idx: self.decl_idx,
                };
                self.decl_idx += 1;

                if let Some(func) = self.func_types.get(ident) {
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

                self.func_types.insert(*ident, tc_func);
                self.functions.insert(*ident, body);
            }
            GlobalStmtKind::Decl(decl) => {
                let decl_type = self.convert_add_type(&decl.decl_type)?;
                if let Some(ident) = decl.ident {
                    self.symbols.insert(ident, decl_type);
                }
            }
        }
        return Err(Error::new("", vec![]));
    }
}
