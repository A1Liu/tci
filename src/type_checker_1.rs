use crate::ast::*;
use crate::errors::Error;
use crate::lexer::Token;
use crate::parser_1::Parser1;
use core::ops::Range;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TCDecl<'a> {
    decl_type: TCType<'a>,
    ident: u32,
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

#[derive(Debug, Clone, PartialEq)]
pub struct TCFunc<'a> {
    return_type: TCType<'a>,
    params: &'a [TCType<'a>],
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
                let typed_members = self.parser.buckets.add_array(vec![]);
                out.kind = TCTypeKind::Struct {
                    members: typed_members,
                    complete: false,
                };

                out.decl_idx = self.decl_idx;
                self.decl_idx += 1;

                return Ok(out);
            }
            ASTTypeKind::Struct { ident } => {
                if let Some(t) = self.struct_types.get(ident) {
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
            GlobalStmtKind::Func {
                return_type,
                ident,
                params,
                body,
            } => {
                if let Some(func) = self.func_types.get(ident) {};
            }
            _ => {}
        }
        return Err(Error::new("", vec![]));
    }
}
