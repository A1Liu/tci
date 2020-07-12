use crate::parser_1::Parser1;
use core::ops::Range;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TCDecl<'a> {
    decl_type: TCType<'a>,
    ident: u32,
    range: Range<u32>,
}

#[derive(Debug, Clone)]
pub enum TCTypeKind<'a> {
    Int,
    Char,
    Struct { members: &'a [TCDecl<'a>] },
}

#[derive(Debug, Clone)]
pub struct TCType<'a> {
    kind: TCTypeKind<'a>,
    range: Range<u32>,
    pointer_count: u32,
    decl_idx: u32,
    complete: bool,
}

#[derive(Debug, Clone)]
pub struct TCFunc<'a> {
    return_type: TCType<'a>,
    params: &'a [TCType<'a>],
}

pub struct TypeChecker1<'a> {
    parser: Parser1<'a>,
    struct_types: HashMap<u32, TCType<'a>>,
    types: HashMap<u32, TCType<'a>>,
    symbols: HashMap<u32, TCType<'a>>,
    functions: HashMap<u32, TCFunc<'a>>,
}

impl<'a> TypeChecker1<'a> {
    pub fn new(data: &'a str) -> Self {
        Self {
            parser: Parser1::new(data),
            struct_types: HashMap::new(),
            types: HashMap::new(),
            symbols: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}
