use crate::ast::{ASTType, ASTTypeKind};
use crate::*;

#[derive(Debug, Clone)]
pub struct TCStructMember {
    pub decl_type: TCType,
    pub ident: u32,
    pub range: Range,
}

#[derive(Debug)]
pub struct TCStruct<'a> {
    pub decl_idx: u32,
    pub defn: Option<(u32, &'a [TCStructMember])>,
    pub ident_range: Range,
    pub range: Range,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TCTypeKind {
    Int,
    Char,
    Void,
    Struct { ident: u32 },
}

#[derive(Debug, Clone, Copy)]
pub struct TCType {
    pub kind: TCTypeKind,
    pub pointer_count: u32,
}

#[derive(Debug, Clone)]
pub struct TCGlobalValue {
    pub decl_type: TCType,
    pub range: Range,
    pub decl_idx: u32,
}

#[derive(Debug, Clone)]
pub struct TCValue {
    pub decl_type: TCType,
    pub range: Range,
}

#[derive(Debug, Clone)]
pub struct TCFuncParam {
    pub decl_type: TCType,
    pub ident: u32,
    pub range: Range,
}

#[derive(Debug, Clone)]
pub struct TCFunc<'a> {
    pub return_type: TCType,
    pub params: &'a [TCFuncParam],
    pub range: Range,
    pub decl_idx: u32,
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

pub fn convert_type(type_node: &ASTType, pointer_count: u32) -> TCType {
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

pub enum TCExprKind<'a> {
    ZeroPadToU16(&'a TCExpr<'a>),
    ZeroPadToU32(&'a TCExpr<'a>),
    ZerPadUTo64(&'a TCExpr<'a>),
}

pub struct TCExpr<'a> {
    kind: TCExprKind<'a>,
    _type: TCType,
    range: Range,
}
