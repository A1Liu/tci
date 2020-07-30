use crate::ast::{ASTType, ASTTypeKind};
use crate::lexer::{Token, TokenKind};
use std::ops::Range;

pub struct Error {
    pub message: String,
    pub sections: Vec<(Range<u32>, String)>,
}

impl Error {
    pub fn new(message: &str, sections: Vec<(Range<u32>, String)>) -> Error {
        Self {
            message: message.to_string(),
            sections,
        }
    }

    pub fn expect_ident(tok: &Token) -> Result<(u32, Range<u32>), Error> {
        if let TokenKind::Ident(id) = tok.kind {
            return Ok((id, tok.range.clone()));
        } else {
            return Err(Self::new(
                "expected ']' token, got something else instead",
                vec![(tok.range.clone(), "should be a ']'".to_string())],
            ));
        }
    }

    pub fn expect_rbracket(tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::RBracket {
            return Err(Self::new(
                "expected ']' token, got something else instead",
                vec![(tok.range.clone(), "should be a ']'".to_string())],
            ));
        }
        return Ok(());
    }

    pub fn expect_lbrace(tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::LBrace {
            return Err(Self::new(
                "expected '{' token, got something else instead",
                vec![(tok.range.clone(), "should be a '{'".to_string())],
            ));
        }
        return Ok(());
    }

    pub fn expect_rparen(tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::RParen {
            return Err(Self::new(
                "expected ')' token, got something else instead",
                vec![(tok.range.clone(), "should be a ')'".to_string())],
            ));
        }
        return Ok(());
    }

    pub fn expect_lparen(tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::LParen {
            return Err(Self::new(
                "expected '(' token, got something else instead",
                vec![(tok.range.clone(), "should be a '('".to_string())],
            ));
        }
        return Ok(());
    }

    pub fn expect_semicolon(tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::Semicolon {
            return Err(Self::new(
                "expected ';' token, got something else instead",
                vec![(tok.range.clone(), "should be a ';'".to_string())],
            ));
        }
        return Ok(());
    }

    pub fn expect_non_struct_defn(ast_type: &ASTType) -> Result<(), Error> {
        match ast_type.kind {
            ASTTypeKind::StructDefn { .. } => {
                return Err(Error::new(
                    "not allowed to define a struct in this context",
                    vec![(ast_type.range.clone(), "struct defintion here".to_string())],
                ));
            }
            _ => return Ok(()),
        }
    }
}
