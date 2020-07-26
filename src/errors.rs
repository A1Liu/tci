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

    pub fn expect_lbrace(tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::LBrace {
            return Err(Self::new(
                "expected '{' token, got something else instead",
                vec![(tok.range.clone(), "should be a '{'".to_string())],
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
}
