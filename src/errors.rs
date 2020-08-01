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

    pub fn unexpected_token(parsing_what: &str, tok: &Token) -> Error {
        return Error {
            message: "unexpected token while parsing ".to_string() + parsing_what,
            sections: vec![(
                tok.range.clone(),
                format!("this was interpreted as {:?}", tok),
            )],
        };
    }

    pub fn expect_ident(tok: &Token) -> Result<(u32, Range<u32>), Error> {
        if let TokenKind::Ident(id) = tok.kind {
            return Ok((id, tok.range.clone()));
        } else {
            return Err(Self::new(
                "expected identifier token, got something else instead",
                vec![(
                    tok.range.clone(),
                    format!(
                        "this was interpreted as {:?} when it should be an identifier",
                        tok
                    ),
                )],
            ));
        }
    }

    pub fn expect_rbracket(tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::RBracket {
            return Err(Self::new(
                "expected ']' token, got something else instead",
                vec![(
                    tok.range.clone(),
                    format!("this was interpreted as {:?} when it should be a ']'", tok),
                )],
            ));
        }
        return Ok(());
    }

    pub fn expect_lbrace(tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::LBrace {
            return Err(Self::new(
                "expected '{' token, got something else instead",
                vec![(
                    tok.range.clone(),
                    format!("this was interpreted as {:?} when it should be a '{{'", tok),
                )],
            ));
        }
        return Ok(());
    }

    pub fn expect_rparen(tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::RParen {
            return Err(Self::new(
                "expected ')' token, got something else instead",
                vec![(
                    tok.range.clone(),
                    format!("this was interpreted as {:?} when it should be a ')'", tok),
                )],
            ));
        }
        return Ok(());
    }

    pub fn expect_lparen(tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::LParen {
            return Err(Self::new(
                "expected '(' token, got something else instead",
                vec![(
                    tok.range.clone(),
                    format!("this was interpreted as {:?} when it should be a '('", tok),
                )],
            ));
        }
        return Ok(());
    }

    pub fn expect_semicolon(tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::Semicolon {
            return Err(Self::new(
                "expected ';' token, got something else instead",
                vec![(
                    tok.range.clone(),
                    format!("this was interpreted as {:?} when it should be a ';'", tok),
                )],
            ));
        }
        return Ok(());
    }
}
