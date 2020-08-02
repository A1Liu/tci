use crate::ast::StructDecl;
use crate::lexer::{Token, TokenKind};
use crate::type_checker::TCStruct;
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

    pub fn struct_redefinition(original: &TCStruct, new: &StructDecl) -> Result<(), Error> {
        if let Some((defn_idx, members)) = original.defn {
            if let Some(new_members) = new.members {
                return Err(Error::new(
                    "redefinition of struct",
                    vec![
                        (
                            original.range.clone(),
                            "original definition here".to_string(),
                        ),
                        (new.range.clone(), "second definition here".to_string()),
                    ],
                ));
            }
        }

        return Ok(());
    }

    pub fn struct_member_redefinition(original: &Range<u32>, new: &Range<u32>) -> Error {
        return Error::new(
            "name redefined in struct",
            vec![
                (original.clone(), "first definition here".to_string()),
                (new.clone(), "second definition here".to_string()),
            ],
        );
    }

    pub fn struct_member_incomplete_type(
        member_type: &TCStruct,
        member_range: &Range<u32>,
    ) -> Error {
        return Error::new(
            "incomplete type used as member",
            vec![
                (
                    member_type.range.clone(),
                    "incomplete type is here".to_string(),
                ),
                (member_range.clone(), "type used is here".to_string()),
            ],
        );
    }

    pub fn struct_member_misordered_type(
        member_type: &TCStruct,
        member_range: &Range<u32>,
    ) -> Error {
        return Error::new(
            "type defined later in file used as member",
            vec![
                (
                    member_type.range.clone(),
                    "type is defined here".to_string(),
                ),
                (member_range.clone(), "type is used here".to_string()),
            ],
        );
    }
}
