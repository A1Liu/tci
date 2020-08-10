use crate::ast::{Expr, StructDecl};
use crate::ast_typed::{TCStruct, TCType, TCTypeKind};
use crate::lexer::{Token, TokenKind};
use crate::*;

pub struct Error {
    pub message: String,
    pub sections: Vec<(Range, String)>,
}

impl Error {
    pub fn new(message: &str, sections: Vec<(Range, String)>) -> Error {
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

    pub fn expect_ident(tok: &Token) -> Result<(u32, Range), Error> {
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

    pub fn expect_rparen(matching_tok: &Range, tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::RParen {
            return Err(Self::new(
                "expected ')' token, got something else instead",
                vec![
                    (
                        tok.range.clone(),
                        format!("this was interpreted as {:?} when it should be a ')'", tok),
                    ),
                    (matching_tok.clone(), "matching left paren here".to_string()),
                ],
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

    pub fn struct_member_redefinition(original: &Range, new: &Range) -> Error {
        return Error::new(
            "name redefined in struct",
            vec![
                (original.clone(), "first definition here".to_string()),
                (new.clone(), "second definition here".to_string()),
            ],
        );
    }

    pub fn struct_incomplete_type(member_type: &TCStruct, member_range: &Range) -> Error {
        return Error::new(
            "referenced incomplete type",
            vec![
                (
                    member_type.range.clone(),
                    "incomplete type is here".to_string(),
                ),
                (member_range.clone(), "type is used here".to_string()),
            ],
        );
    }

    pub fn struct_misordered_type(member_type: &TCStruct, member_range: &Range) -> Error {
        return Error::new(
            "used type defined later in file",
            vec![
                (
                    member_type.range.clone(),
                    "type is defined here".to_string(),
                ),
                (member_range.clone(), "type is used here".to_string()),
            ],
        );
    }

    pub fn struct_doesnt_exist(member_range: &Range) -> Error {
        return Error::new(
            "referenced struct that doesn't exist",
            vec![(member_range.clone(), "struct is used here".to_string())],
        );
    }

    pub fn variable_redefinition(original_range: &Range, range: &Range) -> Error {
        return Error::new(
            "redefinition of variable",
            vec![
                (
                    original_range.clone(),
                    "original definition here".to_string(),
                ),
                (range.clone(), "second definition here".to_string()),
            ],
        );
    }

    pub fn parameter_redeclaration(original_range: &Range, range: &Range) -> Error {
        return Error::new(
            "redeclaration of function parameter",
            vec![
                (
                    original_range.clone(),
                    "original declaration here".to_string(),
                ),
                (range.clone(), "second declaration here".to_string()),
            ],
        );
    }

    pub fn function_declaration_mismatch(original_range: &Range, range: &Range) -> Error {
        return Error::new(
            "function declaration doesn't match previous declaration",
            vec![
                (
                    original_range.clone(),
                    "original declaration here".to_string(),
                ),
                (range.clone(), "second declaration here".to_string()),
            ],
        );
    }

    pub fn function_redefinition(original_range: &Range, range: &Range) -> Error {
        return Error::new(
            "redefinition of function",
            vec![
                (
                    original_range.clone(),
                    "original definition here".to_string(),
                ),
                (range.clone(), "second definition here".to_string()),
            ],
        );
    }

    pub fn truth_value_of_struct(value: &Expr, value_type: &TCType) -> Result<(), Error> {
        if let TCType {
            kind: TCTypeKind::Struct { .. },
            ..
        } = value_type
        {
            return Err(Error::new(
                "tried to check truth value of struct",
                vec![(value.range.clone(), "value is a struct type".to_string())],
            ));
        }

        return Ok(());
    }
}
