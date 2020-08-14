use crate::ast::*;
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

    pub fn variable_is_void(decl_range: Range) -> Error {
        return Error::new(
            "variable has type void",
            vec![(decl_range, "variable is here".to_string())],
        );
    }

    pub fn assignment_convert_incompatible(target_range: Range, value_range: Range) -> Error {
        return Error::new(
            "value cannot be converted to target type",
            vec![
                (target_range, "target type is here".to_string()),
                (value_range, "value is here".to_string()),
            ],
        );
    }

    pub fn unexpected_return_value(value_range: Range) -> Error {
        return Error::new(
            "didn't expect value in return statement (return type is void)",
            vec![(value_range, "value is here".to_string())],
        );
    }

    pub fn expected_return_value(target_range: Range, value_range: Range) -> Error {
        return Error::new(
            "expected value in return statement (return type is not void)",
            vec![
                (target_range, "target type is here".to_string()),
                (value_range, "value is here".to_string()),
            ],
        );
    }

    pub fn return_type_convert(
        target_type: &TCType,
        target_range: Range,
        value_type: Option<&TCType>,
        value_range: Range,
    ) -> Result<(), Error> {
        let value_type = match value_type {
            Some(t) => t,
            None => {
                if target_type.kind == TCTypeKind::Void && target_type.pointer_count == 0 {
                    return Ok(());
                } else {
                    return Err(Error::expected_return_value(target_range, value_range));
                }
            }
        };

        if target_type.kind == TCTypeKind::Void && target_type.pointer_count == 0 {
            return Err(Error::unexpected_return_value(value_range));
        }

        return Error::assignment_convert(target_type, target_range, value_type, value_range);
    }

    pub fn assignment_convert(
        target_type: &TCType,
        target_range: Range,
        value_type: &TCType,
        value_range: Range,
    ) -> Result<(), Error> {
        match target_type.kind {
            TCTypeKind::Void => {
                if target_type.pointer_count == 0 {
                    return Err(Error::variable_is_void(target_range));
                }

                if target_type == value_type {
                    return Ok(());
                }

                if target_type.pointer_count == 1 && value_type.pointer_count > 0 {
                    return Ok(());
                }

                return Err(Error::assignment_convert_incompatible(
                    target_range,
                    value_range,
                ));
            }
            TCTypeKind::Int | TCTypeKind::Char => {
                if target_type.pointer_count != value_type.pointer_count {
                    return Err(Error::assignment_convert_incompatible(
                        target_range,
                        value_range,
                    ));
                }

                if target_type.pointer_count == 0 {
                    match value_type.kind {
                        TCTypeKind::Int | TCTypeKind::Char => return Ok(()),
                        _ => {
                            return Err(Error::assignment_convert_incompatible(
                                target_range,
                                value_range,
                            ))
                        }
                    }
                } else if target_type.kind == value_type.kind {
                    return Ok(());
                } else {
                    return Err(Error::assignment_convert_incompatible(
                        target_range,
                        value_range,
                    ));
                }
            }
            TCTypeKind::Struct { .. } => {
                if target_type == value_type {
                    return Ok(());
                }

                return Err(Error::assignment_convert_incompatible(
                    target_range,
                    value_range,
                ));
            }
        }
    }
}
