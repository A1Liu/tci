use crate::ast::*;
use crate::buckets::BucketList;
use crate::errors::Error;
use crate::lexer::{Lexer, Token, TokenKind};

pub struct Parser1<'a, 'b> {
    pub _buckets: &'a mut BucketList<'b>,
    pub lexer: Lexer<'b>,
    pub token_stack: Vec<Token>,
}

pub trait Parser<'a> {
    fn peek(&mut self) -> Token;
    fn pop(&mut self) -> Token;
    fn buckets(&mut self) -> &mut BucketList<'a>;
}

pub trait TypeParser<'a>: Parser<'a> {
    fn parse_simple_type_prefix(&mut self) -> Result<ASTType, Error> {
        let tok = self.pop();
        match tok.kind {
            TokenKind::Int => {
                return Ok(ASTType {
                    kind: ASTTypeKind::Int,
                    range: tok.range,
                })
            }
            TokenKind::Void => {
                return Ok(ASTType {
                    kind: ASTTypeKind::Void,
                    range: tok.range,
                })
            }
            TokenKind::Char => {
                return Ok(ASTType {
                    kind: ASTTypeKind::Char,
                    range: tok.range,
                })
            }
            TokenKind::Struct => panic!("struct should be handled by another function"),
            _ => return Err(Error::unexpected_token("type", &tok)),
        }
    }

    // Vec returned is always non-empty
    fn parse_simple_decl(&mut self) -> Result<Result<DeclIdent, Vec<Token>>, Error> {
        let mut pointer_count: u32 = 0;
        let mut toks = Vec::new();
        while self.peek().kind == TokenKind::Star {
            toks.push(self.pop());
            pointer_count += 1;
        }

        let tok = self.pop();
        let (ident, range) = Error::expect_ident(&tok)?;
        toks.push(tok);

        let tok = self.peek();
        if tok.kind == TokenKind::Comma {
            self.pop();
            let decl_var = DeclIdent {
                pointer_count,
                ident,
                range,
            };
            return Ok(Ok(decl_var));
        }

        if tok.kind != TokenKind::Eq {
            let decl_var = DeclIdent {
                pointer_count,
                ident,
                range,
            };
            return Ok(Ok(decl_var));
        }

        let mut tok = self.peek();
        while tok.kind != TokenKind::Semicolon && tok.kind != TokenKind::End {
            self.pop();
            toks.push(tok);
            tok = self.peek();
        }

        if tok.kind == TokenKind::End {
            return Err(Error::new(
                "unexpected end of file",
                vec![(tok.range.clone(), "end of file here".to_string())],
            ));
        }

        return Ok(Err(toks));
    }

    fn parse_inner_struct_decl(&mut self) -> Result<InnerStructDecl, Error> {
        let decl_type = match self.peek().kind {
            TokenKind::Struct => {
                let start = self.pop().range.start;
                let (ident, range) = Error::expect_ident(&self.pop())?;

                ASTType {
                    kind: ASTTypeKind::Struct { ident },
                    range: start..range.end,
                }
            }
            _ => self.parse_simple_type_prefix()?,
        };

        let mut pointer_count: u32 = 0;
        while self.peek().kind == TokenKind::Star {
            self.pop();
            pointer_count += 1;
        }

        let (ident, range) = Error::expect_ident(&self.pop())?;

        return Ok(InnerStructDecl {
            range: decl_type.range.start..range.end,
            pointer_count,
            decl_type,
            ident,
        });
    }
}

impl<'a, 'b> Parser<'b> for Parser1<'a, 'b> {
    fn peek(&mut self) -> Token {
        if self.token_stack.len() > 0 {
            return self.token_stack[self.token_stack.len() - 1].clone();
        }

        let tok = self.lexer.next();
        self.token_stack.push(tok.clone());
        return tok;
    }

    fn pop(&mut self) -> Token {
        match self.token_stack.pop() {
            Some(x) => x,
            None => self.lexer.next(),
        }
    }

    fn buckets(&mut self) -> &mut BucketList<'b> {
        return self._buckets;
    }
}

impl<'a, 'b> TypeParser<'b> for Parser1<'a, 'b> {}

impl<'a, 'b> Parser1<'a, 'b> {
    pub fn new(data: &'b str) -> Self {
        Self {
            _buckets: BucketList::new(),
            lexer: Lexer::new(data),
            token_stack: Vec::new(),
        }
    }

    pub fn parse_global_decl(&mut self) -> Result<GlobalStmt<'b>, Error> {
        let decl_type = match self.peek().kind {
            TokenKind::Struct => {
                let start = self.pop().range.start;
                let (ident, ident_range) = Error::expect_ident(&self.pop())?;
                let tok = self.peek();

                if tok.kind == TokenKind::LBrace {
                    // parse as type definition
                    self.pop();

                    let mut decls = Vec::new();
                    while self.peek().kind != TokenKind::RBrace {
                        let decl = self.parse_inner_struct_decl()?;
                        decls.push(decl);
                        Error::expect_semicolon(&self.pop())?;
                    }

                    let end = self.pop().range.end;
                    Error::expect_semicolon(&self.pop())?;

                    return Ok(GlobalStmt {
                        kind: GlobalStmtKind::StructDecl(StructDecl {
                            ident,
                            ident_range,
                            range: start..end,
                            members: Some(self.buckets().add_array(decls)),
                        }),
                        range: start..end,
                    });
                }

                if tok.kind == TokenKind::Semicolon {
                    self.pop();

                    return Ok(GlobalStmt {
                        range: start..ident_range.end,
                        kind: GlobalStmtKind::StructDecl(StructDecl {
                            ident,
                            range: start..ident_range.end,
                            ident_range,
                            members: None,
                        }),
                    });
                }

                ASTType {
                    kind: ASTTypeKind::Struct { ident },
                    range: start..ident_range.end,
                }
            }
            _ => self.parse_simple_type_prefix()?,
        };

        let decl = self.parse_simple_decl()?;
        let (pointer_count, ident, ident_range) = match decl {
            Ok(decl) => (decl.pointer_count, decl.ident, decl.range),
            Err(tokens) => {
                Error::expect_semicolon(&self.pop())?;
                return Ok(GlobalStmt {
                    range: decl_type.range.start..tokens.last().unwrap().range.end,
                    kind: GlobalStmtKind::Decl {
                        decl_type,
                        tokens: self.buckets().add_array(tokens),
                    },
                });
            }
        };

        let header_start = decl_type.range.start;
        let tok = self.pop();
        if tok.kind == TokenKind::Semicolon {
            return Ok(GlobalStmt {
                range: decl_type.range.start..ident_range.end,
                kind: GlobalStmtKind::SingletonDecl {
                    pointer_count,
                    decl_type,
                    ident,
                },
            });
        }

        if tok.kind != TokenKind::LParen {
            return Err(Error::unexpected_token("function declaration", &tok));
        }

        let mut params = Vec::new();
        let rparen_tok = self.peek();
        if rparen_tok.kind != TokenKind::RParen {
            params.push(self.parse_inner_struct_decl()?);
            let mut comma_tok = self.peek();
            while comma_tok.kind == TokenKind::Comma {
                self.pop();
                params.push(self.parse_inner_struct_decl()?);
                comma_tok = self.peek();
            }

            if comma_tok.kind != TokenKind::RParen {
                return Err(Error::unexpected_token(
                    "end of function declaration",
                    &comma_tok,
                ));
            }
        }

        let end = self.pop().range.end;
        let params = self.buckets().add_array(params);
        let end_decl_tok = self.pop();
        if end_decl_tok.kind == TokenKind::Semicolon {
            return Ok(GlobalStmt {
                range: decl_type.range.start..end,
                kind: GlobalStmtKind::FuncDecl {
                    pointer_count,
                    return_type: decl_type,
                    ident: ident,
                    params,
                },
            });
        }

        if end_decl_tok.kind != TokenKind::LBrace {
            return Err(Error::new(
                "unexpected token when parsing ending of function declaration",
                vec![
                    (
                        decl_type.range.start..end,
                        "this was parsed as a function declaration".to_string(),
                    ),
                    (end_decl_tok.range, "expected a ';' or '{' here".to_string()),
                ],
            ));
        }

        let mut brace_count = 1;
        let mut body = Vec::new();

        let mut tok = self.pop();
        match tok.kind {
            TokenKind::RBrace => brace_count -= 1,
            TokenKind::LBrace => brace_count += 1,
            TokenKind::End => {
                return Err(Error::new(
                    "unexpected end of file while parsing function",
                    vec![
                        (
                            decl_type.range.start..end,
                            "function declared here".to_string(),
                        ),
                        (tok.range, "missing closing brace here".to_string()),
                    ],
                ))
            }
            _ => {}
        }

        while brace_count > 0 {
            body.push(tok);
            tok = self.pop();
            match tok.kind {
                TokenKind::RBrace => brace_count -= 1,
                TokenKind::LBrace => brace_count += 1,
                TokenKind::End => {
                    return Err(Error::new(
                        "unexpected end of file while parsing function",
                        vec![
                            (
                                decl_type.range.start..end,
                                "function declared here".to_string(),
                            ),
                            (tok.range, "missing closing brace here".to_string()),
                        ],
                    ))
                }
                _ => {}
            }
        }

        let body = self.buckets().add_array(body);
        return Ok(GlobalStmt {
            range: decl_type.range.start..end,
            kind: GlobalStmtKind::Func {
                return_type: decl_type,
                pointer_count,
                ident: ident,
                params,
                body,
            },
        });
    }
}
