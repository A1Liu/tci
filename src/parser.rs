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
                    pointer_count: 0,
                })
            }
            TokenKind::Void => {
                return Ok(ASTType {
                    kind: ASTTypeKind::Void,
                    range: tok.range,
                    pointer_count: 0,
                })
            }
            TokenKind::Char => {
                return Ok(ASTType {
                    kind: ASTTypeKind::Char,
                    range: tok.range,
                    pointer_count: 0,
                })
            }
            TokenKind::Struct => panic!("struct should be handled by another function"),
            _ => return Err(Error::unexpected_token("type", &tok)),
        }
    }

    fn parse_simple_decl(&mut self, mut decl_type: ASTType) -> Result<Decl<'a>, Error> {
        while self.peek().kind == TokenKind::Star {
            self.pop();
            decl_type.pointer_count += 1;
        }

        let (ident, range) = Error::expect_ident(&self.pop())?;

        let eq_tok = self.peek();
        if eq_tok.kind != TokenKind::Eq {
            return Ok(Decl {
                range: decl_type.range.start..range.end,
                kind: DeclKind::Uninit { decl_type, ident },
            });
        }

        self.pop();

        let mut toks = Vec::new();
        let mut tok = self.pop();
        while tok.kind != TokenKind::Semicolon && tok.kind != TokenKind::End {
            toks.push(tok);
            tok = self.pop();
        }

        if tok.kind == TokenKind::End {
            return Err(Error::new("unexpected end of file", vec![]));
        }

        return Ok(Decl {
            range: decl_type.range.start..tok.range.start,
            kind: DeclKind::WithValue {
                decl_type,
                ident,
                value: self.buckets().add_array(toks),
            },
        });
    }

    fn parse_simple_decl_local(&mut self) -> Result<Decl<'a>, Error> {
        let decl_type = match self.peek().kind {
            TokenKind::Struct => {
                let start = self.pop().range.start;
                let (ident, range) = Error::expect_ident(&self.pop())?;
                let tok = self.pop();

                ASTType {
                    kind: ASTTypeKind::Struct { ident },
                    range: start..range.end,
                    pointer_count: 0,
                }
            }
            _ => self.parse_simple_type_prefix()?,
        };

        self.parse_simple_decl(decl_type)
    }

    fn parse_inner_struct_decl(&mut self) -> Result<InnerStructDecl, Error> {
        let mut decl_type = match self.peek().kind {
            TokenKind::Struct => {
                let start = self.pop().range.start;
                let (ident, range) = Error::expect_ident(&self.pop())?;

                ASTType {
                    kind: ASTTypeKind::Struct { ident },
                    range: start..range.end,
                    pointer_count: 0,
                }
            }
            _ => self.parse_simple_type_prefix()?,
        };

        while self.peek().kind == TokenKind::Star {
            self.pop();
            decl_type.pointer_count += 1;
        }

        let (ident, range) = Error::expect_ident(&self.pop())?;

        return Ok(InnerStructDecl {
            range: decl_type.range.start..range.end,
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
                    pointer_count: 0,
                }
            }
            _ => self.parse_simple_type_prefix()?,
        };

        let decl = self.parse_simple_decl(decl_type)?;
        let (decl_type, ident) = match decl.kind {
            DeclKind::WithValue { .. } => {
                Error::expect_semicolon(&self.pop())?;
                return Ok(GlobalStmt {
                    range: decl.range.clone(),
                    kind: GlobalStmtKind::Decl(decl),
                });
            }
            DeclKind::Uninit { decl_type, ident } => (decl_type, ident),
        };

        let header_start = decl_type.range.start;
        let tok = self.pop();
        if tok.kind == TokenKind::Semicolon {
            return Ok(GlobalStmt {
                range: decl.range.clone(),
                kind: GlobalStmtKind::Decl(Decl {
                    kind: DeclKind::Uninit { decl_type, ident },
                    range: decl.range,
                }),
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
                kind: GlobalStmtKind::FuncDecl {
                    return_type: decl_type,
                    ident: ident,
                    params,
                },
                range: decl.range.start..end,
            });
        }

        if end_decl_tok.kind != TokenKind::LBrace {
            return Err(Error::new(
                "unexpected token when parsing ending of function declaration",
                vec![
                    (
                        decl.range.start..end,
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
                        (decl.range.start..end, "function declared here".to_string()),
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
                            (decl.range.start..end, "function declared here".to_string()),
                            (tok.range, "missing closing brace here".to_string()),
                        ],
                    ))
                }
                _ => {}
            }
        }

        let body = self.buckets().add_array(body);
        return Ok(GlobalStmt {
            kind: GlobalStmtKind::Func {
                return_type: decl_type,
                ident: ident,
                params,
                body,
            },
            range: decl.range.start..end,
        });
    }
}
