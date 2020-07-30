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

pub trait ExprParser<'a>: Parser<'a> {
    fn parse_expr(&mut self) -> Result<Expr<'a>, Error> {
        return self.parse_postfix();
    }

    fn parse_postfix(&mut self) -> Result<Expr<'a>, Error> {
        let operand = self.parse_atom()?;
        let start = operand.range.start;

        match self.peek().kind {
            TokenKind::LParen => {
                self.pop();
                let mut params = Vec::new();
                let rparen_tok = self.peek();
                if rparen_tok.kind != TokenKind::RParen {
                    let param = self.parse_expr()?;
                    params.push(param);
                    let mut comma_tok = self.peek();
                    while comma_tok.kind == TokenKind::Comma {
                        self.pop();
                        params.push(self.parse_expr()?);
                        comma_tok = self.peek();
                    }

                    if comma_tok.kind != TokenKind::RParen {
                        let range = comma_tok.range.clone();
                        return Err(Error::new(
                            "unexpected token when parsing end of function declaration",
                            vec![
                                (
                                    params.pop().unwrap().range,
                                    "interpreted as parameter declaration".to_string(),
                                ),
                                (range, format!("interpreted as {:?}", comma_tok)),
                            ],
                        ));
                    }
                }

                let end = self.pop().range.end;
                let params = self.buckets().add_array(params);
                return Ok(Expr {
                    kind: ExprKind::Call {
                        function: self.buckets().add(operand),
                        params,
                    },
                    range: start..end,
                });
            }
            TokenKind::PlusPlus => {
                return Ok(Expr {
                    kind: ExprKind::PostIncr(self.buckets().add(operand)),
                    range: start..self.pop().range.end,
                })
            }
            TokenKind::DashDash => {
                return Ok(Expr {
                    kind: ExprKind::PostDecr(self.buckets().add(operand)),
                    range: start..self.pop().range.end,
                })
            }
            TokenKind::LBracket => {
                self.pop();
                let index = self.parse_expr()?;
                let end = index.range.end;
                Error::expect_rbracket(&self.pop())?;
                return Ok(Expr {
                    kind: ExprKind::Index {
                        ptr: self.buckets().add(operand),
                        index: self.buckets().add(index),
                    },
                    range: start..end,
                });
            }
            TokenKind::Arrow => {
                self.pop();

                let (member, range) = Error::expect_ident(&self.pop())?;

                return Ok(Expr {
                    kind: ExprKind::PtrMember {
                        expr: self.buckets().add(operand),
                        member,
                    },
                    range: start..range.end,
                });
            }
            TokenKind::Dot => {
                self.pop();

                let (member, range) = Error::expect_ident(&self.pop())?;

                return Ok(Expr {
                    kind: ExprKind::Member {
                        expr: self.buckets().add(operand),
                        member,
                    },
                    range: start..range.end,
                });
            }
            _ => return Ok(operand),
        }
    }

    fn parse_atom(&mut self) -> Result<Expr<'a>, Error> {
        let tok = self.pop();
        match tok.kind {
            TokenKind::Ident(i) => {
                return Ok(Expr {
                    kind: ExprKind::Ident(i),
                    range: tok.range,
                })
            }
            TokenKind::IntLiteral(i) => {
                return Ok(Expr {
                    kind: ExprKind::IntLiteral(i),
                    range: tok.range,
                })
            }
            _ => return Err(Error::new("", vec![])),
        }
    }
}

pub trait TypeParser<'a>: ExprParser<'a> {
    fn parse_type_prefix(&mut self) -> Result<ASTType<'a>, Error> {
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
            TokenKind::Struct => { /* fallthrough */ }
            _ => {
                return Err(Error::new(
                    "unexpected token while parsing type",
                    vec![(
                        tok.range.clone(),
                        format!("this was interpreted as {:?}", tok),
                    )],
                ));
            }
        }

        let ident_tok = self.peek();
        let mut ident = None;
        if let TokenKind::Ident(id) = ident_tok.kind {
            self.pop();
            if self.peek().kind != TokenKind::LBrace {
                return Ok(ASTType {
                    kind: ASTTypeKind::Struct { ident: id },
                    range: tok.range.start..ident_tok.range.end,
                    pointer_count: 0,
                });
            }
            ident = Some(id);
        }

        Error::expect_lbrace(&self.pop())?;
        let mut members = Vec::new();

        while self.peek().kind != TokenKind::RBrace {
            let decl = self.parse_simple_decl()?;
            Error::expect_semicolon(&self.pop())?;
            members.push(decl);
        }

        let members = self.buckets().add_array(members);
        return Ok(ASTType {
            kind: ASTTypeKind::StructDefn { ident, members },
            range: tok.range.start..self.pop().range.end,
            pointer_count: 0,
        });
    }

    fn parse_simple_decl(&mut self) -> Result<Decl<'a>, Error> {
        let tok = self.peek();
        let start = tok.range.start;
        let mut decl_type = self.parse_type_prefix()?;

        while self.peek().kind == TokenKind::Star {
            self.pop();
            decl_type.pointer_count += 1;
        }

        let tok = self.peek();
        if let TokenKind::Ident(id) = tok.kind {
            self.pop();

            let eq_tok = self.peek();
            if eq_tok.kind == TokenKind::Eq {
                self.pop();

                let expr = self.parse_expr()?;
                let end = expr.range.end;
                return Ok(Decl {
                    kind: DeclKind::WithValue {
                        decl_type,
                        ident: id,
                        value: expr,
                    },
                    range: start..end,
                });
            } else {
                return Ok(Decl {
                    kind: DeclKind::Uninit {
                        decl_type,
                        ident: id,
                    },
                    range: start..tok.range.end,
                });
            }
        }

        return Ok(Decl {
            kind: DeclKind::Type(decl_type),
            range: start..tok.range.end,
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

impl<'a, 'b> ExprParser<'b> for Parser1<'a, 'b> {}
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
        let decl = self.parse_simple_decl()?;

        let (decl_type, ident) = match decl.kind {
            DeclKind::Type(_) | DeclKind::WithValue { .. } => {
                Error::expect_semicolon(&self.pop())?;
                return Ok(GlobalStmt {
                    range: decl.range.clone(),
                    kind: GlobalStmtKind::Decl(decl),
                });
            }
            DeclKind::Uninit { decl_type, ident } => (decl_type, ident),
        };

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
            return Err(Error::new(
                "unexpected token when parsing function declaration",
                vec![
                    (
                        decl.range.clone(),
                        "interpreted this as declaration".to_string(),
                    ),
                    (tok.range, "expected '(' here".to_string()),
                ],
            ));
        }

        let mut params = Vec::new();
        let rparen_tok = self.peek();
        if rparen_tok.kind != TokenKind::RParen {
            let param = self.parse_simple_decl()?;
            params.push(param);
            let mut comma_tok = self.peek();
            while comma_tok.kind == TokenKind::Comma {
                self.pop();
                params.push(self.parse_simple_decl()?);
                comma_tok = self.peek();
            }

            if comma_tok.kind != TokenKind::RParen {
                let range = comma_tok.range.clone();
                return Err(Error::new(
                    "unexpected token when parsing end of function declaration",
                    vec![
                        (
                            params.pop().unwrap().range,
                            "interpreted as parameter declaration".to_string(),
                        ),
                        (range, format!("interpreted as {:?}", comma_tok)),
                    ],
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
