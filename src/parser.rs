use crate::ast::*;
use crate::buckets::BucketList;
use crate::errors::Error;
use crate::lexer::{Lexer, Token, TokenKind};
use core::ops::Range;
use core::slice;

pub struct Parser1<'a, 'b> {
    pub _buckets: &'a mut BucketList<'b>,
    pub lexer: Lexer<'b>,
    pub token_stack: Vec<Token>,
}

impl<'a, 'b> Parser1<'a, 'b> {
    pub fn new(data: &'b str) -> Self {
        Self {
            _buckets: BucketList::new(),
            lexer: Lexer::new(data),
            token_stack: Vec::new(),
        }
    }

    pub fn peek(&mut self) -> Token {
        if self.token_stack.len() > 0 {
            return self.token_stack[self.token_stack.len() - 1].clone();
        }

        let tok = self.lexer.next();
        self.token_stack.push(tok.clone());
        return tok;
    }

    pub fn pop(&mut self) -> Token {
        match self.token_stack.pop() {
            Some(x) => x,
            None => self.lexer.next(),
        }
    }

    #[inline]
    pub fn parse_expr(&mut self) -> Result<Expr<'b>, Error> {
        return self.parse_assignment();
    }

    pub fn parse_assignment(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_ternary()
    }

    pub fn parse_ternary(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_bool_or()
    }

    pub fn parse_bool_or(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_bool_and()
    }

    pub fn parse_bool_and(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_bit_or()
    }

    pub fn parse_bit_or(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_bit_xor()
    }

    pub fn parse_bit_xor(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_bit_and()
    }

    pub fn parse_bit_and(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_equality()
    }

    pub fn parse_equality(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_comparison()
    }

    pub fn parse_comparison(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_shift()
    }

    pub fn parse_shift(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_add()
    }

    pub fn parse_add(&mut self) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_multiply()?;
        loop {
            let start = expr.range.start;
            match self.peek().kind {
                TokenKind::Plus => {
                    self.pop();
                    let right = self.parse_multiply()?;
                    let end = right.range.end;
                    let left = self._buckets.add(expr);
                    let right = self._buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::Add(left, right),
                        range: start..end,
                    };
                }
                TokenKind::Dash => {
                    self.pop();
                    let right = self.parse_multiply()?;
                    let end = right.range.end;
                    let left = self._buckets.add(expr);
                    let right = self._buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::Subtract(left, right),
                        range: start..end,
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_multiply(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_prefix()
    }

    pub fn parse_prefix(&mut self) -> Result<Expr<'b>, Error> {
        self.parse_postfix()
    }

    pub fn parse_postfix(&mut self) -> Result<Expr<'b>, Error> {
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
                let params = self._buckets.add_array(params);
                return Ok(Expr {
                    kind: ExprKind::Call {
                        function: self._buckets.add(operand),
                        params,
                    },
                    range: start..end,
                });
            }
            TokenKind::PlusPlus => {
                return Ok(Expr {
                    kind: ExprKind::PostIncr(self._buckets.add(operand)),
                    range: start..self.pop().range.end,
                })
            }
            TokenKind::DashDash => {
                return Ok(Expr {
                    kind: ExprKind::PostDecr(self._buckets.add(operand)),
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
                        ptr: self._buckets.add(operand),
                        index: self._buckets.add(index),
                    },
                    range: start..end,
                });
            }
            TokenKind::Arrow => {
                self.pop();

                let (member, range) = Error::expect_ident(&self.pop())?;

                return Ok(Expr {
                    kind: ExprKind::PtrMember {
                        expr: self._buckets.add(operand),
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
                        expr: self._buckets.add(operand),
                        member,
                    },
                    range: start..range.end,
                });
            }
            _ => return Ok(operand),
        }
    }

    pub fn parse_atom(&mut self) -> Result<Expr<'b>, Error> {
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
            TokenKind::LParen => {
                let mut expr = self.parse_expr()?;
                let mut expr_list = Vec::new();
                let start = expr.range.start;
                while self.peek().kind == TokenKind::Comma {
                    expr_list.push(expr);
                    self.pop();
                    expr = self.parse_expr()?;
                }

                Error::expect_rparen(&tok.range, &self.pop())?;

                if expr_list.len() == 0 {
                    return Ok(expr);
                } else {
                    let end = expr.range.end;
                    expr_list.push(expr);
                    return Ok(Expr {
                        kind: ExprKind::List(self._buckets.add_array(expr_list)),
                        range: start..end,
                    });
                }
            }
            _ => return Err(Error::unexpected_token("expression", &tok)),
        }
    }

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

    fn parse_simple_decl(&mut self) -> Result<Decl<'b>, Error> {
        let mut pointer_count: u32 = 0;
        while self.peek().kind == TokenKind::Star {
            pointer_count += 1;
            self.pop();
        }

        let (ident, range) = Error::expect_ident(&self.pop())?;
        let tok = self.peek();
        let expr;
        if tok.kind == TokenKind::Eq {
            self.pop();
            expr = self.parse_expr()?;
        } else {
            expr = Expr {
                kind: ExprKind::Uninit,
                range: range.clone(),
            };
        }

        return Ok(Decl {
            pointer_count,
            ident,
            range,
            expr,
        });
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

    fn parse_multi_decl(&mut self) -> Result<(Vec<Decl<'b>>, Decl<'b>), Error> {
        let mut decl = self.parse_simple_decl()?;
        let mut tok = self.peek();
        let mut decls = Vec::new();

        while tok.kind == TokenKind::Comma {
            self.pop();
            decls.push(decl);
            decl = self.parse_simple_decl()?;
            tok = self.peek();
        }

        return Ok((decls, decl));
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
                            members: Some(self._buckets.add_array(decls)),
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

        let header_start = decl_type.range.start;
        let (mut decls, decl) = self.parse_multi_decl()?;
        let tok = self.pop();

        if decls.len() > 0 {
            Error::expect_semicolon(&tok)?;
            let range_end = decl.range.end;
            decls.push(decl);
            return Ok(GlobalStmt {
                range: header_start..range_end,
                kind: GlobalStmtKind::Decl {
                    decl_type,
                    decls: self._buckets.add_array(decls),
                },
            });
        }

        if decl.expr.kind != ExprKind::Uninit {
            Error::expect_semicolon(&tok)?;
            let range_end = decl.range.end;
            decls.push(decl);
            return Ok(GlobalStmt {
                range: decl_type.range.start..range_end,
                kind: GlobalStmtKind::Decl {
                    decl_type,
                    decls: self._buckets.add_array(decls),
                },
            });
        }

        if tok.kind == TokenKind::Semicolon {
            let range_end = decl.range.end;
            decls.push(decl);
            return Ok(GlobalStmt {
                range: decl_type.range.start..range_end,
                kind: GlobalStmtKind::Decl {
                    decl_type,
                    decls: self._buckets.add_array(decls),
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
        let params = self._buckets.add_array(params);
        let end_decl_tok = self.pop();
        if end_decl_tok.kind == TokenKind::Semicolon {
            return Ok(GlobalStmt {
                range: decl_type.range.start..end,
                kind: GlobalStmtKind::FuncDecl {
                    pointer_count: decl.pointer_count,
                    return_type: decl_type,
                    ident: decl.ident,
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

        let mut body = Vec::new();
        while self.peek().kind != TokenKind::RBrace {
            body.push(self.parse_stmt()?);
        }
        let tok = self.pop();

        let body = self._buckets.add_array(body);
        return Ok(GlobalStmt {
            range: decl_type.range.start..end,
            kind: GlobalStmtKind::Func {
                return_type: decl_type,
                pointer_count: decl.pointer_count,
                ident: decl.ident,
                params,
                body,
            },
        });
    }

    pub fn parse_block(&mut self) -> Result<(&'b [Stmt<'b>], Range<u32>), Error> {
        let (stmts, range) = match self.peek().kind {
            TokenKind::LBrace => {
                let start = self.pop().range.start;

                let mut stmts = Vec::new();
                while self.peek().kind != TokenKind::RBrace {
                    stmts.push(self.parse_stmt()?);
                }
                let end = self.pop().range.end;

                if stmts.len() == 1 {
                    if let StmtKind::Block(stmts) = stmts[0].kind {
                        (stmts, start..end)
                    } else {
                        (&*self._buckets.add_array(stmts), start..end)
                    }
                } else {
                    (&*self._buckets.add_array(stmts), start..end)
                }
            }
            _ => {
                let stmt = self.parse_stmt()?;
                let range = stmt.range.clone();
                (slice::from_ref(self._buckets.add(stmt)), range)
            }
        };

        return Ok((stmts, range));
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt<'b>, Error> {
        let tok = self.peek();
        match &tok.kind {
            TokenKind::For => {
                let start = self.pop().range.start;

                let lparen_tok = self.pop();
                Error::expect_lparen(&lparen_tok)?;

                let is_decl = match &self.peek().kind {
                    TokenKind::Char | TokenKind::Int | TokenKind::Void => true,
                    _ => false,
                };

                let first_part = if is_decl {
                    let decl_type = self.parse_simple_type_prefix()?;
                    let (mut decls, decl) = self.parse_multi_decl()?;
                    decls.push(decl);
                    Ok((decl_type, decls))
                } else {
                    Err(self.parse_expr()?)
                };
                Error::expect_semicolon(&self.pop())?;

                let condition = self.parse_expr()?;
                Error::expect_semicolon(&self.pop())?;

                let post_expr = self.parse_expr()?;
                Error::expect_semicolon(&self.pop())?;

                Error::expect_rparen(&lparen_tok.range, &self.pop())?;

                let (body, body_range) = self.parse_block()?;

                match first_part {
                    Ok((decl_type, decls)) => {
                        return Ok(Stmt {
                            kind: StmtKind::ForDecl {
                                at_start_decl_type: decl_type,
                                at_start: self._buckets.add_array(decls),
                                condition,
                                post_expr,
                                body,
                            },
                            range: start..body_range.end,
                        });
                    }
                    Err(at_start) => {
                        return Ok(Stmt {
                            kind: StmtKind::For {
                                at_start,
                                condition,
                                post_expr,
                                body,
                            },
                            range: start..body_range.end,
                        })
                    }
                }
            }
            TokenKind::Semicolon => {
                return Ok(Stmt {
                    kind: StmtKind::Nop,
                    range: self.pop().range,
                });
            }
            TokenKind::If => {
                let start = self.pop().range.start;

                let lparen_tok = self.pop();
                Error::expect_lparen(&self.pop())?;
                let if_cond = self.parse_expr()?;
                Error::expect_rparen(&lparen_tok.range, &self.pop())?;

                let (if_body, if_body_range) = self.parse_block()?;

                if self.peek().kind != TokenKind::Else {
                    return Ok(Stmt {
                        kind: StmtKind::Branch {
                            if_cond,
                            if_body,
                            else_body: None,
                        },
                        range: start..if_body_range.end,
                    });
                }

                self.pop();
                let (else_body, else_body_range) = self.parse_block()?;

                return Ok(Stmt {
                    kind: StmtKind::Branch {
                        if_cond,
                        if_body,
                        else_body: Some(else_body),
                    },
                    range: start..else_body_range.end,
                });
            }
            TokenKind::LBrace => {
                let (stmts, range) = self.parse_block()?;
                if stmts.len() == 0 {
                    return Ok(Stmt {
                        kind: StmtKind::Nop,
                        range,
                    });
                } else {
                    return Ok(Stmt {
                        kind: StmtKind::Block(stmts),
                        range,
                    });
                }
            }
            TokenKind::Return => {
                self.pop();

                if self.peek().kind == TokenKind::Semicolon {
                    self.pop();
                    return Ok(Stmt {
                        kind: StmtKind::Ret,
                        range: tok.range,
                    });
                }

                let expr = self.parse_expr()?;
                Error::expect_semicolon(&self.pop())?;

                return Ok(Stmt {
                    kind: StmtKind::RetVal(expr),
                    range: tok.range,
                });
            }
            TokenKind::Int | TokenKind::Char | TokenKind::Void => {
                let decl_type = self.parse_simple_type_prefix()?;
                let range_start = decl_type.range.start;
                let (mut decls, decl) = self.parse_multi_decl()?;
                let range_end = decl.range.end;
                decls.push(decl);
                Error::expect_semicolon(&self.pop())?;

                return Ok(Stmt {
                    range: range_start..range_end,
                    kind: StmtKind::Decl {
                        decl_type,
                        decls: self._buckets.add_array(decls),
                    },
                });
            }
            _ => {
                let expr = self.parse_expr()?;
                Error::expect_semicolon(&self.pop())?;

                return Ok(Stmt {
                    range: expr.range.clone(),
                    kind: StmtKind::Expr(expr),
                });
            }
        }
    }
}
