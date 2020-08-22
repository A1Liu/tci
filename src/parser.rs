use crate::ast::*;
use crate::buckets::BucketListRef;
use crate::lexer::{Token, TokenKind};
use crate::*;
use core::slice;

pub struct Parser<'a, 'b> {
    pub buckets: BucketListRef<'b>,
    pub file_id: u32,
    pub tokens: &'a [Token],
    pub current: usize,
}

pub fn parse_tokens<'a, 'b>(
    buckets: BucketListRef<'b>,
    file_id: u32,
    tokens: &'a [Token],
) -> Result<Vec<GlobalStmt<'b>>, Error> {
    let mut parser = Parser::new(buckets, file_id, tokens);
    let mut parse_result = Vec::new();
    loop {
        let decl = parser.parse_global_decl()?;
        parse_result.push(decl);

        if parser.peek().kind == lexer::TokenKind::End {
            break;
        }
    }

    return Ok(parse_result);
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(buckets: BucketListRef<'b>, file_id: u32, tokens: &'a [Token]) -> Self {
        Self {
            buckets,
            tokens,
            file_id,
            current: 0,
        }
    }

    pub fn peek(&self) -> Token {
        return *self.tokens.get(self.current).unwrap_or(&Token {
            kind: TokenKind::End,
            range: r(0, 0),
        });
    }

    pub fn pop(&mut self) -> Token {
        let current = self.current;
        self.current += 1;
        return *self.tokens.get(current).unwrap_or(&Token {
            kind: TokenKind::End,
            range: r(0, 0),
        });
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
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Add, left, right),
                        range: r(start, end),
                    };
                }
                TokenKind::Dash => {
                    self.pop();
                    let right = self.parse_multiply()?;
                    let end = right.range.end;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Sub, left, right),
                        range: r(start, end),
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
                        return Err(error!(
                            "unexpected token when parsing end of function declaration",
                            params.pop().unwrap().range,
                            self.file_id,
                            "interpreted as parameter declaration".to_string(),
                            comma_tok.range,
                            self.file_id,
                            format!("interpreted as {:?}", comma_tok)
                        ));
                    }
                }

                let end = self.pop().range.end;
                let params = self.buckets.add_array(params);
                return Ok(Expr {
                    kind: ExprKind::Call {
                        function: self.buckets.add(operand),
                        params,
                    },
                    range: r(start, end),
                });
            }
            TokenKind::PlusPlus => {
                return Ok(Expr {
                    kind: ExprKind::PostIncr(self.buckets.add(operand)),
                    range: r(start, self.pop().range.end),
                })
            }
            TokenKind::DashDash => {
                return Ok(Expr {
                    kind: ExprKind::PostDecr(self.buckets.add(operand)),
                    range: r(start, self.pop().range.end),
                })
            }
            TokenKind::LBracket => {
                self.pop();
                let index = self.parse_expr()?;
                let end = index.range.end;
                self.expect_rbracket()?;
                return Ok(Expr {
                    kind: ExprKind::Index {
                        ptr: self.buckets.add(operand),
                        index: self.buckets.add(index),
                    },
                    range: r(start, end),
                });
            }
            TokenKind::Arrow => {
                self.pop();

                let (member, range) = self.expect_ident()?;

                return Ok(Expr {
                    kind: ExprKind::PtrMember {
                        expr: self.buckets.add(operand),
                        member,
                    },
                    range: r(start, range.end),
                });
            }
            TokenKind::Dot => {
                self.pop();

                let (member, range) = self.expect_ident()?;

                return Ok(Expr {
                    kind: ExprKind::Member {
                        expr: self.buckets.add(operand),
                        member,
                    },
                    range: r(start, range.end),
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

                self.expect_rparen(&tok.range)?;

                if expr_list.len() == 0 {
                    return Ok(expr);
                } else {
                    let end = expr.range.end;
                    expr_list.push(expr);
                    return Ok(Expr {
                        kind: ExprKind::List(self.buckets.add_array(expr_list)),
                        range: r(start, end),
                    });
                }
            }
            _ => return Err(self.unexpected_token("expression", &tok)),
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
            _ => return Err(self.unexpected_token("type", &tok)),
        }
    }

    fn parse_simple_decl(&mut self) -> Result<Decl<'b>, Error> {
        let mut pointer_count: u32 = 0;
        while self.peek().kind == TokenKind::Star {
            pointer_count += 1;
            self.pop();
        }

        let (ident, range) = self.expect_ident()?;
        let tok = self.peek();
        let expr;
        if tok.kind == TokenKind::Eq {
            self.pop();
            expr = self.parse_expr()?;
        } else {
            expr = Expr {
                kind: ExprKind::Uninit,
                range: range,
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
                let (ident, range) = self.expect_ident()?;

                ASTType {
                    kind: ASTTypeKind::Struct { ident },
                    range: r(start, range.end),
                }
            }
            _ => self.parse_simple_type_prefix()?,
        };

        let mut pointer_count: u32 = 0;
        while self.peek().kind == TokenKind::Star {
            self.pop();
            pointer_count += 1;
        }

        let (ident, range) = self.expect_ident()?;

        return Ok(InnerStructDecl {
            range: r(decl_type.range.start, range.end),
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
                let (ident, ident_range) = self.expect_ident()?;
                let tok = self.peek();

                if tok.kind == TokenKind::LBrace {
                    // parse as type definition
                    self.pop();

                    let mut decls = Vec::new();
                    while self.peek().kind != TokenKind::RBrace {
                        let decl = self.parse_inner_struct_decl()?;
                        decls.push(decl);
                        self.eat_semicolon()?;
                    }

                    let end = self.pop().range.end;
                    self.eat_semicolon()?;

                    return Ok(GlobalStmt {
                        kind: GlobalStmtKind::StructDecl(StructDecl {
                            ident,
                            ident_range,
                            range: r(start, end),
                            members: Some(self.buckets.add_array(decls)),
                        }),
                        range: r(start, end),
                    });
                }

                if tok.kind == TokenKind::Semicolon {
                    self.pop();

                    return Ok(GlobalStmt {
                        range: r(start, ident_range.end),
                        kind: GlobalStmtKind::StructDecl(StructDecl {
                            ident,
                            range: r(start, ident_range.end),
                            ident_range,
                            members: None,
                        }),
                    });
                }

                ASTType {
                    kind: ASTTypeKind::Struct { ident },
                    range: r(start, ident_range.end),
                }
            }
            _ => self.parse_simple_type_prefix()?,
        };

        let header_start = decl_type.range.start;
        let (mut decls, decl) = self.parse_multi_decl()?;
        let tok = self.pop();

        if decls.len() > 0 {
            self.expect_semicolon(&tok)?;
            let range_end = decl.range.end;
            decls.push(decl);
            return Ok(GlobalStmt {
                range: r(header_start, range_end),
                kind: GlobalStmtKind::Decl {
                    decl_type,
                    decls: self.buckets.add_array(decls),
                },
            });
        }

        if decl.expr.kind != ExprKind::Uninit {
            self.expect_semicolon(&tok)?;
            let range_end = decl.range.end;
            decls.push(decl);
            return Ok(GlobalStmt {
                range: r(decl_type.range.start, range_end),
                kind: GlobalStmtKind::Decl {
                    decl_type,
                    decls: self.buckets.add_array(decls),
                },
            });
        }

        if tok.kind == TokenKind::Semicolon {
            let range_end = decl.range.end;
            decls.push(decl);
            return Ok(GlobalStmt {
                range: r(decl_type.range.start, range_end),
                kind: GlobalStmtKind::Decl {
                    decl_type,
                    decls: self.buckets.add_array(decls),
                },
            });
        }

        if tok.kind != TokenKind::LParen {
            return Err(self.unexpected_token("function declaration", &tok));
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
                return Err(self.unexpected_token("end of function declaration", &comma_tok));
            }
        }

        let end = self.pop().range.end;
        let params = self.buckets.add_array(params);
        let end_decl_tok = self.pop();
        if end_decl_tok.kind == TokenKind::Semicolon {
            return Ok(GlobalStmt {
                range: r(decl_type.range.start, end),
                kind: GlobalStmtKind::FuncDecl {
                    pointer_count: decl.pointer_count,
                    return_type: decl_type,
                    ident: decl.ident,
                    params,
                },
            });
        }

        if end_decl_tok.kind != TokenKind::LBrace {
            return Err(error!(
                "unexpected token when parsing ending of function declaration",
                r(decl_type.range.start, end),
                self.file_id,
                "this was parsed as a function declaration".to_string(),
                end_decl_tok.range,
                self.file_id,
                "expected a ';' or '{' here".to_string()
            ));
        }

        let mut body = Vec::new();
        while self.peek().kind != TokenKind::RBrace {
            body.push(self.parse_stmt()?);
        }
        let tok = self.pop();

        let body = self.buckets.add_array(body);
        return Ok(GlobalStmt {
            range: r(decl_type.range.start, end),
            kind: GlobalStmtKind::Func {
                return_type: decl_type,
                pointer_count: decl.pointer_count,
                ident: decl.ident,
                params,
                body,
            },
        });
    }

    pub fn parse_block(&mut self) -> Result<(&'b [Stmt<'b>], Range), Error> {
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
                        (stmts, r(start, end))
                    } else {
                        (&*self.buckets.add_array(stmts), r(start, end))
                    }
                } else {
                    (&*self.buckets.add_array(stmts), r(start, end))
                }
            }
            _ => {
                let stmt = self.parse_stmt()?;
                let range = stmt.range;
                (slice::from_ref(self.buckets.add(stmt)), range)
            }
        };

        return Ok((stmts, range));
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt<'b>, Error> {
        let tok = self.peek();
        match &tok.kind {
            TokenKind::For => {
                let start = self.pop().range.start;

                let lparen_tok = self.expect_lparen()?;

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
                self.eat_semicolon()?;

                let condition = self.parse_expr()?;
                self.eat_semicolon()?;

                let post_expr = self.parse_expr()?;
                self.eat_semicolon()?;

                self.expect_rparen(&lparen_tok.range)?;

                let (body, body_range) = self.parse_block()?;

                match first_part {
                    Ok((decl_type, decls)) => {
                        return Ok(Stmt {
                            kind: StmtKind::ForDecl {
                                at_start_decl_type: decl_type,
                                at_start: self.buckets.add_array(decls),
                                condition,
                                post_expr,
                                body,
                            },
                            range: r(start, body_range.end),
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
                            range: r(start, body_range.end),
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

                let lparen_tok = self.expect_lparen()?;
                let if_cond = self.parse_expr()?;
                self.expect_rparen(&lparen_tok.range)?;

                let (if_body, if_body_range) = self.parse_block()?;

                if self.peek().kind != TokenKind::Else {
                    return Ok(Stmt {
                        kind: StmtKind::Branch {
                            if_cond,
                            if_body,
                            else_body: None,
                        },
                        range: r(start, if_body_range.end),
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
                    range: r(start, else_body_range.end),
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
                self.eat_semicolon()?;

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
                self.eat_semicolon()?;

                return Ok(Stmt {
                    range: r(range_start, range_end),
                    kind: StmtKind::Decl {
                        decl_type,
                        decls: self.buckets.add_array(decls),
                    },
                });
            }
            _ => {
                let expr = self.parse_expr()?;
                self.eat_semicolon()?;

                return Ok(Stmt {
                    range: expr.range,
                    kind: StmtKind::Expr(expr),
                });
            }
        }
    }

    pub fn unexpected_token(&self, parsing_what: &str, tok: &Token) -> Error {
        return error!(
            "unexpected token while parsing ",
            tok.range,
            self.file_id,
            format!("this was interpreted as {:?}", tok)
        );
    }

    pub fn expect_ident(&mut self) -> Result<(u32, Range), Error> {
        let tok = self.pop();
        if let TokenKind::Ident(id) = tok.kind {
            return Ok((id, tok.range.clone()));
        } else {
            return Err(error!(
                "expected identifier token, got something else instead",
                tok.range,
                self.file_id,
                format!(
                    "this was interpreted as {:?} when it should be an identifier",
                    tok
                )
            ));
        }
    }

    pub fn expect_rbracket(&mut self) -> Result<(), Error> {
        let tok = self.pop();
        if tok.kind != TokenKind::RBracket {
            return Err(error!(
                "expected ']' token, got something else instead",
                tok.range,
                self.file_id,
                format!("this was interpreted as {:?} when it should be a ']'", tok)
            ));
        }
        return Ok(());
    }

    pub fn expect_lbrace(&mut self) -> Result<(), Error> {
        let tok = self.pop();
        if tok.kind != TokenKind::LBrace {
            return Err(error!(
                "expected '{' token, got something else instead",
                tok.range,
                self.file_id,
                format!("this was interpreted as {:?} when it should be a '{{'", tok)
            ));
        }
        return Ok(());
    }

    pub fn expect_rparen(&mut self, matching_tok: &Range) -> Result<(), Error> {
        let tok = self.pop();
        if tok.kind != TokenKind::RParen {
            return Err(error!(
                "expected ')' token, got something else instead",
                tok.range,
                self.file_id,
                format!("this was interpreted as {:?} when it should be a ')'", tok),
                *matching_tok,
                self.file_id,
                "matching left paren here".to_string()
            ));
        }
        return Ok(());
    }

    pub fn expect_lparen(&mut self) -> Result<Token, Error> {
        let tok = self.pop();
        if tok.kind != TokenKind::LParen {
            return Err(error!(
                "expected '(' token, got something else instead",
                tok.range,
                self.file_id,
                format!("this was interpreted as {:?} when it should be a '('", tok)
            ));
        }
        return Ok(tok);
    }

    pub fn eat_semicolon(&mut self) -> Result<(), Error> {
        let tok = self.pop();
        return self.expect_semicolon(&tok);
    }

    pub fn expect_semicolon(&self, tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::Semicolon {
            return Err(error!(
                "expected ';' token, got something else instead",
                tok.range,
                self.file_id,
                format!("this was interpreted as {:?} when it should be a ';'", tok)
            ));
        }
        return Ok(());
    }
}
