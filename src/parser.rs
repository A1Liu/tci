use crate::ast::*;
use crate::buckets::BucketListRef;
use crate::lexer::*;
use crate::util::*;
use core::slice;

pub struct Parser<'a, 'b> {
    pub buckets: BucketListRef<'b>,
    pub file_id: u32,
    pub tokens: &'a [Token<'a>],
    pub current: usize,
}

pub fn parse_tokens<'a, 'b>(
    buckets: BucketListRef<'b>,
    file_id: u32,
    tokens: &'a [Token<'a>],
) -> Result<Vec<GlobalStmt<'b>>, Error> {
    let mut parser = Parser::new(buckets, file_id, tokens);
    let mut parse_result = Vec::new();
    loop {
        let decl = parser.parse_global_decl()?;
        parse_result.push(decl);

        if parser.peek_o().is_none() {
            break;
        }

        while let Some(next) = parser.buckets.next() {
            parser.buckets = next;
        }
    }

    return Ok(parse_result);
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(buckets: BucketListRef<'b>, file_id: u32, tokens: &'a [Token<'a>]) -> Self {
        Self {
            buckets,
            tokens,
            file_id,
            current: 0,
        }
    }

    pub fn peek_o(&self) -> Option<Token<'a>> {
        let tok = *self.tokens.get(self.current)?;
        return Some(tok);
    }

    pub fn peek(&self) -> Result<Token<'a>, Error> {
        let map_err = || error!("expected token");
        self.peek_o().ok_or_else(map_err)
    }

    pub fn pop(&mut self) -> Result<Token<'a>, Error> {
        let tok = self.peek()?;
        self.current += 1;
        Ok(tok)
    }

    #[inline]
    pub fn parse_expr(&mut self) -> Result<Expr<'b>, Error> {
        return self.parse_assignment();
    }

    pub fn parse_assignment(&mut self) -> Result<Expr<'b>, Error> {
        let left = self.parse_ternary()?;
        match self.peek()?.kind {
            TokenKind::Eq => {
                self.pop().unwrap();
                let right = self.parse_assignment()?;
                let (right, left) = self.buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::BinOp(BinOp::Assign, left, right),
                });
            }
            _ => {
                return Ok(left);
            }
        }
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
            let start_loc = expr.loc;
            match self.peek()?.kind {
                TokenKind::Plus => {
                    self.pop().expect("shouldn't fail");
                    let right = self.parse_multiply()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Add, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Dash => {
                    self.pop().expect("shouldn't fail");
                    let right = self.parse_multiply()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Sub, left, right),
                        loc: l_from(start_loc, end_loc),
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
        let tok = self.peek()?;
        match tok.kind {
            TokenKind::Amp => {
                self.pop().expect("shouldn't fail");
                let target = self.parse_prefix()?;
                let target = self.buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::Ref(target),
                });
            }
            TokenKind::Star => {
                self.pop().expect("shouldn't fail");
                let target = self.parse_prefix()?;
                let target = self.buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::Deref(target),
                });
            }
            _ => return self.parse_postfix(),
        }
    }

    pub fn parse_postfix(&mut self) -> Result<Expr<'b>, Error> {
        let mut operand = self.parse_atom()?;
        let start_loc = operand.loc;

        loop {
            match self.peek()?.kind {
                TokenKind::LParen => {
                    self.pop().expect("shouldn't fail");
                    let mut params = Vec::new();
                    let rparen_tok = self.peek()?;

                    if rparen_tok.kind != TokenKind::RParen {
                        let param = self.parse_expr()?;
                        params.push(param);
                        let mut comma_tok = self.peek()?;

                        while comma_tok.kind == TokenKind::Comma {
                            self.pop().expect("shouldn't fail");
                            params.push(self.parse_expr()?);
                            comma_tok = self.peek()?;
                        }

                        if comma_tok.kind != TokenKind::RParen {
                            return Err(error!(
                                "unexpected token when parsing end of function declaration",
                                params.pop().unwrap().loc,
                                "interpreted as parameter declaration".to_string(),
                                comma_tok.loc,
                                format!("interpreted as {:?}", comma_tok)
                            ));
                        }
                    }

                    let end_loc = self.pop().expect("shouldn't fail").loc;
                    let params = self.buckets.add_array(params);
                    operand = Expr {
                        loc: l_from(start_loc, end_loc),
                        kind: ExprKind::Call {
                            function: self.buckets.add(operand),
                            params,
                        },
                    };
                }
                TokenKind::PlusPlus => {
                    operand = Expr {
                        kind: ExprKind::PostIncr(self.buckets.add(operand)),
                        loc: l_from(start_loc, self.pop().expect("shouldn't fail").loc),
                    };
                }
                TokenKind::DashDash => {
                    operand = Expr {
                        kind: ExprKind::PostDecr(self.buckets.add(operand)),
                        loc: l_from(start_loc, self.pop()?.loc),
                    };
                }
                TokenKind::LBracket => {
                    self.pop().expect("shouldn't fail");
                    let index = self.parse_expr()?;
                    self.expect_rbracket()?;
                    operand = Expr {
                        loc: l_from(start_loc, index.loc),
                        kind: ExprKind::Index {
                            ptr: self.buckets.add(operand),
                            index: self.buckets.add(index),
                        },
                    };
                }
                TokenKind::Arrow => {
                    self.pop().expect("shouldn't fail");

                    let (member, loc) = self.expect_any_ident()?;

                    operand = Expr {
                        kind: ExprKind::PtrMember {
                            base: self.buckets.add(operand),
                            member,
                        },
                        loc: l_from(start_loc, loc),
                    };
                }
                TokenKind::Dot => {
                    self.pop().expect("shouldn't fail");

                    let (member, loc) = self.expect_any_ident()?;

                    operand = Expr {
                        kind: ExprKind::Member {
                            base: self.buckets.add(operand),
                            member,
                        },
                        loc: l_from(start_loc, loc),
                    };
                }
                _ => return Ok(operand),
            }
        }
    }

    pub fn parse_atom(&mut self) -> Result<Expr<'b>, Error> {
        let tok = self.pop()?;
        match tok.kind {
            TokenKind::Ident(i) => {
                return Ok(Expr {
                    kind: ExprKind::Ident(i),
                    loc: tok.loc,
                })
            }
            TokenKind::IntLiteral(i) => {
                return Ok(Expr {
                    kind: ExprKind::IntLiteral(i),
                    loc: tok.loc,
                })
            }
            TokenKind::LParen => {
                let start_loc = tok.loc;
                let mut expr = self.parse_expr()?;
                let mut expr_list = Vec::new();
                while self.peek()?.kind == TokenKind::Comma {
                    expr_list.push(expr);
                    self.pop().unwrap();
                    expr = self.parse_expr()?;
                }

                let end_loc = self.expect_rparen(tok.loc)?;

                if expr_list.len() == 0 {
                    return Ok(expr);
                } else {
                    expr_list.push(expr);
                    return Ok(Expr {
                        kind: ExprKind::List(self.buckets.add_array(expr_list)),
                        loc: l_from(start_loc, end_loc),
                    });
                }
            }
            TokenKind::StringLiteral(string) => {
                let mut string = string.to_string();
                let mut end_loc = tok.loc;
                while let TokenKind::StringLiteral(tstr) = self.peek()?.kind {
                    string.push_str(tstr);
                    end_loc = l_from(end_loc, self.pop().unwrap().loc);
                }

                return Ok(Expr {
                    kind: ExprKind::StringLiteral(self.buckets.add_str(&string)),
                    loc: l_from(tok.loc, end_loc),
                });
            }
            _ => return Err(self.unexpected_token("expression", &tok)),
        }
    }

    fn parse_simple_type_prefix(&mut self) -> Result<ASTType, Error> {
        let tok = self.pop()?;
        match tok.kind {
            TokenKind::Int => {
                return Ok(ASTType {
                    kind: ASTTypeKind::Int,
                    loc: tok.loc,
                })
            }
            TokenKind::Void => {
                return Ok(ASTType {
                    kind: ASTTypeKind::Void,
                    loc: tok.loc,
                })
            }
            TokenKind::Char => {
                return Ok(ASTType {
                    kind: ASTTypeKind::Char,
                    loc: tok.loc,
                })
            }
            TokenKind::Struct => panic!("struct should be handled by another function"),
            _ => return Err(self.unexpected_token("type", &tok)),
        }
    }

    fn parse_simple_decl(&mut self) -> Result<Decl<'b>, Error> {
        let mut pointer_count: u32 = 0;
        while self.peek()?.kind == TokenKind::Star {
            pointer_count += 1;
            self.pop().unwrap();
        }

        let (ident, ident_loc) = self.expect_ident()?;
        let tok = self.peek()?;
        let expr = if tok.kind == TokenKind::Eq {
            self.pop().unwrap();
            self.parse_expr()?
        } else {
            Expr {
                kind: ExprKind::Uninit,
                loc: ident_loc,
            }
        };

        return Ok(Decl {
            pointer_count,
            ident,
            loc: l_from(ident_loc, expr.loc),
            expr,
        });
    }

    fn parse_inner_struct_decl(&mut self) -> Result<InnerStructDecl, Error> {
        let decl_type = match self.peek()?.kind {
            TokenKind::Struct => {
                let start_loc = self.pop().unwrap().loc;
                let (ident, ident_loc) = self.expect_any_ident()?;

                ASTType {
                    kind: ASTTypeKind::Struct { ident },
                    loc: l_from(start_loc, ident_loc),
                }
            }
            _ => self.parse_simple_type_prefix()?,
        };

        let mut pointer_count: u32 = 0;
        while self.peek()?.kind == TokenKind::Star {
            self.pop().unwrap();
            pointer_count += 1;
        }

        let (ident, ident_loc) = self.expect_ident()?;

        return Ok(InnerStructDecl {
            loc: l_from(decl_type.loc, ident_loc),
            pointer_count,
            decl_type,
            ident,
        });
    }

    fn parse_param_decl(&mut self) -> Result<ParamDecl, Error> {
        let vararg_tok = self.peek()?;
        if vararg_tok.kind == TokenKind::DotDotDot {
            self.pop().unwrap();
            return Ok(ParamDecl {
                kind: ParamKind::Vararg,
                loc: vararg_tok.loc,
            });
        }

        let struct_decl = self.parse_inner_struct_decl()?;
        return Ok(ParamDecl {
            kind: ParamKind::StructLike {
                decl_type: struct_decl.decl_type,
                pointer_count: struct_decl.pointer_count,
                ident: struct_decl.ident,
            },
            loc: struct_decl.loc,
        });
    }

    fn parse_multi_decl(&mut self) -> Result<(Vec<Decl<'b>>, Decl<'b>), Error> {
        let mut decl = self.parse_simple_decl()?;
        let mut tok = self.peek()?;
        let mut decls = Vec::new();

        while tok.kind == TokenKind::Comma {
            self.pop().unwrap();
            decls.push(decl);
            decl = self.parse_simple_decl()?;
            tok = self.peek()?;
        }

        return Ok((decls, decl));
    }

    pub fn parse_global_decl(&mut self) -> Result<GlobalStmt<'b>, Error> {
        let decl_type = match self.peek()?.kind {
            TokenKind::Struct => {
                let start_loc = self.pop().unwrap().loc;
                let (ident, ident_loc) = self.expect_any_ident()?;
                let tok = self.peek()?;

                if tok.kind == TokenKind::LBrace {
                    // parse as type definition
                    self.pop().unwrap();

                    let mut decls = Vec::new();
                    while self.peek()?.kind != TokenKind::RBrace {
                        decls.push(self.parse_inner_struct_decl()?);
                        self.eat_semicolon()?;
                    }

                    let end_loc = self.pop().unwrap().loc;
                    self.eat_semicolon()?;

                    return Ok(GlobalStmt {
                        kind: GlobalStmtKind::StructDecl(StructDecl {
                            ident,
                            ident_loc,
                            loc: l_from(start_loc, end_loc),
                            members: Some(self.buckets.add_array(decls)),
                        }),
                        loc: l_from(start_loc, end_loc),
                    });
                }

                if tok.kind == TokenKind::Semicolon {
                    self.pop().unwrap();

                    return Ok(GlobalStmt {
                        loc: l_from(start_loc, ident_loc),
                        kind: GlobalStmtKind::StructDecl(StructDecl {
                            ident,
                            loc: l_from(start_loc, ident_loc),
                            ident_loc,
                            members: None,
                        }),
                    });
                }

                ASTType {
                    kind: ASTTypeKind::Struct { ident },
                    loc: l_from(start_loc, ident_loc),
                }
            }
            _ => self.parse_simple_type_prefix()?,
        };

        let (mut decls, decl) = self.parse_multi_decl()?;
        let tok = self.pop()?;

        if decls.len() > 0 {
            self.expect_semicolon(&tok)?;
            let end_loc = decl.loc;
            decls.push(decl);
            return Ok(GlobalStmt {
                loc: l_from(decl_type.loc, end_loc),
                kind: GlobalStmtKind::Decl {
                    decl_type,
                    decls: self.buckets.add_array(decls),
                },
            });
        }

        if decl.expr.kind != ExprKind::Uninit {
            self.expect_semicolon(&tok)?;
            let end_loc = decl.loc;
            decls.push(decl);
            return Ok(GlobalStmt {
                loc: l_from(decl_type.loc, end_loc),
                kind: GlobalStmtKind::Decl {
                    decl_type,
                    decls: self.buckets.add_array(decls),
                },
            });
        }

        if tok.kind == TokenKind::Semicolon {
            let end_loc = decl.loc;
            decls.push(decl);
            return Ok(GlobalStmt {
                loc: l_from(decl_type.loc, end_loc),
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
        let rparen_tok = self.peek()?;
        if rparen_tok.kind != TokenKind::RParen {
            params.push(self.parse_param_decl()?);
            let mut comma_tok = self.peek()?;
            while comma_tok.kind == TokenKind::Comma {
                self.pop().unwrap();
                params.push(self.parse_param_decl()?);
                comma_tok = self.peek()?;
            }

            if comma_tok.kind != TokenKind::RParen {
                return Err(self.unexpected_token("end of function declaration", &comma_tok));
            }
        }

        let end_loc = self.pop().unwrap().loc;
        let params = self.buckets.add_array(params);
        let end_decl_tok = self.pop()?;
        if end_decl_tok.kind == TokenKind::Semicolon {
            return Ok(GlobalStmt {
                loc: l_from(decl_type.loc, end_loc),
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
                l_from(decl_type.loc, end_loc),
                "this was parsed as a function declaration".to_string(),
                end_decl_tok.loc,
                "expected a ';' or '{' here".to_string()
            ));
        }

        let mut body = Vec::new();
        while self.peek()?.kind != TokenKind::RBrace {
            body.push(self.parse_stmt()?);
        }
        let tok = self.pop().unwrap();

        let body = self.buckets.add_array(body);
        return Ok(GlobalStmt {
            loc: l_from(decl_type.loc, end_loc),
            kind: GlobalStmtKind::Func {
                return_type: decl_type,
                pointer_count: decl.pointer_count,
                ident: decl.ident,
                params,
                body,
            },
        });
    }

    pub fn parse_block(&mut self) -> Result<(&'b [Stmt<'b>], CodeLoc), Error> {
        match self.peek()?.kind {
            TokenKind::LBrace => {
                let start_loc = self.pop()?.loc;

                let mut stmts = Vec::new();
                while self.peek()?.kind != TokenKind::RBrace {
                    stmts.push(self.parse_stmt()?);
                }
                let end_loc = self.pop()?.loc;

                if stmts.len() == 1 {
                    if let StmtKind::Block(stmts) = stmts[0].kind {
                        return Ok((stmts, l_from(start_loc, end_loc)));
                    } else {
                        return Ok((&*self.buckets.add_array(stmts), l_from(start_loc, end_loc)));
                    }
                } else {
                    return Ok((&*self.buckets.add_array(stmts), l_from(start_loc, end_loc)));
                }
            }
            _ => {
                let stmt = self.parse_stmt()?;
                let loc = stmt.loc;
                return Ok((slice::from_ref(self.buckets.add(stmt)), loc));
            }
        }
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt<'b>, Error> {
        let tok = self.peek()?;
        match &tok.kind {
            TokenKind::For => {
                let start_loc = self.pop().unwrap().loc;

                let lparen_tok = self.expect_lparen()?;

                let is_decl = match &self.peek()?.kind {
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

                self.expect_rparen(lparen_tok.loc)?;

                let (body, body_loc) = self.parse_block()?;

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
                            loc: l_from(start_loc, body_loc),
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
                            loc: l_from(start_loc, body_loc),
                        })
                    }
                }
            }
            TokenKind::Semicolon => {
                return Ok(Stmt {
                    kind: StmtKind::Nop,
                    loc: self.pop().unwrap().loc,
                });
            }
            TokenKind::If => {
                let start_loc = self.pop().unwrap().loc;

                let lparen_tok = self.expect_lparen()?;
                let if_cond = self.parse_expr()?;
                self.expect_rparen(lparen_tok.loc)?;

                let (if_body, if_body_loc) = self.parse_block()?;

                if self.peek()?.kind != TokenKind::Else {
                    return Ok(Stmt {
                        kind: StmtKind::Branch {
                            if_cond,
                            if_body,
                            else_body: None,
                        },
                        loc: l_from(tok.loc, if_body_loc),
                    });
                }

                self.pop().unwrap();
                let (else_body, else_body_loc) = self.parse_block()?;

                return Ok(Stmt {
                    kind: StmtKind::Branch {
                        if_cond,
                        if_body,
                        else_body: Some(else_body),
                    },
                    loc: l_from(tok.loc, else_body_loc),
                });
            }
            TokenKind::LBrace => {
                let (stmts, loc) = self.parse_block()?;
                if stmts.len() == 0 {
                    return Ok(Stmt {
                        kind: StmtKind::Nop,
                        loc,
                    });
                } else {
                    return Ok(Stmt {
                        kind: StmtKind::Block(stmts),
                        loc,
                    });
                }
            }
            TokenKind::Return => {
                self.pop().unwrap();

                if self.peek()?.kind == TokenKind::Semicolon {
                    self.pop().unwrap();
                    return Ok(Stmt {
                        kind: StmtKind::Ret,
                        loc: tok.loc,
                    });
                }

                let expr = self.parse_expr()?;
                self.eat_semicolon()?;

                return Ok(Stmt {
                    loc: l_from(tok.loc, expr.loc),
                    kind: StmtKind::RetVal(expr),
                });
            }
            TokenKind::Int | TokenKind::Char | TokenKind::Void => {
                let decl_type = self.parse_simple_type_prefix()?;
                let start_loc = decl_type.loc;
                let (mut decls, decl) = self.parse_multi_decl()?;
                let end_loc = decl.loc;
                decls.push(decl);
                self.eat_semicolon()?;

                return Ok(Stmt {
                    loc: l_from(start_loc, end_loc),
                    kind: StmtKind::Decl {
                        decl_type,
                        decls: self.buckets.add_array(decls),
                    },
                });
            }
            TokenKind::Struct => {
                let start_loc = self.pop().unwrap().loc;
                let (ident, loc) = self.expect_any_ident()?;

                let decl_type = ASTType {
                    kind: ASTTypeKind::Struct { ident },
                    loc: l_from(start_loc, loc),
                };

                let start_loc = decl_type.loc;
                let (mut decls, decl) = self.parse_multi_decl()?;
                let end_loc = decl.loc;
                decls.push(decl);
                self.eat_semicolon()?;

                return Ok(Stmt {
                    loc: l_from(start_loc, end_loc),
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
                    loc: expr.loc,
                    kind: StmtKind::Expr(expr),
                });
            }
        }
    }

    pub fn unexpected_token(&self, parsing_what: &str, tok: &Token) -> Error {
        return error!(
            &format!("unexpected token while parsing {}", parsing_what),
            tok.loc,
            format!("this was interpreted as {:?}", tok)
        );
    }

    pub fn expect_any_ident(&mut self) -> Result<(u32, CodeLoc), Error> {
        let tok = self.pop()?;
        if let TokenKind::Ident(id) = tok.kind {
            return Ok((id, tok.loc));
        } else if let TokenKind::TypeIdent(id) = tok.kind {
            return Ok((id, tok.loc));
        } else {
            return Err(error!(
                "expected identifier token, got something else instead",
                tok.loc,
                format!(
                    "this was interpreted as {:?} when it should be an identifier",
                    tok
                )
            ));
        }
    }

    pub fn expect_ident(&mut self) -> Result<(u32, CodeLoc), Error> {
        let tok = self.pop()?;
        if let TokenKind::Ident(id) = tok.kind {
            return Ok((id, tok.loc));
        } else {
            return Err(error!(
                "expected identifier token, got something else instead",
                tok.loc,
                format!(
                    "this was interpreted as {:?} when it should be an identifier",
                    tok
                )
            ));
        }
    }

    pub fn expect_rbracket(&mut self) -> Result<(), Error> {
        let tok = self.pop()?;
        if tok.kind != TokenKind::RBracket {
            return Err(error!(
                "expected ']' token, got something else instead",
                tok.loc,
                format!("this was interpreted as {:?} when it should be a ']'", tok)
            ));
        }
        return Ok(());
    }

    pub fn expect_lbrace(&mut self) -> Result<(), Error> {
        let tok = self.pop()?;
        if tok.kind != TokenKind::LBrace {
            return Err(error!(
                "expected '{' token, got something else instead",
                tok.loc,
                format!("this was interpreted as {:?} when it should be a '{{'", tok)
            ));
        }
        return Ok(());
    }

    pub fn expect_rparen(&mut self, matching_tok: CodeLoc) -> Result<CodeLoc, Error> {
        let tok = self.pop()?;
        if tok.kind != TokenKind::RParen {
            return Err(error!(
                "expected ')' token, got something else instead",
                tok.loc,
                format!("this was interpreted as {:?} when it should be a ')'", tok),
                matching_tok,
                "matching left paren here".to_string()
            ));
        }
        return Ok(tok.loc);
    }

    pub fn expect_lparen(&mut self) -> Result<Token<'a>, Error> {
        let tok = self.pop()?;
        if tok.kind != TokenKind::LParen {
            return Err(error!(
                "expected '(' token, got something else instead",
                tok.loc,
                format!("this was interpreted as {:?} when it should be a '('", tok)
            ));
        }
        return Ok(tok);
    }

    pub fn eat_semicolon(&mut self) -> Result<(), Error> {
        let tok = self.pop()?;
        return self.expect_semicolon(&tok);
    }

    pub fn expect_semicolon(&self, tok: &Token) -> Result<(), Error> {
        if tok.kind != TokenKind::Semicolon {
            return Err(error!(
                "expected ';' token, got something else instead",
                tok.loc,
                format!("this was interpreted as {:?} when it should be a ';'", tok)
            ));
        }
        return Ok(());
    }
}
