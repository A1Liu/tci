use crate::ast_2::*;
use crate::buckets::BucketList;
use crate::errors::Error;
use crate::lexer::{Token, TokenKind};
use crate::parser::{Parser, TypeParser};
use crate::type_checker::*;
use core::ops::Range;
use core::slice;

pub struct Parser2<'a, 'b> {
    pub _buckets: &'a mut BucketList<'b>,
    pub env: &'a TypeEnv<'a, 'b>,
    pub toks: &'a [Token],
    pub idx: usize,
}

impl<'a, 'b> Parser<'b> for Parser2<'a, 'b> {
    fn peek(&mut self) -> Token {
        if self.idx >= self.toks.len() {
            return Token {
                kind: TokenKind::End,
                range: 0..0,
            };
        }
        return self.toks[self.idx].clone();
    }

    fn pop(&mut self) -> Token {
        let tok = self.peek();
        self.idx += 1;
        return tok;
    }

    fn buckets(&mut self) -> &mut BucketList<'b> {
        return self._buckets;
    }
}

impl<'a, 'b> TypeParser<'b> for Parser2<'a, 'b> {}

impl<'a, 'b> Parser2<'a, 'b> {
    pub fn new(env: &'a TypeEnv<'a, 'b>, toks: &'a [Token]) -> Self {
        Self {
            _buckets: BucketList::new(),
            env,
            toks,
            idx: 0,
        }
    }

    pub fn parse_local_decl(&mut self) -> Result<Decl<'a>, Error> {
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

        let decl = self.parse_simple_decl(decl_type)?;
        return Ok(decl);
    }

    pub fn parse_block(&mut self) -> Result<(&'a [Stmt<'a>], Range<u32>), Error> {
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
                        (&*self.buckets().add_array(stmts), start..end)
                    }
                } else {
                    (&*self.buckets().add_array(stmts), start..end)
                }
            }
            _ => {
                let stmt = self.parse_stmt()?;
                let range = stmt.range.clone();
                (slice::from_ref(self.buckets().add(stmt)), range)
            }
        };

        return Ok((stmts, range));
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt<'a>, Error> {
        let tok = self.peek();
        match &tok.kind {
            TokenKind::For => {
                let start = self.pop().range.start;

                Error::expect_lparen(&self.pop())?;

                let is_decl = match &self.peek().kind {
                    TokenKind::Char | TokenKind::Int | TokenKind::Void => true,
                    _ => false,
                };

                let first_part = if is_decl {
                    Ok(self.parse_local_decl()?)
                } else {
                    Err(self.parse_expr()?)
                };
                Error::expect_semicolon(&self.pop())?;

                let condition = self.parse_expr()?;
                Error::expect_semicolon(&self.pop())?;

                let post_expr = self.parse_expr()?;
                Error::expect_semicolon(&self.pop())?;

                Error::expect_rparen(&self.pop())?;

                let (body, body_range) = self.parse_block()?;

                match first_part {
                    Ok(at_start) => {
                        return Ok(Stmt {
                            kind: StmtKind::ForDecl {
                                at_start,
                                condition,
                                post_expr,
                                body,
                            },
                            range: start..body_range.end,
                        })
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

                Error::expect_lparen(&self.pop())?;
                let if_cond = self.parse_expr()?;
                Error::expect_rparen(&self.pop())?;

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
                let decl = self.parse_local_decl()?;
                Error::expect_semicolon(&self.pop())?;

                return Ok(Stmt {
                    range: decl.range.clone(),
                    kind: StmtKind::Decl(decl),
                });
            }
            TokenKind::Ident(id) => {
                let expr = self.parse_expr()?;
                Error::expect_semicolon(&self.pop())?;

                return Ok(Stmt {
                    range: expr.range.clone(),
                    kind: StmtKind::Expr(expr),
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
