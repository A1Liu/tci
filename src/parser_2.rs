use crate::ast_2::*;
use crate::buckets::BucketList;
use crate::errors::Error;
use crate::lexer::{Token, TokenKind};
use crate::parser::{ExprParser, Parser, TypeParser};
use crate::type_checker::*;
use core::ops::Range;
use core::slice;
use std::collections::HashMap;

pub struct TypeEnv<'a, 'b> {
    pub _buckets: &'a mut BucketList<'b>,
    pub global_struct_types: HashMap<u32, TCType<'b>>,
    pub global_types: HashMap<u32, TCType<'b>>,
    pub global_symbols: HashMap<u32, TCType<'b>>,
    pub global_func_types: HashMap<u32, TCFunc<'b>>,
}

impl<'a, 'b> TypeEnv<'a, 'b> {
    pub fn new(checker: TypeChecker1<'a, 'b>) -> (HashMap<u32, &'b [Token]>, Self) {
        let env = Self {
            _buckets: checker.parser._buckets,
            global_struct_types: checker.struct_types,
            global_types: checker.types,
            global_symbols: checker.symbols,
            global_func_types: checker.func_types,
        };

        return (checker.functions, env);
    }
}

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

impl<'a, 'b> ExprParser<'b> for Parser2<'a, 'b> {}
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
        let decl = self.parse_simple_decl()?;
        match &decl.decl_type().kind {
            ASTTypeKind::StructDefn { .. } => {
                return Err(Error::new(
                    "unexpected type defintion inside function body",
                    vec![(
                        decl.decl_type().range.clone(),
                        "type definition found here".to_string(),
                    )],
                ));
            }
            _ => {}
        }
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
                    TokenKind::Ident(id) => {
                        if self.env.global_types.contains_key(id) {
                            true
                        } else {
                            false
                        }
                    }
                    TokenKind::Char | TokenKind::Int | TokenKind::Void => true,
                    _ => false,
                };

                let first_part = if is_decl {
                    Ok(self.parse_simple_decl()?)
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
                if self.env.global_types.contains_key(id) {
                    let decl = self.parse_local_decl()?;
                    Error::expect_semicolon(&self.pop())?;

                    return Ok(Stmt {
                        range: decl.range.clone(),
                        kind: StmtKind::Decl(decl),
                    });
                } else {
                    let expr = self.parse_expr()?;
                    Error::expect_semicolon(&self.pop())?;

                    return Ok(Stmt {
                        range: expr.range.clone(),
                        kind: StmtKind::Expr(expr),
                    });
                }
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
