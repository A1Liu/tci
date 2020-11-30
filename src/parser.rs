use crate::ast::*;
use crate::buckets::BucketListRef;
use crate::lexer::*;
use crate::util::*;
use core::slice;
use std::collections::HashMap;

pub type AstDb<'a> = HashMap<u32, &'a [GlobalStmt<'a>]>;

pub struct Parser<'b> {
    pub db: AstDb<'b>,
}

pub fn peek_o<'a>(tokens: &[Token<'a>], current: &usize) -> Option<Token<'a>> {
    return Some(*tokens.get(*current)?);
}

pub fn peek2_o<'a>(tokens: &[Token<'a>], current: &usize) -> Option<Token<'a>> {
    return Some(*tokens.get(*current + 1)?);
}

pub fn peek<'a>(tokens: &[Token<'a>], current: &usize) -> Result<Token<'a>, Error> {
    let map_err = || error!("expected token");
    peek_o(tokens, current).ok_or_else(map_err)
}

pub fn pop<'a>(tokens: &[Token<'a>], current: &mut usize) -> Result<Token<'a>, Error> {
    let tok = peek(tokens, current)?;
    *current += 1;
    Ok(tok)
}

/// True if the parse is about to see a type, false otherwise
pub fn peek_type_or_expr<'a>(tokens: &'a [Token<'a>], current: &usize) -> Result<bool, Error> {
    let tok = peek(tokens, current)?;
    match tok.kind {
        TokenKind::TypeIdent(_) => return Ok(true),
        TokenKind::Int | TokenKind::Long => return Ok(true),
        TokenKind::Unsigned | TokenKind::Signed => return Ok(true),
        TokenKind::Float | TokenKind::Double => return Ok(true),
        TokenKind::Char | TokenKind::Struct | TokenKind::Void => return Ok(true),
        _ => return Ok(false),
    }
}

impl<'b> Parser<'b> {
    pub fn new() -> Self {
        Self { db: HashMap::new() }
    }

    pub fn parse_tokens<'a>(
        &mut self,
        buckets: BucketListRef<'b>,
        token_db: &TokenDb<'a>,
        file: u32,
    ) -> Result<ASTProgram<'b>, Error> {
        if let Some(stmts) = self.db.get(&file) {
            return Ok(ASTProgram { stmts });
        }

        let mut parser = Parser::new();
        let mut parse_result = Vec::new();
        parser.parse_tokens_rec(buckets, token_db, file, &mut parse_result)?;
        let stmts = buckets.add_array(parse_result);
        let prev = self.db.insert(file, stmts);
        debug_assert!(prev.is_none());
        return Ok(ASTProgram { stmts });
    }

    pub fn parse_tokens_rec<'a>(
        &mut self,
        mut buckets: BucketListRef<'b>,
        tdb: &TokenDb<'a>,
        file: u32,
        parse_result: &mut Vec<GlobalStmt<'b>>,
    ) -> Result<(), Error> {
        let tokens = tdb[&file];
        let mut current = 0;

        loop {
            if peek_o(tokens, &mut current).is_none() {
                break;
            }

            self.parse_global_decls(buckets, tdb, tokens, &mut current, parse_result)?;

            while let Some(next) = buckets.next() {
                buckets = next;
            }
        }

        return Ok(());
    }

    #[inline]
    pub fn parse_expr<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        return self.parse_assignment(buckets, tokens, current);
    }

    pub fn parse_assignment<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let left = self.parse_ternary(buckets, tokens, current)?;
        match peek(tokens, current)?.kind {
            TokenKind::Eq => {
                pop(tokens, current).unwrap();
                let right = self.parse_assignment(buckets, tokens, current)?;
                let (right, left) = buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::Assign(left, right),
                });
            }
            TokenKind::PlusEq => {
                pop(tokens, current).unwrap();
                let right = self.parse_assignment(buckets, tokens, current)?;
                let (right, left) = buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::MutAssign {
                        target: left,
                        value: right,
                        op: BinOp::Add,
                    },
                });
            }
            TokenKind::DashEq => {
                pop(tokens, current).unwrap();
                let right = self.parse_assignment(buckets, tokens, current)?;
                let (right, left) = buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    //Not sure if I should create new ExprKind or modify Assign
                    kind: ExprKind::MutAssign {
                        target: left,
                        value: right,
                        op: BinOp::Sub,
                    },
                });
            }
            TokenKind::StarEq => {
                pop(tokens, current).unwrap();
                let right = self.parse_assignment(buckets, tokens, current)?;
                let (right, left) = buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    //Not sure if I should create new ExprKind or modify Assign
                    kind: ExprKind::MutAssign {
                        target: left,
                        value: right,
                        op: BinOp::Mul,
                    },
                });
            }
            TokenKind::SlashEq => {
                pop(tokens, current).unwrap();
                let right = self.parse_assignment(buckets, tokens, current)?;
                let (right, left) = buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    //Not sure if I should create new ExprKind or modify Assign
                    kind: ExprKind::MutAssign {
                        target: left,
                        value: right,
                        op: BinOp::Div,
                    },
                });
            }
            TokenKind::PercentEq => {
                pop(tokens, current).unwrap();
                let right = self.parse_assignment(buckets, tokens, current)?;
                let (right, left) = buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    //Not sure if I should create new ExprKind or modify Assign
                    kind: ExprKind::MutAssign {
                        target: left,
                        value: right,
                        op: BinOp::Mod,
                    },
                });
            }
            TokenKind::LtLtEq => {
                pop(tokens, current).unwrap();
                let right = self.parse_assignment(buckets, tokens, current)?;
                let (right, left) = buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    //Not sure if I should create new ExprKind or modify Assign
                    kind: ExprKind::Assign(left, right),
                });
            }
            TokenKind::GtGtEq => {
                pop(tokens, current).unwrap();
                let right = self.parse_assignment(buckets, tokens, current)?;
                let (right, left) = buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    //Not sure if I should create new ExprKind or modify Assign
                    kind: ExprKind::Assign(left, right),
                });
            }
            TokenKind::AmpEq => {
                pop(tokens, current).unwrap();
                let right = self.parse_assignment(buckets, tokens, current)?;
                let (right, left) = buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    //Not sure if I should create new ExprKind or modify Assign
                    kind: ExprKind::MutAssign {
                        target: left,
                        value: right,
                        op: BinOp::BitAnd,
                    },
                });
            }
            TokenKind::CaretEq => {
                pop(tokens, current).unwrap();
                let right = self.parse_assignment(buckets, tokens, current)?;
                let (right, left) = buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    //Not sure if I should create new ExprKind or modify Assign
                    kind: ExprKind::MutAssign {
                        target: left,
                        value: right,
                        op: BinOp::BitXor,
                    },
                });
            }
            TokenKind::LineEq => {
                pop(tokens, current).unwrap();
                let right = self.parse_assignment(buckets, tokens, current)?;
                let (right, left) = buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    //Not sure if I should create new ExprKind or modify Assign
                    kind: ExprKind::MutAssign {
                        target: left,
                        value: right,
                        op: BinOp::BitOr,
                    },
                });
            }
            _ => {
                return Ok(left);
            }
        }
    }

    pub fn parse_ternary<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let condition = self.parse_bool_or(buckets, tokens, current)?;

        let question_tok = peek(tokens, current)?;
        if question_tok.kind != TokenKind::Question {
            return Ok(condition);
        }

        pop(tokens, current).unwrap();

        let if_true = self.parse_expr(buckets, tokens, current)?;

        let colon_tok = pop(tokens, current)?;
        if colon_tok.kind != TokenKind::Colon {
            return Err(error!(
                "expected ':' token, got something else instead",
                colon_tok.loc,
                format!(
                    "this was interpreted as {:?} when it should be a ':'",
                    colon_tok
                ),
                question_tok.loc,
                "expected ':' because of matching '?' here"
            ));
        }

        let if_false = self.parse_bool_or(buckets, tokens, current)?;

        let condition = buckets.add(condition);
        let if_true = buckets.add(if_true);
        let if_false = buckets.add(if_false);

        return Ok(Expr {
            loc: l_from(condition.loc, if_false.loc),
            kind: ExprKind::Ternary {
                condition,
                if_true,
                if_false,
            },
        });
    }

    pub fn parse_bool_or<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_bool_and(buckets, tokens, current)?;
        loop {
            let start_loc = expr.loc;
            match peek(tokens, current)?.kind {
                TokenKind::LineLine => {
                    pop(tokens, current).unwrap();

                    let right = self.parse_bool_and(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::BoolOr, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_bool_and<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_bit_or(buckets, tokens, current)?;
        loop {
            let start_loc = expr.loc;
            match peek(tokens, current)?.kind {
                TokenKind::AmpAmp => {
                    pop(tokens, current).unwrap();

                    let right = self.parse_bit_or(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::BoolAnd, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_bit_or<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_bit_xor(buckets, tokens, current)?;
        loop {
            let start_loc = expr.loc;
            match peek(tokens, current)?.kind {
                TokenKind::Line => {
                    pop(tokens, current).unwrap();

                    let right = self.parse_bit_xor(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::BitOr, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_bit_xor<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_bit_and(buckets, tokens, current)?;
        loop {
            let start_loc = expr.loc;
            match peek(tokens, current)?.kind {
                TokenKind::Caret => {
                    pop(tokens, current).unwrap();

                    let right = self.parse_bit_and(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::BitXor, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_bit_and<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_equality(buckets, tokens, current)?;
        loop {
            let start_loc = expr.loc;
            match peek(tokens, current)?.kind {
                TokenKind::Amp => {
                    pop(tokens, current).unwrap();

                    let right = self.parse_equality(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::BitAnd, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_equality<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_comparison(buckets, tokens, current)?;
        loop {
            let start_loc = expr.loc;
            match peek(tokens, current)?.kind {
                TokenKind::EqEq => {
                    pop(tokens, current).unwrap();

                    let right = self.parse_comparison(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Eq, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Neq => {
                    pop(tokens, current).unwrap();

                    let right = self.parse_comparison(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Neq, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_comparison<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_shift(buckets, tokens, current)?;
        loop {
            let start_loc = expr.loc;
            match peek(tokens, current)?.kind {
                TokenKind::Lt => {
                    pop(tokens, current).unwrap();

                    let right = self.parse_shift(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Lt, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Leq => {
                    pop(tokens, current).unwrap();

                    let right = self.parse_shift(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Leq, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Gt => {
                    pop(tokens, current).unwrap();

                    let right = self.parse_shift(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Gt, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Geq => {
                    pop(tokens, current).unwrap();

                    let right = self.parse_shift(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Geq, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_shift<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_add(buckets, tokens, current)?;
        loop {
            let start_loc = expr.loc;
            match peek(tokens, current)?.kind {
                TokenKind::GtGt => {
                    pop(tokens, current).unwrap();
                    let right = self.parse_add(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::RShift, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::LtLt => {
                    pop(tokens, current).unwrap();
                    let right = self.parse_add(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::LShift, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_add<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_multiply(buckets, tokens, current)?;
        loop {
            let start_loc = expr.loc;
            match peek(tokens, current)?.kind {
                TokenKind::Plus => {
                    pop(tokens, current).unwrap();
                    let right = self.parse_multiply(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Add, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Dash => {
                    pop(tokens, current).unwrap();
                    let right = self.parse_multiply(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Sub, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_multiply<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_prefix(buckets, tokens, current)?;
        loop {
            let start_loc = expr.loc;
            match peek(tokens, current)?.kind {
                TokenKind::Slash => {
                    pop(tokens, current).unwrap();
                    let right = self.parse_prefix(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Div, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Star => {
                    pop(tokens, current).unwrap();
                    let right = self.parse_prefix(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Mul, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Percent => {
                    pop(tokens, current).unwrap();
                    let right = self.parse_prefix(buckets, tokens, current)?;
                    let end_loc = right.loc;
                    let left = buckets.add(expr);
                    let right = buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Mod, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_prefix<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let tok = peek(tokens, current)?;
        match tok.kind {
            TokenKind::Sizeof => {
                pop(tokens, current).unwrap();

                let lparen_tok = peek(tokens, current)?;
                if lparen_tok.kind == TokenKind::LParen {
                    pop(tokens, current).unwrap();

                    if peek_type_or_expr(tokens, current)? {
                        let sizeof_type = self.parse_type_prefix(buckets, tokens, current)?;
                        let mut pointer_count = 0;
                        while peek(tokens, current)?.kind == TokenKind::Star {
                            pointer_count += 1;
                            pop(tokens, current).unwrap();
                        }

                        let rparen_loc = expect_rparen(tokens, current, lparen_tok.loc)?;
                        return Ok(Expr {
                            kind: ExprKind::SizeofType {
                                sizeof_type,
                                pointer_count,
                            },
                            loc: l_from(tok.loc, rparen_loc),
                        });
                    }

                    let target = self.parse_expr(buckets, tokens, current)?;
                    let rparen_loc = expect_rparen(tokens, current, lparen_tok.loc)?;
                    let target = buckets.add(target);
                    return Ok(Expr {
                        loc: l_from(tok.loc, rparen_loc),
                        kind: ExprKind::SizeofExpr(target),
                    });
                }

                let target = self.parse_prefix(buckets, tokens, current)?;
                let target = buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::SizeofExpr(target),
                });
            }

            TokenKind::Amp => {
                pop(tokens, current).unwrap();
                let target = self.parse_prefix(buckets, tokens, current)?;
                let target = buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::Ref(target),
                });
            }
            TokenKind::Star => {
                pop(tokens, current).unwrap();
                let target = self.parse_prefix(buckets, tokens, current)?;
                let target = buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::Deref(target),
                });
            }

            TokenKind::Bang => {
                pop(tokens, current).unwrap();
                let target = self.parse_prefix(buckets, tokens, current)?;
                let target = buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::UnaryOp(UnaryOp::BoolNot, target),
                });
            }
            TokenKind::Tilde => {
                pop(tokens, current).unwrap();
                let target = self.parse_prefix(buckets, tokens, current)?;
                let target = buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::UnaryOp(UnaryOp::BitNot, target),
                });
            }
            TokenKind::Dash => {
                pop(tokens, current).unwrap();
                let target = self.parse_prefix(buckets, tokens, current)?;
                let target = buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::UnaryOp(UnaryOp::Neg, target),
                });
            }

            TokenKind::LParen => {
                let type_tok = if let Some(tok) = peek2_o(tokens, current) {
                    tok
                } else {
                    return self.parse_postfix(buckets, tokens, current);
                };

                let (lparen, cast_to) = match type_tok.kind {
                    TokenKind::TypeIdent(_)
                    | TokenKind::Struct
                    | TokenKind::Char
                    | TokenKind::Void
                    | TokenKind::Int => {
                        let lparen = pop(tokens, current).unwrap();
                        let ast_type = self.parse_type_prefix(buckets, tokens, current)?;
                        (lparen, ast_type)
                    }
                    _ => return self.parse_postfix(buckets, tokens, current),
                };

                let mut pointer_count: u32 = 0;
                while peek(tokens, current)?.kind == TokenKind::Star {
                    pop(tokens, current).unwrap();
                    pointer_count += 1;
                }

                let end_loc = expect_rparen(tokens, current, lparen.loc)?;

                let target = self.parse_prefix(buckets, tokens, current)?;
                let target = buckets.add(target);

                return Ok(Expr {
                    loc: l_from(lparen.loc, target.loc),
                    kind: ExprKind::Cast {
                        cast_to,
                        cast_to_loc: l_from(lparen.loc, end_loc),
                        pointer_count,
                        expr: target,
                    },
                });
            }
            _ => return self.parse_postfix(buckets, tokens, current),
        }
    }

    pub fn parse_postfix<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let mut operand = self.parse_atom(buckets, tokens, current)?;
        let start_loc = operand.loc;

        loop {
            match peek(tokens, current)?.kind {
                TokenKind::LParen => {
                    pop(tokens, current).unwrap();
                    let mut params = Vec::new();
                    let rparen_tok = peek(tokens, current)?;

                    if rparen_tok.kind != TokenKind::RParen {
                        let param = self.parse_expr(buckets, tokens, current)?;
                        params.push(param);
                        let mut comma_tok = peek(tokens, current)?;

                        while comma_tok.kind == TokenKind::Comma {
                            pop(tokens, current).unwrap();
                            params.push(self.parse_expr(buckets, tokens, current)?);
                            comma_tok = peek(tokens, current)?;
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

                    let end_loc = pop(tokens, current).unwrap().loc;
                    let params = buckets.add_array(params);
                    operand = Expr {
                        loc: l_from(start_loc, end_loc),
                        kind: ExprKind::Call {
                            function: buckets.add(operand),
                            params,
                        },
                    };
                }
                TokenKind::PlusPlus => {
                    operand = Expr {
                        kind: ExprKind::PostIncr(buckets.add(operand)),
                        loc: l_from(start_loc, pop(tokens, current).unwrap().loc),
                    };
                }
                TokenKind::DashDash => {
                    operand = Expr {
                        kind: ExprKind::PostDecr(buckets.add(operand)),
                        loc: l_from(start_loc, pop(tokens, current)?.loc),
                    };
                }
                TokenKind::LBracket => {
                    let lbracket = pop(tokens, current).unwrap();
                    let index = self.parse_expr(buckets, tokens, current)?;
                    let rbracket_tok = expect_rbracket(tokens, current, lbracket.loc)?;

                    let loc = l_from(start_loc, rbracket_tok.loc);

                    operand = Expr {
                        kind: ExprKind::BinOp(
                            BinOp::Index,
                            buckets.add(operand),
                            buckets.add(index),
                        ),
                        loc,
                    };
                }
                TokenKind::Arrow => {
                    pop(tokens, current).unwrap();

                    let (member, loc) = expect_any_ident(tokens, current)?;

                    operand = Expr {
                        kind: ExprKind::PtrMember {
                            base: buckets.add(operand),
                            member,
                        },
                        loc: l_from(start_loc, loc),
                    };
                }
                TokenKind::Dot => {
                    pop(tokens, current).unwrap();

                    let (member, loc) = expect_any_ident(tokens, current)?;

                    operand = Expr {
                        kind: ExprKind::Member {
                            base: buckets.add(operand),
                            member,
                        },
                        loc: l_from(start_loc, loc),
                    };
                }
                _ => return Ok(operand),
            }
        }
    }

    pub fn parse_atom<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let tok = pop(tokens, current)?;
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
            TokenKind::CharLiteral(c) => {
                return Ok(Expr {
                    kind: ExprKind::CharLiteral(c),
                    loc: tok.loc,
                })
            }
            TokenKind::StringLiteral(string) => {
                let mut string = string.to_string();
                let mut end_loc = tok.loc;
                while let TokenKind::StringLiteral(tstr) = peek(tokens, current)?.kind {
                    string.push_str(tstr);
                    end_loc = l_from(end_loc, pop(tokens, current).unwrap().loc);
                }

                return Ok(Expr {
                    kind: ExprKind::StringLiteral(buckets.add_str(&string)),
                    loc: l_from(tok.loc, end_loc),
                });
            }
            TokenKind::LBrace => {
                let start_loc = tok.loc;
                let mut expr = self.parse_expr(buckets, tokens, current)?;
                let mut expr_list = Vec::new();
                while peek(tokens, current)?.kind == TokenKind::Comma {
                    expr_list.push(expr);
                    pop(tokens, current).unwrap();
                    expr = self.parse_expr(buckets, tokens, current)?;
                }

                let end_loc = expect_rbrace(tokens, current, tok.loc)?;

                if expr_list.len() == 0 {
                    return Ok(expr);
                } else {
                    expr_list.push(expr);
                    return Ok(Expr {
                        kind: ExprKind::BraceList(buckets.add_array(expr_list)),
                        loc: l_from(start_loc, end_loc),
                    });
                }
            }
            TokenKind::LParen => {
                let start_loc = tok.loc;
                let mut expr = self.parse_expr(buckets, tokens, current)?;
                let mut expr_list = Vec::new();
                while peek(tokens, current)?.kind == TokenKind::Comma {
                    expr_list.push(expr);
                    pop(tokens, current).unwrap();
                    expr = self.parse_expr(buckets, tokens, current)?;
                }

                let end_loc = expect_rparen(tokens, current, tok.loc)?;

                if expr_list.len() == 0 {
                    return Ok(expr);
                } else {
                    expr_list.push(expr);
                    return Ok(Expr {
                        kind: ExprKind::ParenList(buckets.add_array(expr_list)),
                        loc: l_from(start_loc, end_loc),
                    });
                }
            }
            _ => return Err(unexpected_token("expression", &tok)),
        }
    }

    fn parse_brackets<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Option<(&'b [u32], CodeLoc)>, Error> {
        let start_loc = peek(tokens, current)?.loc;
        let mut end_loc = start_loc;
        let mut array_dims = Vec::new();
        while peek(tokens, current)?.kind == TokenKind::LBracket {
            let lbracket_tok = pop(tokens, current).unwrap();
            let rbracket_tok = peek(tokens, current)?;

            if rbracket_tok.kind == TokenKind::RBracket {
                pop(tokens, current).unwrap();
                array_dims.push(0);
                end_loc = rbracket_tok.loc;
                continue;
            }

            let expr = self.parse_expr(buckets, tokens, current)?;
            match   expr.kind{
                ExprKind::IntLiteral(value) => {
                    if value <= 0 {
                        return Err(error!(
                            "array dimension value must be at least 1",
                            expr.loc, "invalid array dimension found here"
                        ));
                    }

                    array_dims.push(value as u32);
                }
                _ => {
                    return Err(error!(
                        "TCI currently doesn't accept anything but integer literals as array dimensions",
                        expr.loc, "non-conforming expression found here"
                    ))
                }
            }

            let rbracket_tok = expect_rbracket(tokens, current, lbracket_tok.loc)?;
            end_loc = rbracket_tok.loc;
        }
        if array_dims.len() == 0 {
            return Ok(None);
        }

        let array_dims = buckets.add_array(array_dims);
        return Ok(Some((array_dims, l_from(start_loc, end_loc))));
    }

    fn parse_decl_receiver<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<DeclReceiver<'b>, Error> {
        let mut pointer_count = 0;
        let loc = peek(tokens, current)?.loc;
        while peek(tokens, current)?.kind == TokenKind::Star {
            pointer_count += 1;
            pop(tokens, current).unwrap();
        }

        let (ident, end_loc) = expect_any_ident(tokens, current)?;

        if let Some((array_dims, end_loc)) = self.parse_brackets(buckets, tokens, current)? {
            return Ok(DeclReceiver {
                loc: l_from(loc, end_loc),
                ident,
                pointer_count,
                array_dims,
            });
        }

        return Ok(DeclReceiver {
            loc: l_from(loc, end_loc),
            ident,
            pointer_count,
            array_dims: &[],
        });
    }

    fn parse_simple_decl<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Decl<'b>, Error> {
        let recv = self.parse_decl_receiver(buckets, tokens, current)?;

        let tok = peek(tokens, current)?;
        let expr = if tok.kind == TokenKind::Eq {
            pop(tokens, current).unwrap();
            self.parse_expr(buckets, tokens, current)?
        } else {
            Expr {
                kind: ExprKind::Uninit,
                loc: recv.loc,
            }
        };

        return Ok(Decl {
            recv,
            loc: l_from(recv.loc, expr.loc),
            expr,
        });
    }

    fn parse_inner_struct_decl(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &[Token],
        current: &mut usize,
    ) -> Result<InnerStructDecl<'b>, Error> {
        let decl_type = self.parse_type_prefix(buckets, tokens, current)?;

        let recv = self.parse_decl_receiver(buckets, tokens, current)?;

        return Ok(InnerStructDecl {
            loc: l_from(decl_type.loc, recv.loc),
            decl_type,
            recv,
        });
    }

    fn parse_param_decl<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<ParamDecl<'b>, Error> {
        let vararg_tok = peek(tokens, current)?;
        if vararg_tok.kind == TokenKind::DotDotDot {
            pop(tokens, current).unwrap();
            return Ok(ParamDecl {
                kind: ParamKind::Vararg,
                loc: vararg_tok.loc,
            });
        }

        let decl_type = self.parse_type_prefix(buckets, tokens, current)?;
        let decl_recv_start_loc = peek(tokens, current)?.loc;
        let mut end_loc = decl_type.loc;

        let mut pointer_count = 0;
        while peek(tokens, current)?.kind == TokenKind::Star {
            pointer_count += 1;
            end_loc = pop(tokens, current).unwrap().loc;
        }

        let ident = if let TokenKind::Ident(ident) = peek(tokens, current)?.kind {
            end_loc = pop(tokens, current).unwrap().loc;
            Some(ident)
        } else {
            None
        };

        let array_dims =
            if let Some((ad, ad_loc)) = self.parse_brackets(buckets, tokens, current)? {
                end_loc = ad_loc;
                ad
            } else {
                &[]
            };

        match ident {
            None => {
                return Ok(ParamDecl {
                    kind: ParamKind::TypeOnly {
                        decl_type: decl_type,
                        pointer_count,
                        array_dims,
                    },
                    loc: l_from(decl_type.loc, end_loc),
                })
            }
            Some(ident) => {
                return Ok(ParamDecl {
                    kind: ParamKind::StructLike {
                        decl_type: decl_type,
                        recv: DeclReceiver {
                            ident,
                            pointer_count,
                            loc: l_from(decl_recv_start_loc, end_loc),
                            array_dims,
                        },
                    },
                    loc: l_from(decl_type.loc, end_loc),
                });
            }
        }
    }

    fn parse_multi_decl<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<(Vec<Decl<'b>>, Decl<'b>), Error> {
        let mut decl = self.parse_simple_decl(buckets, tokens, current)?;
        let mut tok = peek(tokens, current)?;
        let mut decls = Vec::new();

        while tok.kind == TokenKind::Comma {
            pop(tokens, current).unwrap();
            decls.push(decl);
            decl = self.parse_simple_decl(buckets, tokens, current)?;
            tok = peek(tokens, current)?;
        }

        return Ok((decls, decl));
    }

    pub fn parse_global_decls<'a>(
        &mut self,
        buckets: BucketListRef<'b>,
        token_db: &TokenDb<'a>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
        decls: &mut Vec<GlobalStmt<'b>>,
    ) -> Result<(), Error> {
        macro_rules! ret_stmt {
            ($stmt:expr) => {
                decls.push($stmt);
                return Ok(());
            };
        }

        let decl_type = match peek(tokens, current)?.kind {
            TokenKind::Typedef => {
                let typedef_tok = pop(tokens, current).unwrap();
                let ast_type = self.parse_type_prefix(buckets, tokens, current)?;
                let recv = self.parse_decl_receiver(buckets, tokens, current)?;
                eat_semicolon(tokens, current)?;
                ret_stmt!(GlobalStmt {
                    kind: GlobalStmtKind::Typedef { ast_type, recv },
                    loc: l_from(typedef_tok.loc, recv.loc)
                });
            }
            _ => self.parse_type_prefix(buckets, tokens, current)?,
        };

        let semicolon_tok = peek(tokens, current)?;
        if semicolon_tok.kind == TokenKind::Semicolon {
            if let ASTTypeKind::Struct(decl) = decl_type.kind {
                pop(tokens, current).unwrap();
                ret_stmt!(GlobalStmt {
                    kind: GlobalStmtKind::StructDecl(decl),
                    loc: decl_type.loc,
                });
            }

            return Err(error!(
                "declared a primitive data type",
                decl_type.loc, "declared a primitive datatype here"
            ));
        }

        let (mut decls, decl) = self.parse_multi_decl(buckets, tokens, current)?;
        let tok = pop(tokens, current)?;

        if decls.len() > 0 {
            expect_semicolon(&tok)?;
            let end_loc = decl.loc;
            decls.push(decl);
            ret_stmt!(GlobalStmt {
                loc: l_from(decl_type.loc, end_loc),
                kind: GlobalStmtKind::Decl {
                    decl_type,
                    decls: buckets.add_array(decls),
                },
            });
        }

        if let ExprKind::Uninit = decl.expr.kind {
        } else {
            expect_semicolon(&tok)?;
            let end_loc = decl.loc;
            decls.push(decl);
            ret_stmt!(GlobalStmt {
                loc: l_from(decl_type.loc, end_loc),
                kind: GlobalStmtKind::Decl {
                    decl_type,
                    decls: buckets.add_array(decls),
                },
            });
        }

        if tok.kind == TokenKind::Semicolon {
            let end_loc = decl.loc;
            decls.push(decl);
            ret_stmt!(GlobalStmt {
                loc: l_from(decl_type.loc, end_loc),
                kind: GlobalStmtKind::Decl {
                    decl_type,
                    decls: buckets.add_array(decls),
                },
            });
        }

        if tok.kind != TokenKind::LParen {
            return Err(unexpected_token("function declaration", &tok));
        }

        let mut params = Vec::new();
        let rparen_tok = peek(tokens, current)?;
        if rparen_tok.kind != TokenKind::RParen {
            params.push(self.parse_param_decl(buckets, tokens, current)?);
            let mut comma_tok = peek(tokens, current)?;
            while comma_tok.kind == TokenKind::Comma {
                pop(tokens, current).unwrap();
                params.push(self.parse_param_decl(buckets, tokens, current)?);
                comma_tok = peek(tokens, current)?;
            }

            if comma_tok.kind != TokenKind::RParen {
                return Err(unexpected_token("end of function declaration", &comma_tok));
            }
        }

        let end_loc = pop(tokens, current).unwrap().loc;
        let params = buckets.add_array(params);
        let end_decl_tok = pop(tokens, current)?;
        if end_decl_tok.kind == TokenKind::Semicolon {
            ret_stmt!(GlobalStmt {
                loc: l_from(decl_type.loc, end_loc),
                kind: GlobalStmtKind::FuncDecl {
                    pointer_count: decl.recv.pointer_count,
                    return_type: decl_type,
                    ident: decl.recv.ident,
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
        while peek(tokens, current)?.kind != TokenKind::RBrace {
            body.push(self.parse_stmt(buckets, tokens, current)?);
        }
        let _tok = pop(tokens, current).unwrap();

        let body = buckets.add_array(body);
        ret_stmt!(GlobalStmt {
            // TODO change end_loc to tok.loc and add header_loc field to func
            loc: l_from(decl_type.loc, end_loc),
            kind: GlobalStmtKind::Func {
                return_type: decl_type,
                pointer_count: decl.recv.pointer_count,
                ident: decl.recv.ident,
                params,
                body,
            },
        });
    }

    pub fn parse_block<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Block<'b>, Error> {
        match peek(tokens, current)?.kind {
            TokenKind::LBrace => {
                let start_loc = pop(tokens, current)?.loc;

                let mut stmts = Vec::new();
                while peek(tokens, current)?.kind != TokenKind::RBrace {
                    stmts.push(self.parse_stmt(buckets, tokens, current)?);
                }
                let end_loc = pop(tokens, current)?.loc;

                if stmts.len() == 1 {
                    if let StmtKind::Block(block) = stmts[0].kind {
                        return Ok(block);
                    } else {
                        return Ok(Block {
                            stmts: buckets.add_array(stmts),
                            loc: l_from(start_loc, end_loc),
                        });
                    }
                } else {
                    return Ok(Block {
                        stmts: buckets.add_array(stmts),
                        loc: l_from(start_loc, end_loc),
                    });
                }
            }
            _ => {
                let stmt = self.parse_stmt(buckets, tokens, current)?;
                return Ok(Block {
                    loc: stmt.loc,
                    stmts: slice::from_ref(buckets.add(stmt)),
                });
            }
        }
    }

    pub fn parse_stmt<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Stmt<'b>, Error> {
        if peek_type_or_expr(tokens, current)? {
            let decl_type = self.parse_type_prefix(buckets, tokens, current)?;
            let start_loc = decl_type.loc;
            let (mut decls, decl) = self.parse_multi_decl(buckets, tokens, current)?;
            let end_loc = decl.loc;
            decls.push(decl);
            eat_semicolon(tokens, current)?;

            return Ok(Stmt {
                loc: l_from(start_loc, end_loc),
                kind: StmtKind::Decl {
                    decl_type,
                    decls: buckets.add_array(decls),
                },
            });
        }

        let tok = peek(tokens, current)?;
        match &tok.kind {
            TokenKind::For => {
                let start_loc = pop(tokens, current).unwrap().loc;

                let lparen_tok = expect_lparen(tokens, current)?;

                let (first_part, semi_tok) = if peek_type_or_expr(tokens, current)? {
                    let decl_type = self.parse_type_prefix(buckets, tokens, current)?;
                    let (mut decls, decl) = self.parse_multi_decl(buckets, tokens, current)?;
                    decls.push(decl);
                    let semi = eat_semicolon(tokens, current)?;
                    (Ok((decl_type, decls)), semi)
                } else if peek(tokens, current)?.kind == TokenKind::Semicolon {
                    let semi = eat_semicolon(tokens, current).unwrap();
                    let expr = Expr {
                        kind: ExprKind::Uninit,
                        loc: l(lparen_tok.loc.end, semi.loc.start, semi.loc.file),
                    };
                    (Err(expr), semi)
                } else {
                    let expr = self.parse_expr(buckets, tokens, current)?;
                    let semi = eat_semicolon(tokens, current)?;
                    (Err(expr), semi)
                };

                let (condition, semi2) = match peek(tokens, current)?.kind {
                    TokenKind::Semicolon => {
                        let semi2 = eat_semicolon(tokens, current).unwrap();
                        let expr = Expr {
                            kind: ExprKind::IntLiteral(1),
                            loc: l(semi_tok.loc.end, semi2.loc.start, semi2.loc.file),
                        };
                        (expr, semi2)
                    }
                    _ => {
                        let expr = self.parse_expr(buckets, tokens, current)?;
                        let semi2 = eat_semicolon(tokens, current)?;
                        (expr, semi2)
                    }
                };

                let mut post_exprs = Vec::new();
                if TokenKind::RParen != peek(tokens, current)?.kind {
                    post_exprs.push(self.parse_expr(buckets, tokens, current)?);
                    while TokenKind::RParen != peek(tokens, current)?.kind {
                        expect_comma(tokens, current)?;
                        post_exprs.push(self.parse_expr(buckets, tokens, current)?);
                    }
                }

                let rparen_loc = expect_rparen(tokens, current, lparen_tok.loc).unwrap();

                let body = self.parse_block(buckets, tokens, current)?;

                let post_exprs = buckets.add_array(post_exprs);
                let post_expr = Expr {
                    kind: ExprKind::ParenList(post_exprs),
                    loc: l(semi2.loc.end, rparen_loc.start, rparen_loc.file),
                };

                match first_part {
                    Ok((decl_type, decls)) => {
                        return Ok(Stmt {
                            loc: l_from(start_loc, body.loc),
                            kind: StmtKind::ForDecl {
                                at_start_decl_type: decl_type,
                                at_start: buckets.add_array(decls),
                                condition,
                                post_expr,
                                body,
                            },
                        });
                    }
                    Err(at_start) => {
                        return Ok(Stmt {
                            loc: l_from(start_loc, body.loc),
                            kind: StmtKind::For {
                                at_start,
                                condition,
                                post_expr,
                                body,
                            },
                        })
                    }
                }
            }

            TokenKind::Semicolon => {
                return Ok(Stmt {
                    kind: StmtKind::Nop,
                    loc: pop(tokens, current).unwrap().loc,
                });
            }

            TokenKind::If => {
                let start_loc = pop(tokens, current).unwrap().loc;

                let lparen_tok = expect_lparen(tokens, current)?;
                let if_cond = self.parse_expr(buckets, tokens, current)?;
                expect_rparen(tokens, current, lparen_tok.loc)?;

                let if_body = self.parse_block(buckets, tokens, current)?;

                if peek(tokens, current)?.kind != TokenKind::Else {
                    return Ok(Stmt {
                        loc: l_from(start_loc, if_body.loc),
                        kind: StmtKind::Branch {
                            else_body: Block {
                                stmts: buckets.add_slice(&[]),
                                loc: l(if_body.loc.end, if_body.loc.end, if_body.loc.file),
                            },
                            if_cond,
                            if_body,
                        },
                    });
                }

                pop(tokens, current).unwrap();
                let else_body = self.parse_block(buckets, tokens, current)?;

                return Ok(Stmt {
                    loc: l_from(tok.loc, else_body.loc),
                    kind: StmtKind::Branch {
                        if_cond,
                        if_body,
                        else_body,
                    },
                });
            }

            TokenKind::LBrace => {
                let block = self.parse_block(buckets, tokens, current)?;
                if block.stmts.len() == 0 {
                    return Ok(Stmt {
                        kind: StmtKind::Nop,
                        loc: block.loc,
                    });
                } else {
                    return Ok(Stmt {
                        loc: block.loc,
                        kind: StmtKind::Block(block),
                    });
                }
            }

            TokenKind::Return => {
                pop(tokens, current).unwrap();

                if peek(tokens, current)?.kind == TokenKind::Semicolon {
                    pop(tokens, current).unwrap();
                    return Ok(Stmt {
                        kind: StmtKind::Ret,
                        loc: tok.loc,
                    });
                }

                let expr = self.parse_expr(buckets, tokens, current)?;
                eat_semicolon(tokens, current)?;

                return Ok(Stmt {
                    loc: l_from(tok.loc, expr.loc),
                    kind: StmtKind::RetVal(expr),
                });
            }

            TokenKind::Break => {
                pop(tokens, current).unwrap();
                eat_semicolon(tokens, current)?;

                return Ok(Stmt {
                    kind: StmtKind::Break,
                    loc: tok.loc,
                });
            }
            TokenKind::Continue => {
                pop(tokens, current).unwrap();
                eat_semicolon(tokens, current)?;

                return Ok(Stmt {
                    kind: StmtKind::Continue,
                    loc: tok.loc,
                });
            }

            _ => {
                let expr = self.parse_expr(buckets, tokens, current)?;
                eat_semicolon(tokens, current)?;

                return Ok(Stmt {
                    loc: expr.loc,
                    kind: StmtKind::Expr(expr),
                });
            }
        }
    }

    fn parse_type_prefix(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &[Token],
        current: &mut usize,
    ) -> Result<ASTType<'b>, Error> {
        let mut kind;

        let mut tok = peek(tokens, current)?;
        match tok.kind {
            TokenKind::Int => kind = ASTTypeKind::Int,
            TokenKind::Void => kind = ASTTypeKind::Void,
            TokenKind::Char => kind = ASTTypeKind::Char,
            TokenKind::Long => kind = ASTTypeKind::Long,
            TokenKind::Unsigned => kind = ASTTypeKind::Unsigned,
            TokenKind::TypeIdent(ident) => {
                pop(tokens, current).unwrap();

                return Ok(ASTType {
                    kind: ASTTypeKind::Ident(ident),
                    loc: tok.loc,
                });
            }
            TokenKind::Struct => {
                pop(tokens, current).unwrap();

                let start_loc = tok.loc;
                if let Some((ident, ident_loc)) = any_ident_o(tokens, current) {
                    let end_loc = ident_loc;

                    if peek(tokens, current)?.kind == TokenKind::LBrace {
                        pop(tokens, current).unwrap();

                        let mut decls = Vec::new();
                        while peek(tokens, current)?.kind != TokenKind::RBrace {
                            decls.push(self.parse_inner_struct_decl(buckets, tokens, current)?);
                            eat_semicolon(tokens, current)?;
                        }
                        // parse as type definition
                        let end_loc = pop(tokens, current).unwrap().loc;
                        let members = &*buckets.add_array(decls);

                        return Ok(ASTType {
                            kind: ASTTypeKind::Struct(StructDecl::NamedDef { ident, members }),
                            loc: l_from(start_loc, end_loc),
                        });
                    }

                    return Ok(ASTType {
                        kind: ASTTypeKind::Struct(StructDecl::Named(ident)),
                        loc: l_from(start_loc, end_loc),
                    });
                } else {
                    expect_lbrace(tokens, current)?;
                    let mut decls = Vec::new();
                    while peek(tokens, current)?.kind != TokenKind::RBrace {
                        decls.push(self.parse_inner_struct_decl(buckets, tokens, current)?);
                        eat_semicolon(tokens, current)?;
                    }

                    let end_loc = pop(tokens, current).unwrap().loc;
                    let members = &*buckets.add_array(decls);

                    return Ok(ASTType {
                        kind: ASTTypeKind::Struct(StructDecl::Unnamed(members)),
                        loc: l_from(start_loc, end_loc),
                    });
                }
            }
            _ => return Err(unexpected_token("type", &tok)),
        }

        let start_loc = pop(tokens, current).unwrap().loc;
        let mut end_loc = start_loc;

        macro_rules! reject {
            ( $( $ident:ident),* ) => {{
                $(
                    if tok.kind == TokenKind::$ident {
                        return Err(error!(
                            "token not allowed in this context",
                            tok.loc, "illegal token found here"
                        ));
                    }
                )*
            }};
        }

        // TODO how do you do this cleaner?
        tok = peek(tokens, current)?;
        loop {
            use ASTTypeKind as ATK;

            match kind {
                ATK::Int => {
                    reject!(Char, Void, Int);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedInt,
                        TokenKind::Long => kind = ATK::LongInt,
                        _ => break,
                    }
                }
                ATK::Long => {
                    reject!(Char, Void);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedLong,
                        TokenKind::Long => kind = ATK::LongLong,
                        TokenKind::Int => kind = ATK::LongInt,
                        _ => break,
                    }
                }
                ATK::Char => {
                    reject!(Long, Char, Void, Int);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedChar,
                        _ => break,
                    }
                }
                ATK::Unsigned => {
                    reject!(Void);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::Unsigned,
                        TokenKind::Char => kind = ATK::UnsignedChar,
                        TokenKind::Int => kind = ATK::UnsignedInt,
                        TokenKind::Long => kind = ATK::UnsignedLong,
                        _ => break,
                    }
                }
                ATK::Void => {
                    reject!(Char, Void, Int, Long, Unsigned);
                    break;
                }

                ATK::LongInt => {
                    reject!(Char, Void, Int);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedLongInt,
                        TokenKind::Long => kind = ATK::LongLongInt,
                        _ => break,
                    }
                }
                ATK::LongLongInt => {
                    reject!(Char, Void, Int, Long);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedLongLongInt,
                        _ => break,
                    }
                }
                ATK::LongLong => {
                    reject!(Char, Void, Long);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedLongLongInt,
                        TokenKind::Int => kind = ATK::LongLongInt,
                        _ => break,
                    }
                }

                ATK::UnsignedInt => {
                    reject!(Char, Void, Int);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedInt,
                        TokenKind::Long => kind = ATK::UnsignedLongInt,
                        _ => break,
                    }
                }
                ATK::UnsignedLong => {
                    reject!(Char, Void);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedLong,
                        TokenKind::Long => kind = ATK::UnsignedLongLong,
                        TokenKind::Int => kind = ATK::UnsignedLongInt,
                        _ => break,
                    }
                }
                ATK::UnsignedLongInt => {
                    reject!(Char, Void, Int);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedLongInt,
                        TokenKind::Long => kind = ATK::UnsignedLongLongInt,
                        _ => break,
                    }
                }
                ATK::UnsignedLongLong => {
                    reject!(Char, Void, Long);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedLongLong,
                        TokenKind::Int => kind = ATK::UnsignedLongLongInt,
                        _ => break,
                    }
                }
                ATK::UnsignedLongLongInt => {
                    reject!(Char, Void, Long, Int);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedLongLongInt,
                        _ => break,
                    }
                }
                ATK::UnsignedChar => {
                    reject!(Char, Void, Long, Int);

                    match tok.kind {
                        TokenKind::Unsigned => kind = ATK::UnsignedChar,
                        _ => break,
                    }
                }

                ATK::Struct(_) | ATK::Ident(_) => unreachable!(),
            }

            end_loc = pop(tokens, current).unwrap().loc;

            tok = peek(tokens, current)?;
        }

        return Ok(ASTType {
            kind,
            loc: l_from(start_loc, end_loc),
        });
    }
}

pub fn unexpected_token(parsing_what: &str, tok: &Token) -> Error {
    return error!(
        &format!("unexpected token while parsing {}", parsing_what),
        tok.loc,
        format!("this was interpreted as {:?}", tok)
    );
}

pub fn any_ident_o<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Option<(u32, CodeLoc)> {
    let tok = peek_o(tokens, current)?;
    if let TokenKind::Ident(id) = tok.kind {
        pop(tokens, current).unwrap();
        return Some((id, tok.loc));
    } else if let TokenKind::TypeIdent(id) = tok.kind {
        pop(tokens, current).unwrap();
        return Some((id, tok.loc));
    } else {
        None
    }
}

pub fn expect_any_ident<'a>(
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<(u32, CodeLoc), Error> {
    let tok = pop(tokens, current)?;
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

pub fn expect_type_ident<'a>(
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<(u32, CodeLoc), Error> {
    let tok = pop(tokens, current)?;
    if let TokenKind::TypeIdent(id) = tok.kind {
        return Ok((id, tok.loc));
    } else {
        return Err(error!(
            "expected type identifier token, got something else instead",
            tok.loc,
            format!(
                "this was interpreted as {:?} when it should be a type identifier",
                tok
            )
        ));
    }
}

pub fn expect_ident<'a>(
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<(u32, CodeLoc), Error> {
    let tok = pop(tokens, current)?;
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

pub fn expect_rbracket<'a>(
    tokens: &[Token<'a>],
    current: &mut usize,
    lbracket_loc: CodeLoc,
) -> Result<Token<'a>, Error> {
    let tok = pop(tokens, current)?;
    if tok.kind != TokenKind::RBracket {
        return Err(error!(
            "expected ']' token, got something else instead",
            tok.loc,
            format!("this was interpreted as {:?} when it should be a ']'", tok),
            lbracket_loc,
            "expected ']' because of matching '[' here"
        ));
    }
    return Ok(tok);
}

pub fn expect_lbrace<'a>(tokens: &[Token<'a>], current: &mut usize) -> Result<(), Error> {
    let tok = pop(tokens, current)?;
    if tok.kind != TokenKind::LBrace {
        return Err(error!(
            "expected '{' token, got something else instead",
            tok.loc,
            format!("this was interpreted as {:?} when it should be a '{{'", tok)
        ));
    }
    return Ok(());
}

pub fn expect_rbrace<'a>(
    tokens: &'a [Token<'a>],
    current: &mut usize,
    matching_tok: CodeLoc,
) -> Result<CodeLoc, Error> {
    let tok = pop(tokens, current)?;
    if tok.kind != TokenKind::RBrace {
        return Err(error!(
            "expected '}' token, got something else instead",
            tok.loc,
            format!("this was interpreted as {:?} when it should be a '}}'", tok),
            matching_tok,
            "matching left brace here".to_string()
        ));
    }
    return Ok(tok.loc);
}

pub fn expect_rparen<'a>(
    tokens: &'a [Token<'a>],
    current: &mut usize,
    matching_tok: CodeLoc,
) -> Result<CodeLoc, Error> {
    let tok = pop(tokens, current)?;
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

pub fn expect_lparen<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Result<Token<'a>, Error> {
    let tok = pop(tokens, current)?;
    if tok.kind != TokenKind::LParen {
        return Err(error!(
            "expected '(' token, got something else instead",
            tok.loc,
            format!("this was interpreted as {:?} when it should be a '('", tok)
        ));
    }
    return Ok(tok);
}

pub fn expect_comma<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Result<Token<'a>, Error> {
    let tok = pop(tokens, current)?;
    if tok.kind != TokenKind::Comma {
        return Err(error!(
            "expected ',' token, got something else instead",
            tok.loc,
            format!("this was interpreted as {:?} when it should be a ','", tok)
        ));
    }
    return Ok(tok);
}

pub fn eat_semicolon<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Result<Token<'a>, Error> {
    let tok = pop(tokens, current)?;
    expect_semicolon(&tok)?;
    return Ok(tok);
}

pub fn expect_semicolon(tok: &Token) -> Result<(), Error> {
    if tok.kind != TokenKind::Semicolon {
        return Err(error!(
            "expected ';' token, got something else instead",
            tok.loc,
            format!("this was interpreted as {:?} when it should be a ';'", tok)
        ));
    }
    return Ok(());
}
