use crate::buckets::*;
use crate::lexer::*;
use crate::new_ast::*;
use crate::util::*;
use std::collections::HashMap;

pub struct ParseEnv {
    pub symbol_is_type: HashMap<u32, bool>, // true is type
    pub parent: *const ParseEnv,
}

impl ParseEnv {
    pub fn new() -> Self {
        Self {
            symbol_is_type: HashMap::new(),
            parent: core::ptr::null(),
        }
    }

    pub fn child(&self) -> Self {
        Self {
            symbol_is_type: HashMap::new(),
            parent: self,
        }
    }

    pub fn is_typename(&self, ident: u32) -> bool {
        let mut env_ptr: *const ParseEnv = self;
        while !env_ptr.is_null() {
            let env = unsafe { &*env_ptr };
            if let Some(symbol) = env.symbol_is_type.get(&ident) {
                return *symbol;
            }

            env_ptr = env.parent;
        }

        return false;
    }

    pub fn handle_declarator(&mut self, mut declarator: &Declarator, is_type: bool) {
        loop {
            match declarator.kind {
                DeclaratorKind::Abstract => return,
                DeclaratorKind::Identifier(i) => {
                    self.add_symbol(i, is_type);
                    break;
                }
                DeclaratorKind::Declarator(d) => declarator = d,
            }
        }
    }

    pub fn add_symbol(&mut self, sym: u32, is_type: bool) {
        self.symbol_is_type.insert(sym, is_type);
    }
}

pub fn parse<'a>(buckets: BucketListRef<'a>, toks: &[Token]) -> &'a [GlobalStatement<'a>] {
    return &[];
}

pub struct Parser<'a, 'b> {
    pub tokens: &'a [Token<'a>],
    pub buckets: BucketListRef<'b>,
    pub current: usize,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(buckets: BucketListRef<'b>, tokens: &'a [Token<'a>]) -> Self {
        Self {
            tokens,
            buckets,
            current: 0,
        }
    }

    pub fn peek_o(&self) -> Option<Token<'a>> {
        return Some(*self.tokens.get(self.current)?);
    }

    pub fn peek2_o(&self) -> Option<Token<'a>> {
        return Some(*self.tokens.get(self.current + 1)?);
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

    /// True if the parse is about to see a type, false otherwise
    pub fn peek_type_or_expr(&self) -> Result<bool, Error> {
        let tok = self.peek()?;
        match tok.kind {
            TokenKind::TypeIdent(_) => return Ok(true),
            TokenKind::Int | TokenKind::Long => return Ok(true),
            TokenKind::Unsigned | TokenKind::Signed => return Ok(true),
            TokenKind::Float | TokenKind::Double => return Ok(true),
            TokenKind::Char | TokenKind::Struct | TokenKind::Void => return Ok(true),
            _ => return Ok(false),
        }
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

    pub fn expect_rbracket(&mut self, lbracket_loc: CodeLoc) -> Result<Token<'a>, Error> {
        let tok = self.pop()?;
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
            TokenKind::PlusEq => {
                self.pop().unwrap();
                let right = self.parse_assignment()?;
                let (right, left) = self.buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::BinOp(BinOp::AssignAdd, left, right),
                });
            }
            TokenKind::DashEq => {
                self.pop().unwrap();
                let right = self.parse_assignment()?;
                let (right, left) = self.buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::BinOp(BinOp::AssignSub, left, right),
                });
            }
            TokenKind::StarEq => {
                self.pop().unwrap();
                let right = self.parse_assignment()?;
                let (right, left) = self.buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::BinOp(BinOp::AssignMul, left, right),
                });
            }
            TokenKind::SlashEq => {
                self.pop().unwrap();
                let right = self.parse_assignment()?;
                let (right, left) = self.buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::BinOp(BinOp::AssignDiv, left, right),
                });
            }
            TokenKind::PercentEq => {
                self.pop().unwrap();
                let right = self.parse_assignment()?;
                let (right, left) = self.buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::BinOp(BinOp::AssignMod, left, right),
                });
            }

            TokenKind::LtLtEq => {
                self.pop().unwrap();
                let right = self.parse_assignment()?;
                let (right, left) = self.buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::BinOp(BinOp::AssignLShift, left, right),
                });
            }
            TokenKind::GtGtEq => {
                self.pop().unwrap();
                let right = self.parse_assignment()?;
                let (right, left) = self.buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::BinOp(BinOp::AssignRShift, left, right),
                });
            }
            TokenKind::AmpEq => {
                self.pop().unwrap();
                let right = self.parse_assignment()?;
                let (right, left) = self.buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::BinOp(BinOp::AssignBitAnd, left, right),
                });
            }
            TokenKind::CaretEq => {
                self.pop().unwrap();
                let right = self.parse_assignment()?;
                let (right, left) = self.buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::BinOp(BinOp::AssignBitXor, left, right),
                });
            }
            TokenKind::LineEq => {
                self.pop().unwrap();
                let right = self.parse_assignment()?;
                let (right, left) = self.buckets.add((right, left));
                return Ok(Expr {
                    loc: l_from(left.loc, right.loc),
                    kind: ExprKind::BinOp(BinOp::AssignBitOr, left, right),
                });
            }
            _ => {
                return Ok(left);
            }
        }
    }

    pub fn parse_ternary(&mut self) -> Result<Expr<'b>, Error> {
        let condition = self.parse_bool_or()?;

        let question_tok = self.peek()?;
        if question_tok.kind != TokenKind::Question {
            return Ok(condition);
        }

        self.pop().unwrap();

        let if_true = self.parse_expr()?;

        let colon_tok = self.pop()?;
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

        let if_false = self.parse_bool_or()?;

        let condition = self.buckets.add(condition);
        let if_true = self.buckets.add(if_true);
        let if_false = self.buckets.add(if_false);

        return Ok(Expr {
            loc: l_from(condition.loc, if_false.loc),
            kind: ExprKind::Ternary {
                condition,
                if_true,
                if_false,
            },
        });
    }

    pub fn parse_bool_or(&mut self) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_bool_and()?;
        loop {
            let start_loc = expr.loc;
            match self.peek()?.kind {
                TokenKind::LineLine => {
                    self.pop().unwrap();

                    let right = self.parse_bool_and()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::BoolOr, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_bool_and(&mut self) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_bit_or()?;
        loop {
            let start_loc = expr.loc;
            match self.peek()?.kind {
                TokenKind::AmpAmp => {
                    self.pop().unwrap();

                    let right = self.parse_bit_or()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::BoolAnd, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_bit_or(&mut self) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_bit_xor()?;
        loop {
            let start_loc = expr.loc;
            match self.peek()?.kind {
                TokenKind::Line => {
                    self.pop().unwrap();

                    let right = self.parse_bit_xor()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::BitOr, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_bit_xor(&mut self) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_bit_and()?;
        loop {
            let start_loc = expr.loc;
            match self.peek()?.kind {
                TokenKind::Caret => {
                    self.pop().unwrap();

                    let right = self.parse_bit_and()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::BitXor, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_bit_and(&mut self) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_equality()?;
        loop {
            let start_loc = expr.loc;
            match self.peek()?.kind {
                TokenKind::Amp => {
                    self.pop().unwrap();

                    let right = self.parse_equality()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::BitAnd, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_equality(&mut self) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_comparison()?;
        loop {
            let start_loc = expr.loc;
            match self.peek()?.kind {
                TokenKind::EqEq => {
                    self.pop().unwrap();

                    let right = self.parse_comparison()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Eq, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Neq => {
                    self.pop().unwrap();

                    let right = self.parse_comparison()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Neq, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_comparison(&mut self) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_shift()?;
        loop {
            let start_loc = expr.loc;
            match self.peek()?.kind {
                TokenKind::Lt => {
                    self.pop().unwrap();

                    let right = self.parse_shift()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Lt, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Leq => {
                    self.pop().unwrap();

                    let right = self.parse_shift()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Leq, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Gt => {
                    self.pop().unwrap();

                    let right = self.parse_shift()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Gt, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Geq => {
                    self.pop().unwrap();

                    let right = self.parse_shift()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Geq, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_shift(&mut self) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_add()?;
        loop {
            let start_loc = expr.loc;
            match self.peek()?.kind {
                TokenKind::GtGt => {
                    self.pop().unwrap();
                    let right = self.parse_add()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::RShift, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::LtLt => {
                    self.pop().unwrap();
                    let right = self.parse_add()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::LShift, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_add(&mut self) -> Result<Expr<'b>, Error> {
        let mut expr = self.parse_multiply()?;
        loop {
            let start_loc = expr.loc;
            match self.peek()?.kind {
                TokenKind::Plus => {
                    self.pop().unwrap();
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
                    self.pop().unwrap();
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
        let mut expr = self.parse_prefix()?;
        loop {
            let start_loc = expr.loc;
            match self.peek()?.kind {
                TokenKind::Slash => {
                    self.pop().unwrap();
                    let right = self.parse_prefix()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Div, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Star => {
                    self.pop().unwrap();
                    let right = self.parse_prefix()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Mul, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                TokenKind::Percent => {
                    self.pop().unwrap();
                    let right = self.parse_prefix()?;
                    let end_loc = right.loc;
                    let left = self.buckets.add(expr);
                    let right = self.buckets.add(right);

                    expr = Expr {
                        kind: ExprKind::BinOp(BinOp::Mod, left, right),
                        loc: l_from(start_loc, end_loc),
                    };
                }
                _ => return Ok(expr),
            }
        }
    }

    pub fn parse_prefix(&mut self) -> Result<Expr<'b>, Error> {
        let tok = self.peek()?;
        match tok.kind {
            // TokenKind::Sizeof => {
            //     self.pop().unwrap();

            //     let lparen_tok = self.peek()?;
            //     if lparen_tok.kind == TokenKind::LParen {
            //         self.pop().unwrap();

            //         if peek_type_or_expr(tokens, *current)? {
            //             let sizeof_type = self.parse_type_prefix()?;
            //             let mut pointer_count = 0;
            //             while self.peek()?.kind == TokenKind::Star {
            //                 pointer_count += 1;
            //                 self.pop().unwrap();
            //             }

            //             let rparen_loc = expect_rparen(tokens, current, lparen_tok.loc)?;
            //             return Ok(Expr {
            //                 kind: ExprKind::SizeofType {
            //                     sizeof_type,
            //                     pointer_count,
            //                 },
            //                 loc: l_from(tok.loc, rparen_loc),
            //             });
            //         }

            //         let target = self.parse_expr()?;
            //         let rparen_loc = expect_rparen(tokens, current, lparen_tok.loc)?;
            //         let target = self.buckets.add(target);
            //         return Ok(Expr {
            //             loc: l_from(tok.loc, rparen_loc),
            //             kind: ExprKind::SizeofExpr(target),
            //         });
            //     }

            //     let target = self.parse_prefix()?;
            //     let target = self.buckets.add(target);
            //     return Ok(Expr {
            //         loc: l_from(tok.loc, target.loc),
            //         kind: ExprKind::SizeofExpr(target),
            //     });
            // }
            TokenKind::Amp => {
                self.pop().unwrap();
                let target = self.parse_prefix()?;
                let target = self.buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::Ref(target),
                });
            }
            TokenKind::Star => {
                self.pop().unwrap();
                let target = self.parse_prefix()?;
                let target = self.buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::Deref(target),
                });
            }

            TokenKind::Bang => {
                self.pop().unwrap();
                let target = self.parse_prefix()?;
                let target = self.buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::UnaryOp(UnaryOp::BoolNot, target),
                });
            }
            TokenKind::Tilde => {
                self.pop().unwrap();
                let target = self.parse_prefix()?;
                let target = self.buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::UnaryOp(UnaryOp::BitNot, target),
                });
            }
            TokenKind::Dash => {
                self.pop().unwrap();
                let target = self.parse_prefix()?;
                let target = self.buckets.add(target);
                return Ok(Expr {
                    loc: l_from(tok.loc, target.loc),
                    kind: ExprKind::UnaryOp(UnaryOp::Neg, target),
                });
            }

            // TokenKind::LParen => {
            //     let (lparen, cast_to) = match peek_type_or_expr(tokens, *current + 1) {
            //         Ok(true) => {
            //             let lparen = self.pop().unwrap();
            //             let ast_type = self.parse_type_prefix()?;
            //             (lparen, ast_type)
            //         }
            //         _ => return self.parse_postfix(),
            //     };

            //     let mut pointer_count: u32 = 0;
            //     while self.peek()?.kind == TokenKind::Star {
            //         self.pop().unwrap();
            //         pointer_count += 1;
            //     }

            //     let end_loc = expect_rparen(tokens, current, lparen.loc)?;

            //     let target = self.parse_prefix()?;
            //     let target = self.buckets.add(target);

            //     return Ok(Expr {
            //         loc: l_from(lparen.loc, target.loc),
            //         kind: ExprKind::Cast {
            //             cast_to,
            //             cast_to_loc: l_from(lparen.loc, end_loc),
            //             pointer_count,
            //             expr: target,
            //         },
            //     });
            // }
            _ => return self.parse_postfix(),
        }
    }

    pub fn parse_postfix(&mut self) -> Result<Expr<'b>, Error> {
        let mut operand = self.parse_atom()?;
        let start_loc = operand.loc;

        loop {
            match self.peek()?.kind {
                TokenKind::LParen => {
                    self.pop().unwrap();
                    let mut params = Vec::new();
                    let rparen_tok = self.peek()?;

                    if rparen_tok.kind != TokenKind::RParen {
                        let param = self.parse_expr()?;
                        params.push(param);
                        let mut comma_tok = self.peek()?;

                        while comma_tok.kind == TokenKind::Comma {
                            self.pop().unwrap();
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

                    let end_loc = self.pop().unwrap().loc;
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
                        loc: l_from(start_loc, self.pop().unwrap().loc),
                    };
                }
                TokenKind::DashDash => {
                    operand = Expr {
                        kind: ExprKind::PostDecr(self.buckets.add(operand)),
                        loc: l_from(start_loc, self.pop()?.loc),
                    };
                }
                TokenKind::LBracket => {
                    let lbracket = self.pop().unwrap();
                    let index = self.parse_expr()?;
                    let rbracket_tok = self.expect_rbracket(lbracket.loc)?;

                    let loc = l_from(start_loc, rbracket_tok.loc);

                    operand = Expr {
                        kind: ExprKind::BinOp(
                            BinOp::Index,
                            self.buckets.add(operand),
                            self.buckets.add(index),
                        ),
                        loc,
                    };
                }
                TokenKind::Arrow => {
                    self.pop().unwrap();

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
                    self.pop().unwrap();

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
            TokenKind::CharLiteral(c) => {
                return Ok(Expr {
                    kind: ExprKind::CharLiteral(c),
                    loc: tok.loc,
                })
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
            // TokenKind::LBrace => {
            //     let start_loc = tok.loc;
            //     let mut expr = self.parse_expr()?;
            //     let mut expr_list = Vec::new();
            //     while self.peek()?.kind == TokenKind::Comma {
            //         expr_list.push(expr);
            //         self.pop().unwrap();
            //         expr = self.parse_expr()?;
            //     }

            //     let end_loc = expect_rbrace(tokens, current, tok.loc)?;

            //     if expr_list.len() == 0 {
            //         return Ok(expr);
            //     } else {
            //         expr_list.push(expr);
            //         return Ok(Expr {
            //             kind: ExprKind::BraceList(buckets.add_array(expr_list)),
            //             loc: l_from(start_loc, end_loc),
            //         });
            //     }
            // }

            // TokenKind::LParen => {
            //     let start_loc = tok.loc;
            //     let mut expr = self.parse_expr()?;
            //     let mut expr_list = Vec::new();
            //     while self.peek()?.kind == TokenKind::Comma {
            //         expr_list.push(expr);
            //         self.pop().unwrap();
            //         expr = self.parse_expr()?;
            //     }

            //     let end_loc = expect_rparen(tokens, current, tok.loc)?;

            //     if expr_list.len() == 0 {
            //         return Ok(expr);
            //     } else {
            //         expr_list.push(expr);
            //         return Ok(Expr {
            //             kind: ExprKind::ParenList(buckets.add_array(expr_list)),
            //             loc: l_from(start_loc, end_loc),
            //         });
            //     }
            // }
            _ => return Err(unexpected_token("expression", &tok)),
        }
    }
}

pub fn parse_global_stmt<'a>(buckets: BucketListRef<'a>, toks: &[Token], current: &mut usize) {}

pub fn unexpected_token(parsing_what: &str, tok: &Token) -> Error {
    return error!(
        &format!("unexpected token while parsing {}", parsing_what),
        tok.loc,
        format!("this was interpreted as {:?}", tok)
    );
}
