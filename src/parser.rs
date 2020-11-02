use crate::ast::*;
use crate::buckets::BucketListRef;
use crate::lexer::*;
use crate::util::*;
use core::slice;
use std::collections::HashMap;

pub type AstDb<'a> = HashMap<u32, &'a [GlobalStmt<'a>]>;
pub type FuncMacroDb<'a> = HashMap<u32, FuncMacro<'a>>;
pub type MacroDb<'a> = HashMap<u32, Macro<'a>>;

pub fn parse_tokens<'a, 'b>(
    buckets: BucketListRef<'b>,
    token_db: &TokenDb<'a>,
    ast_db: &mut AstDb<'b>,
    file: u32,
) -> Result<ASTProgram<'b>, Error> {
    if let Some(stmts) = ast_db.get(&file) {
        return Ok(ASTProgram { stmts });
    }

    let mut parser = Parser::new();
    let mut parse_result = Vec::new();
    parser.parse_tokens_rec(buckets, token_db, ast_db, file, &mut parse_result)?;
    let stmts = buckets.add_array(parse_result);
    let prev = ast_db.insert(file, stmts);
    debug_assert!(prev.is_none());
    return Ok(ASTProgram { stmts });
}

pub struct Parser<'b> {
    pub macros: HashMap<u32, Macro<'b>>,
    pub func_macros: HashMap<u32, FuncMacro<'b>>,
}

pub fn peek_o<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Option<Token<'a>> {
    return Some(*tokens.get(*current)?);
}

pub fn peek2_o<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Option<Token<'a>> {
    return Some(*tokens.get(*current + 1)?);
}

pub fn peek<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Result<Token<'a>, Error> {
    let map_err = || error!("expected token");
    peek_o(tokens, current).ok_or_else(map_err)
}

pub fn pop<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Result<Token<'a>, Error> {
    let tok = peek(tokens, current)?;
    *current += 1;
    Ok(tok)
}

impl<'b> Parser<'b> {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            func_macros: HashMap::new(),
        }
    }

    pub fn parse_tokens_rec<'a>(
        &mut self,
        mut buckets: BucketListRef<'b>,
        tdb: &TokenDb<'a>,
        adb: &mut AstDb<'b>,
        file: u32,
        parse_result: &mut Vec<GlobalStmt<'b>>,
    ) -> Result<(), Error> {
        let tokens = tdb[&file];
        let mut current = 0;

        loop {
            if peek_o(tokens, &mut current).is_none() {
                break;
            }

            self.parse_global_decls(buckets, tdb, adb, tokens, &mut current, parse_result)?;

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
        self.parse_bool_or(buckets, tokens, current)
    }

    pub fn parse_bool_or<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        self.parse_bool_and(buckets, tokens, current)
    }

    pub fn parse_bool_and<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        self.parse_bit_or(buckets, tokens, current)
    }

    pub fn parse_bit_or<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        self.parse_bit_xor(buckets, tokens, current)
    }

    pub fn parse_bit_xor<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        self.parse_bit_and(buckets, tokens, current)
    }

    pub fn parse_bit_and<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        self.parse_equality(buckets, tokens, current)
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

                    let right = self.parse_shift(buckets, tokens, current)?;
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

                    let right = self.parse_shift(buckets, tokens, current)?;
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
        self.parse_add(buckets, tokens, current)
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
        self.parse_prefix(buckets, tokens, current)
    }

    pub fn parse_prefix<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Expr<'b>, Error> {
        let tok = peek(tokens, current)?;
        match tok.kind {
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
            TokenKind::LParen => {
                let type_tok = if let Some(tok) = peek2_o(tokens, current) {
                    tok
                } else {
                    return self.parse_postfix(buckets, tokens, current);
                };

                let (lparen, cast_to) = match type_tok.kind {
                    TokenKind::Struct => {
                        let lparen = pop(tokens, current).unwrap();
                        let start_loc = pop(tokens, current).unwrap().loc;
                        let (ident, ident_loc) = expect_any_ident(tokens, current)?;

                        let ast_type = ASTType {
                            kind: ASTTypeKind::Struct { ident },
                            loc: l_from(start_loc, ident_loc),
                        };

                        (lparen, ast_type)
                    }
                    TokenKind::Char | TokenKind::Void | TokenKind::Int => {
                        let lparen = pop(tokens, current).unwrap();
                        let ast_type = parse_simple_type_prefix(tokens, current)?;
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
                    pop(tokens, current).unwrap();
                    let index = self.parse_expr(buckets, tokens, current)?;
                    expect_rbracket(tokens, current)?;
                    operand = Expr {
                        loc: l_from(start_loc, index.loc),
                        kind: ExprKind::Index {
                            ptr: buckets.add(operand),
                            index: buckets.add(index),
                        },
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
                        kind: ExprKind::List(buckets.add_array(expr_list)),
                        loc: l_from(start_loc, end_loc),
                    });
                }
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
            _ => return Err(unexpected_token("expression", &tok)),
        }
    }

    fn parse_simple_decl<'a>(
        &self,
        buckets: BucketListRef<'b>,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<Decl<'b>, Error> {
        let mut pointer_count: u32 = 0;
        while peek(tokens, current)?.kind == TokenKind::Star {
            pointer_count += 1;
            pop(tokens, current).unwrap();
        }

        let (ident, ident_loc) = expect_ident(tokens, current)?;
        let tok = peek(tokens, current)?;
        let expr = if tok.kind == TokenKind::Eq {
            pop(tokens, current).unwrap();
            self.parse_expr(buckets, tokens, current)?
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

    fn parse_inner_struct_decl<'a>(
        &self,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<InnerStructDecl, Error> {
        let decl_type = match peek(tokens, current)?.kind {
            TokenKind::Struct => {
                let start_loc = pop(tokens, current).unwrap().loc;
                let (ident, ident_loc) = expect_any_ident(tokens, current)?;

                ASTType {
                    kind: ASTTypeKind::Struct { ident },
                    loc: l_from(start_loc, ident_loc),
                }
            }
            _ => parse_simple_type_prefix(tokens, current)?,
        };

        let mut pointer_count: u32 = 0;
        while peek(tokens, current)?.kind == TokenKind::Star {
            pop(tokens, current).unwrap();
            pointer_count += 1;
        }

        let (ident, ident_loc) = expect_ident(tokens, current)?;

        return Ok(InnerStructDecl {
            loc: l_from(decl_type.loc, ident_loc),
            pointer_count,
            decl_type,
            ident,
        });
    }

    fn parse_param_decl<'a>(
        &self,
        tokens: &'a [Token<'a>],
        current: &mut usize,
    ) -> Result<ParamDecl, Error> {
        let vararg_tok = peek(tokens, current)?;
        if vararg_tok.kind == TokenKind::DotDotDot {
            pop(tokens, current).unwrap();
            return Ok(ParamDecl {
                kind: ParamKind::Vararg,
                loc: vararg_tok.loc,
            });
        }

        let struct_decl = self.parse_inner_struct_decl(tokens, current)?;
        return Ok(ParamDecl {
            kind: ParamKind::StructLike {
                decl_type: struct_decl.decl_type,
                pointer_count: struct_decl.pointer_count,
                ident: struct_decl.ident,
            },
            loc: struct_decl.loc,
        });
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
        ast_db: &mut AstDb<'b>,
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
            TokenKind::IncludeSys(include_id) => {
                pop(tokens, current).unwrap();
                if let Some(include_stmts) = ast_db.get(&include_id) {
                    decls.extend_from_slice(include_stmts);
                    return Ok(());
                }

                let mut include_stmts = Vec::new();
                self.parse_tokens_rec(buckets, token_db, ast_db, include_id, &mut include_stmts)?;
                let stmts = buckets.add_array(include_stmts);
                let prev = ast_db.insert(include_id, stmts);
                debug_assert!(prev.is_none());
                decls.extend_from_slice(stmts);

                return Ok(());
            }
            TokenKind::Include(include_id) => {
                pop(tokens, current).unwrap();
                if let Some(include_stmts) = ast_db.get(&include_id) {
                    decls.extend_from_slice(include_stmts);
                    return Ok(());
                }

                let mut include_stmts = Vec::new();
                self.parse_tokens_rec(buckets, token_db, ast_db, include_id, &mut include_stmts)?;
                let stmts = buckets.add_array(include_stmts);
                let prev = ast_db.insert(include_id, stmts);
                debug_assert!(prev.is_none());
                decls.extend_from_slice(stmts);

                return Ok(());
            }
            TokenKind::MacroDef(ident) => {
                let tok = pop(tokens, current).unwrap();

                if peek(tokens, current)?.kind == TokenKind::LParen {
                    let lparen = pop(tokens, current).unwrap();
                    let mut params = Vec::new();
                    if peek(tokens, current)?.kind != TokenKind::RParen {
                        params.push(expect_ident(tokens, current)?);
                        while peek(tokens, current)?.kind == TokenKind::Comma {
                            params.push(expect_ident(tokens, current)?);
                        }
                    }

                    expect_rparen(tokens, current, lparen.loc)?;

                    let expr = self.parse_expr(buckets, tokens, current)?;
                    let params = buckets.add_array(params);
                    self.func_macros.insert(
                        ident,
                        FuncMacro {
                            ident,
                            params,
                            expr,
                        },
                    );
                } else {
                    let expr = self.parse_expr(buckets, tokens, current)?;
                    self.macros.insert(ident, Macro { ident, expr });
                }

                let end_tok = pop(tokens, current)?;
                if end_tok.kind != TokenKind::MacroDefEnd {
                    return Err(error!(
                        "expected macro definition to end here",
                        end_tok.loc, "expected this to be a newline"
                    ));
                }
                return Ok(());
            }
            TokenKind::Struct => {
                let start_loc = pop(tokens, current).unwrap().loc;
                let (ident, ident_loc) = expect_any_ident(tokens, current)?;
                let tok = peek(tokens, current)?;

                if tok.kind == TokenKind::LBrace {
                    // parse as type definition
                    pop(tokens, current).unwrap();

                    let mut decls = Vec::new();
                    while peek(tokens, current)?.kind != TokenKind::RBrace {
                        decls.push(self.parse_inner_struct_decl(tokens, current)?);
                        eat_semicolon(tokens, current)?;
                    }

                    let end_loc = pop(tokens, current).unwrap().loc;
                    eat_semicolon(tokens, current)?;

                    ret_stmt!(GlobalStmt {
                        kind: GlobalStmtKind::StructDecl(StructDecl {
                            ident,
                            ident_loc,
                            loc: l_from(start_loc, end_loc),
                            members: Some(buckets.add_array(decls)),
                        }),
                        loc: l_from(start_loc, end_loc),
                    });
                }

                if tok.kind == TokenKind::Semicolon {
                    pop(tokens, current).unwrap();

                    ret_stmt!(GlobalStmt {
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
            _ => parse_simple_type_prefix(tokens, current)?,
        };

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
            params.push(self.parse_param_decl(tokens, current)?);
            let mut comma_tok = peek(tokens, current)?;
            while comma_tok.kind == TokenKind::Comma {
                pop(tokens, current).unwrap();
                params.push(self.parse_param_decl(tokens, current)?);
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
                pointer_count: decl.pointer_count,
                ident: decl.ident,
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
        let tok = peek(tokens, current)?;
        match &tok.kind {
            TokenKind::For => {
                let start_loc = pop(tokens, current).unwrap().loc;

                let lparen_tok = expect_lparen(tokens, current)?;

                let (first_part, semi_tok) = match peek(tokens, current)?.kind {
                    TokenKind::Char | TokenKind::Int | TokenKind::Void => {
                        let decl_type = parse_simple_type_prefix(tokens, current)?;
                        let (mut decls, decl) = self.parse_multi_decl(buckets, tokens, current)?;
                        decls.push(decl);
                        let semi = eat_semicolon(tokens, current)?;
                        (Ok((decl_type, decls)), semi)
                    }
                    TokenKind::Semicolon => {
                        let semi = eat_semicolon(tokens, current).unwrap();
                        let expr = Expr {
                            kind: ExprKind::Uninit,
                            loc: l(lparen_tok.loc.end, semi.loc.start, semi.loc.file),
                        };
                        (Err(expr), semi)
                    }
                    _ => {
                        let expr = self.parse_expr(buckets, tokens, current)?;
                        let semi = eat_semicolon(tokens, current)?;
                        (Err(expr), semi)
                    }
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
                    kind: ExprKind::List(post_exprs),
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

            TokenKind::Int | TokenKind::Char | TokenKind::Void => {
                let decl_type = parse_simple_type_prefix(tokens, current)?;
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

            TokenKind::Struct => {
                let start_loc = pop(tokens, current).unwrap().loc;
                let (ident, loc) = expect_any_ident(tokens, current)?;

                let decl_type = ASTType {
                    kind: ASTTypeKind::Struct { ident },
                    loc: l_from(start_loc, loc),
                };

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

            TokenKind::Include(_) | TokenKind::IncludeSys(_) => {
                return Err(error!(
                    "include directives aren't allowed inside functions",
                    tok.loc, "found here"
                ));
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
}

fn parse_simple_type_prefix<'a>(
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<ASTType, Error> {
    let tok = pop(tokens, current)?;
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
        _ => return Err(unexpected_token("type", &tok)),
    }
}

pub fn unexpected_token(parsing_what: &str, tok: &Token) -> Error {
    return error!(
        &format!("unexpected token while parsing {}", parsing_what),
        tok.loc,
        format!("this was interpreted as {:?}", tok)
    );
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

pub fn expect_rbracket<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Result<(), Error> {
    let tok = pop(tokens, current)?;
    if tok.kind != TokenKind::RBracket {
        return Err(error!(
            "expected ']' token, got something else instead",
            tok.loc,
            format!("this was interpreted as {:?} when it should be a ']'", tok)
        ));
    }
    return Ok(());
}

pub fn expect_lbrace<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Result<(), Error> {
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
