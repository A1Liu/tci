use crate::ast::*;
use crate::buckets::BucketListRef;
use crate::lexer::*;
use crate::util::*;
use core::slice;
use std::collections::HashMap;

pub type AstDb<'a> = HashMap<u32, &'a [GlobalStmt<'a>]>;

pub fn parse_tokens<'a, 'b>(
    buckets: BucketListRef<'b>,
    token_db: &TokenDb<'a>,
    ast_db: &mut AstDb<'b>,
    file: u32,
) -> Result<ASTProgram<'b>, Error> {
    if let Some(stmts) = ast_db.get(&file) {
        return Ok(ASTProgram { stmts });
    }

    let mut parse_result = Vec::new();
    parse_tokens_rec(buckets, token_db, ast_db, file, &mut parse_result)?;
    let stmts = buckets.add_array(parse_result);
    let prev = ast_db.insert(file, stmts);
    debug_assert!(prev.is_none());
    return Ok(ASTProgram { stmts });
}

pub fn parse_tokens_rec<'a, 'b>(
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

        parse_global_decls(buckets, tdb, adb, tokens, &mut current, parse_result)?;

        while let Some(next) = buckets.next() {
            buckets = next;
        }
    }

    return Ok(());
}

pub fn peek_o<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Option<Token<'a>> {
    let tok = *tokens.get(*current)?;
    return Some(tok);
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

#[inline]
pub fn parse_expr<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    return parse_assignment(buckets, tokens, current);
}

pub fn parse_assignment<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    let left = parse_ternary(buckets, tokens, current)?;
    match peek(tokens, current)?.kind {
        TokenKind::Eq => {
            pop(tokens, current).unwrap();
            let right = parse_assignment(buckets, tokens, current)?;
            let (right, left) = buckets.add((right, left));
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

pub fn parse_ternary<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    parse_bool_or(buckets, tokens, current)
}

pub fn parse_bool_or<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    parse_bool_and(buckets, tokens, current)
}

pub fn parse_bool_and<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    parse_bit_or(buckets, tokens, current)
}

pub fn parse_bit_or<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    parse_bit_xor(buckets, tokens, current)
}

pub fn parse_bit_xor<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    parse_bit_and(buckets, tokens, current)
}

pub fn parse_bit_and<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    parse_equality(buckets, tokens, current)
}

pub fn parse_equality<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    parse_comparison(buckets, tokens, current)
}

pub fn parse_comparison<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    parse_shift(buckets, tokens, current)
}

pub fn parse_shift<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    parse_add(buckets, tokens, current)
}

pub fn parse_add<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    let mut expr = parse_multiply(buckets, tokens, current)?;
    loop {
        let start_loc = expr.loc;
        match peek(tokens, current)?.kind {
            TokenKind::Plus => {
                pop(tokens, current).expect("shouldn't fail");
                let right = parse_multiply(buckets, tokens, current)?;
                let end_loc = right.loc;
                let left = buckets.add(expr);
                let right = buckets.add(right);

                expr = Expr {
                    kind: ExprKind::BinOp(BinOp::Add, left, right),
                    loc: l_from(start_loc, end_loc),
                };
            }
            TokenKind::Dash => {
                pop(tokens, current).expect("shouldn't fail");
                let right = parse_multiply(buckets, tokens, current)?;
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

pub fn parse_multiply<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    parse_prefix(buckets, tokens, current)
}

pub fn parse_prefix<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    let tok = peek(tokens, current)?;
    match tok.kind {
        TokenKind::Amp => {
            pop(tokens, current).expect("shouldn't fail");
            let target = parse_prefix(buckets, tokens, current)?;
            let target = buckets.add(target);
            return Ok(Expr {
                loc: l_from(tok.loc, target.loc),
                kind: ExprKind::Ref(target),
            });
        }
        TokenKind::Star => {
            pop(tokens, current).expect("shouldn't fail");
            let target = parse_prefix(buckets, tokens, current)?;
            let target = buckets.add(target);
            return Ok(Expr {
                loc: l_from(tok.loc, target.loc),
                kind: ExprKind::Deref(target),
            });
        }
        _ => return parse_postfix(buckets, tokens, current),
    }
}

pub fn parse_postfix<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Expr<'b>, Error> {
    let mut operand = parse_atom(buckets, tokens, current)?;
    let start_loc = operand.loc;

    loop {
        match peek(tokens, current)?.kind {
            TokenKind::LParen => {
                pop(tokens, current).expect("shouldn't fail");
                let mut params = Vec::new();
                let rparen_tok = peek(tokens, current)?;

                if rparen_tok.kind != TokenKind::RParen {
                    let param = parse_expr(buckets, tokens, current)?;
                    params.push(param);
                    let mut comma_tok = peek(tokens, current)?;

                    while comma_tok.kind == TokenKind::Comma {
                        pop(tokens, current).expect("shouldn't fail");
                        params.push(parse_expr(buckets, tokens, current)?);
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

                let end_loc = pop(tokens, current).expect("shouldn't fail").loc;
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
                    loc: l_from(start_loc, pop(tokens, current).expect("shouldn't fail").loc),
                };
            }
            TokenKind::DashDash => {
                operand = Expr {
                    kind: ExprKind::PostDecr(buckets.add(operand)),
                    loc: l_from(start_loc, pop(tokens, current)?.loc),
                };
            }
            TokenKind::LBracket => {
                pop(tokens, current).expect("shouldn't fail");
                let index = parse_expr(buckets, tokens, current)?;
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
                pop(tokens, current).expect("shouldn't fail");

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
                pop(tokens, current).expect("shouldn't fail");

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

pub fn parse_atom<'a, 'b>(
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
            let mut expr = parse_expr(buckets, tokens, current)?;
            let mut expr_list = Vec::new();
            while peek(tokens, current)?.kind == TokenKind::Comma {
                expr_list.push(expr);
                pop(tokens, current).unwrap();
                expr = parse_expr(buckets, tokens, current)?;
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

fn parse_simple_decl<'a, 'b>(
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
        parse_expr(buckets, tokens, current)?
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

fn parse_inner_struct_decl<'a, 'b>(
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

fn parse_param_decl<'a>(tokens: &'a [Token<'a>], current: &mut usize) -> Result<ParamDecl, Error> {
    let vararg_tok = peek(tokens, current)?;
    if vararg_tok.kind == TokenKind::DotDotDot {
        pop(tokens, current).unwrap();
        return Ok(ParamDecl {
            kind: ParamKind::Vararg,
            loc: vararg_tok.loc,
        });
    }

    let struct_decl = parse_inner_struct_decl(tokens, current)?;
    return Ok(ParamDecl {
        kind: ParamKind::StructLike {
            decl_type: struct_decl.decl_type,
            pointer_count: struct_decl.pointer_count,
            ident: struct_decl.ident,
        },
        loc: struct_decl.loc,
    });
}

fn parse_multi_decl<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<(Vec<Decl<'b>>, Decl<'b>), Error> {
    let mut decl = parse_simple_decl(buckets, tokens, current)?;
    let mut tok = peek(tokens, current)?;
    let mut decls = Vec::new();

    while tok.kind == TokenKind::Comma {
        pop(tokens, current).unwrap();
        decls.push(decl);
        decl = parse_simple_decl(buckets, tokens, current)?;
        tok = peek(tokens, current)?;
    }

    return Ok((decls, decl));
}

pub fn parse_global_decls<'a, 'b>(
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
        TokenKind::Include(include_id) => {
            pop(tokens, current).unwrap();
            if let Some(include_stmts) = ast_db.get(&include_id) {
                decls.extend_from_slice(include_stmts);
                return Ok(());
            }

            let mut include_stmts = Vec::new();
            parse_tokens_rec(buckets, token_db, ast_db, include_id, &mut include_stmts)?;
            let stmts = buckets.add_array(include_stmts);
            let prev = ast_db.insert(include_id, stmts);
            debug_assert!(prev.is_none());
            decls.extend_from_slice(stmts);

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
                    decls.push(parse_inner_struct_decl(tokens, current)?);
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

    let (mut decls, decl) = parse_multi_decl(buckets, tokens, current)?;
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

    if decl.expr.kind != ExprKind::Uninit {
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
        params.push(parse_param_decl(tokens, current)?);
        let mut comma_tok = peek(tokens, current)?;
        while comma_tok.kind == TokenKind::Comma {
            pop(tokens, current).unwrap();
            params.push(parse_param_decl(tokens, current)?);
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
        body.push(parse_stmt(buckets, tokens, current)?);
    }
    let tok = pop(tokens, current).unwrap();

    let body = buckets.add_array(body);
    ret_stmt!(GlobalStmt {
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

pub fn parse_block<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<(&'b [Stmt<'b>], CodeLoc), Error> {
    match peek(tokens, current)?.kind {
        TokenKind::LBrace => {
            let start_loc = pop(tokens, current)?.loc;

            let mut stmts = Vec::new();
            while peek(tokens, current)?.kind != TokenKind::RBrace {
                stmts.push(parse_stmt(buckets, tokens, current)?);
            }
            let end_loc = pop(tokens, current)?.loc;

            if stmts.len() == 1 {
                if let StmtKind::Block(stmts) = stmts[0].kind {
                    return Ok((stmts, l_from(start_loc, end_loc)));
                } else {
                    return Ok((buckets.add_array(stmts), l_from(start_loc, end_loc)));
                }
            } else {
                return Ok((&*buckets.add_array(stmts), l_from(start_loc, end_loc)));
            }
        }
        _ => {
            let stmt = parse_stmt(buckets, tokens, current)?;
            let loc = stmt.loc;
            return Ok((slice::from_ref(buckets.add(stmt)), loc));
        }
    }
}

pub fn parse_stmt<'a, 'b>(
    buckets: BucketListRef<'b>,
    tokens: &'a [Token<'a>],
    current: &mut usize,
) -> Result<Stmt<'b>, Error> {
    let tok = peek(tokens, current)?;
    match &tok.kind {
        TokenKind::For => {
            let start_loc = pop(tokens, current).unwrap().loc;

            let lparen_tok = expect_lparen(tokens, current)?;

            let is_decl = match peek(tokens, current)?.kind {
                TokenKind::Char | TokenKind::Int | TokenKind::Void => true,
                _ => false,
            };

            let first_part = if is_decl {
                let decl_type = parse_simple_type_prefix(tokens, current)?;
                let (mut decls, decl) = parse_multi_decl(buckets, tokens, current)?;
                decls.push(decl);
                Ok((decl_type, decls))
            } else {
                Err(parse_expr(buckets, tokens, current)?)
            };
            eat_semicolon(tokens, current)?;

            let condition = parse_expr(buckets, tokens, current)?;
            eat_semicolon(tokens, current)?;

            let post_expr = parse_expr(buckets, tokens, current)?;
            eat_semicolon(tokens, current)?;

            expect_rparen(tokens, current, lparen_tok.loc)?;

            let (body, body_loc) = parse_block(buckets, tokens, current)?;

            match first_part {
                Ok((decl_type, decls)) => {
                    return Ok(Stmt {
                        kind: StmtKind::ForDecl {
                            at_start_decl_type: decl_type,
                            at_start: buckets.add_array(decls),
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
                loc: pop(tokens, current).unwrap().loc,
            });
        }
        TokenKind::If => {
            let start_loc = pop(tokens, current).unwrap().loc;

            let lparen_tok = expect_lparen(tokens, current)?;
            let if_cond = parse_expr(buckets, tokens, current)?;
            expect_rparen(tokens, current, lparen_tok.loc)?;

            let (if_body, if_body_loc) = parse_block(buckets, tokens, current)?;

            if peek(tokens, current)?.kind != TokenKind::Else {
                return Ok(Stmt {
                    kind: StmtKind::Branch {
                        if_cond,
                        if_body,
                        else_body: None,
                    },
                    loc: l_from(tok.loc, if_body_loc),
                });
            }

            pop(tokens, current).unwrap();
            let (else_body, else_body_loc) = parse_block(buckets, tokens, current)?;

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
            let (stmts, loc) = parse_block(buckets, tokens, current)?;
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
            pop(tokens, current).unwrap();

            if peek(tokens, current)?.kind == TokenKind::Semicolon {
                pop(tokens, current).unwrap();
                return Ok(Stmt {
                    kind: StmtKind::Ret,
                    loc: tok.loc,
                });
            }

            let expr = parse_expr(buckets, tokens, current)?;
            eat_semicolon(tokens, current)?;

            return Ok(Stmt {
                loc: l_from(tok.loc, expr.loc),
                kind: StmtKind::RetVal(expr),
            });
        }
        TokenKind::Int | TokenKind::Char | TokenKind::Void => {
            let decl_type = parse_simple_type_prefix(tokens, current)?;
            let start_loc = decl_type.loc;
            let (mut decls, decl) = parse_multi_decl(buckets, tokens, current)?;
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
            let (mut decls, decl) = parse_multi_decl(buckets, tokens, current)?;
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
        _ => {
            let expr = parse_expr(buckets, tokens, current)?;
            eat_semicolon(tokens, current)?;

            return Ok(Stmt {
                loc: expr.loc,
                kind: StmtKind::Expr(expr),
            });
        }
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

pub fn eat_semicolon(tokens: &[Token], current: &mut usize) -> Result<(), Error> {
    let tok = pop(tokens, current)?;
    return expect_semicolon(&tok);
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
