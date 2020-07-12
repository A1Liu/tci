use crate::ast::*;
use crate::buckets::BucketList;
use crate::errors::Error;
use crate::lexer::{Lexer, Token, TokenKind};

pub struct Parser<'a> {
    buckets: &'a mut BucketList<'a>,
    lexer: Lexer<'a>,
    token_stack: Vec<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(data: &'a str) -> Self {
        Self {
            buckets: BucketList::new(),
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

    pub fn parse_global_decl(&mut self) -> Result<GlobalStmt<'a>, Error> {
        let decl = self.parse_simple_decl()?;
        let tok = self.pop();
        if tok.kind == TokenKind::Semicolon {
            let range = decl.range.clone();
            return Ok(GlobalStmt {
                kind: GlobalStmtKind::Decl(decl),
                range,
            });
        }

        if tok.kind != TokenKind::LParen || decl.ident.is_none() {
            return Err(Error::new(
                "unexpected token when parsing function definition".to_string(),
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
                    "unexpected token when parsing end of function declaration".to_string(),
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
        let params = self.buckets.add_array(params);
        let end_decl_tok = self.pop();
        if end_decl_tok.kind == TokenKind::Semicolon {
            return Ok(GlobalStmt {
                kind: GlobalStmtKind::FuncDecl {
                    return_type: decl.decl_type,
                    ident: decl.ident.unwrap(),
                    params,
                },
                range: decl.range.start..end,
            });
        }

        if end_decl_tok.kind != TokenKind::LBrace {
            return Err(Error::new(
                "unexpected token when parsing ending of function declaration".to_string(),
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
        while brace_count > 0 {
            let tok = self.pop();
            match tok.kind {
                TokenKind::RBrace => brace_count -= 1,
                TokenKind::LBrace => brace_count += 1,
                TokenKind::End => {
                    return Err(Error::new(
                        "unexpected end of file while parsing function".to_string(),
                        vec![
                            (decl.range.start..end, "function declared here".to_string()),
                            (tok.range, "missing closing brace here".to_string()),
                        ],
                    ))
                }
                _ => {}
            }
            body.push(tok);
        }
        let body = self.buckets.add_array(body);
        return Ok(GlobalStmt {
            kind: GlobalStmtKind::Func {
                return_type: decl.decl_type,
                ident: decl.ident.unwrap(),
                params,
                body,
            },
            range: decl.range.start..end,
        });
    }

    pub fn parse_simple_decl(&mut self) -> Result<Decl<'a>, Error> {
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
                panic!("haven't handled expr case yet");
            } else {
                return Ok(Decl {
                    decl_type,
                    ident: Some(id),
                    value: None,
                    range: start..tok.range.end,
                });
            }
        }

        return Ok(Decl {
            decl_type,
            ident: None,
            value: None,
            range: start..tok.range.end,
        });
    }

    pub fn parse_type_prefix(&mut self) -> Result<ASTType<'a>, Error> {
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
            TokenKind::Struct => {}
            _ => {
                return Err(Error::new(
                    "unexpected token while parsing type".to_string(),
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

        let lbrace_tok = self.pop();
        if lbrace_tok.kind != TokenKind::LBrace {
            return Err(Error::new(
                "expected '{' token, got something else instead".to_string(),
                vec![(lbrace_tok.range, "should be a '{'".to_string())],
            ));
        }

        let mut members = Vec::new();

        while self.peek().kind != TokenKind::RBrace {
            let decl = self.parse_simple_decl()?;
            let semi_tok = self.pop();
            if semi_tok.kind != TokenKind::Semicolon {
                return Err(Error::new(
                    "expected ';' token, got something else instead".to_string(),
                    vec![(semi_tok.range, "should be a '{'".to_string())],
                ));
            }

            members.push(decl);
        }

        let members = self.buckets.add_array(members);
        return Ok(ASTType {
            kind: ASTTypeKind::StructDefn { ident, members },
            range: tok.range.start..self.pop().range.end,
            pointer_count: 0,
        });
    }
}
