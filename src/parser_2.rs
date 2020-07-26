use crate::ast_2::*;
use crate::buckets::BucketList;
use crate::errors::Error;
use crate::lexer::{Token, TokenKind};
use crate::parser::{ExprParser, Parser, TypeParser};
use crate::type_checker::*;
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
    _buckets: &'a mut BucketList<'b>,
    env: &'a TypeEnv<'a, 'b>,
    toks: &'a [Token],
    idx: usize,
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

    pub fn parse_stmt(&mut self) -> Result<Stmt, Error> {
        let tok = self.peek();
        match &tok.kind {
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
            _ => {
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

                Error::expect_semicolon(&self.pop())?;
                return Ok(Stmt {
                    range: decl.range.clone(),
                    kind: StmtKind::Decl(decl),
                });
            }
        }
    }
}
