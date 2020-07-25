use crate::ast_2::*;
use crate::buckets::BucketList;
use crate::errors::Error;
use crate::lexer::Token;
use crate::type_checker::*;
use std::collections::HashMap;

pub struct TypeEnv<'a, 'b> {
    pub buckets: &'a mut BucketList<'b>,
    pub global_struct_types: HashMap<u32, TCType<'b>>,
    pub global_types: HashMap<u32, TCType<'b>>,
    pub global_symbols: HashMap<u32, TCType<'b>>,
    pub global_func_types: HashMap<u32, TCFunc<'b>>,
}

impl<'a, 'b> TypeEnv<'a, 'b> {
    pub fn new(checker: TypeChecker1<'a, 'b>) -> (HashMap<u32, &'b [Token]>, Self) {
        let env = Self {
            buckets: checker.parser.buckets,
            global_struct_types: checker.struct_types,
            global_types: checker.types,
            global_symbols: checker.symbols,
            global_func_types: checker.func_types,
        };

        return (checker.functions, env);
    }
}

#[derive(Clone, Copy)]
pub struct Parser2<'a, 'b> {
    buckets: &'a BucketList<'b>,
    env: &'a TypeEnv<'a, 'b>,
    toks: &'a [Token],
}

impl<'a, 'b> Parser2<'a, 'b> {
    pub fn new(env: &'a TypeEnv<'a, 'b>, toks: &'a [Token]) -> Self {
        Self {
            buckets: BucketList::new(),
            env,
            toks,
        }
    }
}
