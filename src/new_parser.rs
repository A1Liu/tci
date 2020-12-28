use crate::buckets::*;
use crate::lexer::*;
use crate::new_ast::*;
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
