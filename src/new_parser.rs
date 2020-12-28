use crate::buckets::*;
use crate::lexer::*;
use crate::new_ast::*;
use crate::util::*;
use std::collections::HashMap;

pub struct ParseEnv {
    pub symbol_is_type: Vec<HashMap<u32, bool>>, // true is type
}

impl ParseEnv {
    pub fn new() -> Self {
        Self {
            symbol_is_type: vec![HashMap::new()],
        }
    }

    pub fn is_typename(&self, ident: u32) -> bool {
        for scope in self.symbol_is_type.iter().rev() {
            if let Some(symbol) = scope.get(&ident) {
                return *symbol;
            }
        }
        false
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
        let scope_o = self.symbol_is_type.last_mut();
        let scope = scope_o.expect("at least one scope should be always present");
        scope.insert(sym, is_type);
    }
}

peg::parser! {
    grammar list_parser() for str {
        rule number() -> u32
            = n:$(['0'..='9']+) { n.parse().unwrap() }

        pub rule list() -> Vec<u32>
            = "[" l:number() ** "," "]" { l }
    }
}
