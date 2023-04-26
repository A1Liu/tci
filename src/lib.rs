#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]

#[macro_use]
extern crate soa_derive;
#[macro_use]
extern crate lazy_static;

mod error;
mod lexer;
mod macros;
mod symbol_table;

pub mod api {
    pub use super::error::Error;
    pub use super::lexer::{Lexer, Token, TokenKind, TokenVec};
    pub use super::symbol_table::{Symbol, SymbolTable};
    pub use std::collections::HashMap;
}
