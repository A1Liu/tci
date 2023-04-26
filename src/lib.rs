#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]

#[macro_use]
extern crate soa_derive;
#[macro_use]
extern crate lazy_static;

mod error;
mod filedb;
mod lexer;
mod macros;

#[cfg(test)]
mod tests;

pub mod api {
    pub use super::error::Error;
    pub use super::filedb::{File, FileDb, Symbol, SymbolTable};
    pub use super::lexer::{lex, Token, TokenKind, TokenVec};
    pub use std::collections::HashMap;

    #[cfg(debug_assertions)]
    pub use serde::{Deserialize, Serialize};

    #[cfg(test)]
    pub use ntest::*;
}
