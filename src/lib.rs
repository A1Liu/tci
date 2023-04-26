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
    pub use super::run_test_code;

    #[cfg(debug_assertions)]
    pub use serde::{Deserialize, Serialize};

    #[cfg(test)]
    pub use ntest::*;
}

const TEST_CASE_DELIMITER: &'static str = "// -- END TEST CASE --\n";

#[derive(serde::Serialize)]
pub struct PipelineOutput<'a> {
    #[serde(skip_serializing)]
    source: &'a str,
    lexer: Vec<lexer::TokenKind>,
}

impl<'a> PipelineOutput<'a> {
    pub fn test_case(&self) -> String {
        let mut output = self.source.to_string();

        let text = serde_json::to_string(self).expect("failed to serialize test output");

        if !output.ends_with("\n") {
            output.push('\n');
        }

        output.push_str(TEST_CASE_DELIMITER);
        output.push_str(&text);
        output.push('\n');

        return output;
    }
}

#[derive(serde::Deserialize)]
pub struct PipelineInput {
    lexer: Option<Vec<lexer::TokenKind>>,
}

#[cfg(debug_assertions)]
pub fn run_test_code(test_source: &str) -> PipelineOutput {
    use crate::api::*;

    let (source, expected_str) = test_source
        .split_once(TEST_CASE_DELIMITER)
        .unwrap_or((test_source, "null"));

    let expected = serde_json::from_str::<Option<PipelineInput>>(expected_str)
        .expect("Test case expected value didn't parse")
        .unwrap_or(PipelineInput { lexer: None });

    let mut source_string = source.to_string();
    if !source_string.ends_with("\n") {
        source_string.push('\n');
    }

    let mut files = FileDb::new();
    let file_id = files
        .add_file("main.c".to_string(), source_string)
        .expect("file should add properly");
    let file = &files.files[file_id as usize];

    let res = lex(&files, file).expect("Expected lex to succeed");

    if let Some(expected) = &expected.lexer {
        assert_eq!(&res.tokens.kind, expected, "Invalid token stream");
    }

    return PipelineOutput {
        source,
        lexer: res.tokens.kind,
    };
}
