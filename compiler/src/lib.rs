#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]

use api::AstNodeKind;

#[macro_use]
extern crate soa_derive;
#[macro_use]
extern crate lazy_static;
// #[macro_use]
// extern crate bitfield_struct;
#[macro_use]
extern crate enum_derive;
#[macro_use]
extern crate macro_attr;

#[macro_use]
pub mod error;

pub mod ast;
pub mod filedb;
pub mod lexer;
pub mod macros;
pub mod parser;

#[cfg(test)]
mod tests;

pub mod api {
    pub use super::ast::{
        self, AstDeclaration, AstDeclarator, AstDerivedDeclarator, AstExpr, AstFunctionDefinition,
        AstNode, AstNodeKind, AstNodeVec, AstSpecifier, AstStatement,
    };
    pub use super::error::{Error, ErrorKind, FileStarts, TranslationUnitDebugInfo};
    pub use super::filedb::{File, FileDb, Symbol, SymbolTable};
    pub use super::lexer::{lex, Token, TokenKind, TokenSlice, TokenVec};
    pub use super::macros::expand_macros;
    pub use super::parser::parse;

    pub use super::run_test_code;

    pub(crate) use serde::{Deserialize, Serialize};
    pub(crate) use std::collections::HashMap;

    #[cfg(test)]
    pub use ntest::*;
}

#[derive(serde::Deserialize)]
pub struct PipelineInput {
    lexer: Option<Vec<lexer::TokenKind>>,
    macro_expansion: Option<Vec<lexer::TokenKind>>,
}

#[derive(serde::Serialize)]
pub struct PipelineOutput<'a> {
    #[serde(skip_serializing)]
    source: &'a str,

    lexer: Vec<lexer::TokenKind>,
    macro_expansion: Vec<lexer::TokenKind>,
    parsed_ast: Vec<SimpleAstNode>,
}

#[derive(serde::Serialize)]
pub struct SimpleAstNode {
    pub kind: AstNodeKind,
    pub parent: u32,
    pub post_order: u32,
    pub height: u16,
}

const TEST_CASE_DELIMITER: &'static str = "// -- END TEST CASE --\n// ";

pub fn run_test_code(test_source: &str) -> PipelineOutput {
    use crate::api::*;

    let (source, expected_str) = test_source
        .split_once(TEST_CASE_DELIMITER)
        .unwrap_or((test_source, "null"));

    let expected = serde_json::from_str::<Option<PipelineInput>>(expected_str)
        .expect("Test case expected value didn't parse")
        .unwrap_or(PipelineInput {
            lexer: None,
            macro_expansion: None,
        });

    let mut source_string = source.to_string();
    if !source_string.ends_with("\n") {
        source_string.push('\n');
    }

    let mut files = FileDb::new();
    let file_id = files
        .add_file("main.c".to_string(), source_string)
        .expect("file should add properly");
    let file = &files.files[file_id as usize];

    let lexer_res = lex(&files, file).expect("Expected lex to succeed");
    if let Some(expected) = &expected.lexer {
        assert_eq!(&lexer_res.tokens.kind, expected, "Invalid token stream");
    }

    let macro_expansion_res = expand_macros(lexer_res.tokens.as_slice());
    if let Some(expected) = &expected.macro_expansion {
        assert_eq!(&macro_expansion_res.kind, expected, "Invalid token stream");
    }

    let parsed_ast = parse(&macro_expansion_res).expect("parsing failed");
    let mut simple_ast = Vec::with_capacity(parsed_ast.len());
    for node in parsed_ast.as_slice() {
        simple_ast.push(SimpleAstNode {
            kind: *node.kind,
            parent: *node.parent,
            post_order: *node.post_order,
            height: *node.height,
        });
    }

    return PipelineOutput {
        source,
        lexer: lexer_res.tokens.kind,
        macro_expansion: macro_expansion_res.kind,
        parsed_ast: simple_ast,
    };
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
