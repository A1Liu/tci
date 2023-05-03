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
pub mod pass;

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

    pub use super::run_compiler_test_case;

    pub(crate) use serde::{Deserialize, Serialize};
    pub(crate) use std::collections::HashMap;

    #[cfg(test)]
    pub use ntest::*;
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum StageOutput<T> {
    Ok(Vec<T>),
    Err(crate::error::ErrorKind),
    Ignore,
}

impl<T> Default for StageOutput<T> {
    fn default() -> Self {
        Self::Ignore
    }
}

impl<T> PartialEq<StageOutput<T>> for StageOutput<T>
where
    T: PartialEq<T>,
{
    fn eq(&self, other: &StageOutput<T>) -> bool {
        match (self, other) {
            // If there's no stage, dw about it
            (Self::Ignore, _) => return true,
            (_, Self::Ignore) => return true,

            (Self::Ok(s), Self::Ok(o)) => return s == o,
            (Self::Err(s), Self::Err(o)) => return s == o,

            _ => return false,
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug)]
pub struct PipelineData {
    #[serde(default)]
    pub lexer: StageOutput<lexer::TokenKind>,

    #[serde(default)]
    pub macro_expansion: StageOutput<lexer::TokenKind>,

    #[serde(default)]
    pub parsed_ast: StageOutput<SimpleAstNode>,
}

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug)]
pub struct SimpleAstNode {
    pub kind: AstNodeKind,
    pub parent: u32,
    pub post_order: u32,
    pub height: u16,
}

const TEST_CASE_DELIMITER: &'static str = "// -- END TEST CASE --\n// ";

// NOTE: the "source" field is empty
pub fn run_compiler_for_testing(source: String) -> PipelineData {
    use crate::api::*;

    let mut files = FileDb::new();
    let file_id = files
        .add_file("main.c".to_string(), source)
        .expect("file should add properly");
    let file = &files.files[file_id as usize];

    let mut out = PipelineData {
        lexer: StageOutput::Err(ErrorKind::DidntRun),
        macro_expansion: StageOutput::Err(ErrorKind::DidntRun),
        parsed_ast: StageOutput::Err(ErrorKind::DidntRun),
    };

    let lexer_res = match lex(&files, file) {
        Ok(res) => res,
        Err(e) => {
            out.lexer = StageOutput::Err(e.error.kind);
            return out;
        }
    };
    out.lexer = StageOutput::Ok(lexer_res.tokens.kind.clone());

    let macro_expansion_res = expand_macros(lexer_res.tokens.as_slice());
    out.macro_expansion = StageOutput::Ok(macro_expansion_res.kind.clone());

    let parsed_ast = match parse(&macro_expansion_res) {
        Ok(res) => res,
        Err(e) => {
            out.parsed_ast = StageOutput::Err(e.kind);
            return out;
        }
    };

    let mut simple_ast = Vec::with_capacity(parsed_ast.len());
    for node in parsed_ast.as_slice() {
        simple_ast.push(SimpleAstNode {
            kind: *node.kind,
            parent: *node.parent,
            post_order: *node.post_order,
            height: *node.height,
        });
    }

    out.parsed_ast = StageOutput::Ok(simple_ast);

    return out;
}

pub fn run_compiler_test_case(test_source: &str) -> (&str, PipelineData) {
    let (source, expected_str) = test_source
        .split_once(TEST_CASE_DELIMITER)
        .unwrap_or((test_source, "null"));

    let mut source_string = source.to_string();
    if !source_string.ends_with("\n") {
        source_string.push('\n');
    }

    let expected = serde_json::from_str::<Option<PipelineData>>(expected_str)
        .expect("Test case expected value didn't parse")
        .unwrap_or(PipelineData {
            lexer: StageOutput::Ignore,
            macro_expansion: StageOutput::Ignore,
            parsed_ast: StageOutput::Ignore,
        });

    let output = run_compiler_for_testing(source_string);
    assert_eq!(output, expected);

    return (source, output);
}

impl PipelineData {
    pub fn test_case(&self, source: &str) -> String {
        let mut output = source.to_string();

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
