#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]

#[macro_use]
extern crate soa_derive;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate bitfield_struct;
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
        AstInterpretData, AstNode, AstNodeKind, AstNodeVec, AstSpecifier, AstStatement,
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

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug, Default)]
pub struct PipelineData {
    #[serde(default)]
    pub lexer: StageOutput<lexer::TokenKind>,

    #[serde(default)]
    pub macro_expansion: StageOutput<lexer::TokenKind>,

    #[serde(default)]
    pub parsed_ast: StageOutput<SimpleAstNode>,

    #[serde(default)]
    pub ast_validation: StageOutput<SimpleAstNode>,
}

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug)]
pub struct SimpleAstNode {
    pub kind: ast::AstNodeKind,
    pub parent: u32,
    pub post_order: u32,
    pub height: u16,
}

impl<'a> From<ast::AstNodeRef<'a>> for SimpleAstNode {
    fn from(node: ast::AstNodeRef) -> Self {
        return Self {
            kind: *node.kind,
            parent: *node.parent,
            post_order: *node.post_order,
            height: *node.height,
        };
    }
}

const TEST_CASE_DELIMITER: &'static str = "// -- END TEST CASE --\n// ";
pub type PrintFunc<'a> =
    &'a dyn Fn(&filedb::FileDb, &error::TranslationUnitDebugInfo, &error::Error);

// NOTE: the "source" field is empty
pub fn run_compiler_for_testing(mut source: String, print_err: Option<PrintFunc>) -> PipelineData {
    use crate::api::*;

    if !source.ends_with("\n") {
        source.push('\n');
    }

    let mut files = FileDb::new();
    let file_id = files
        .add_file("main.c".to_string(), source)
        .expect("file should add properly");
    let file = &files.files[file_id as usize];

    let mut out = PipelineData {
        lexer: StageOutput::Err(ErrorKind::DidntRun),
        macro_expansion: StageOutput::Err(ErrorKind::DidntRun),
        parsed_ast: StageOutput::Err(ErrorKind::DidntRun),
        ast_validation: StageOutput::Err(ErrorKind::DidntRun),
    };

    let lexer_res = match lex(&files, file) {
        Ok(res) => res,
        Err(e) => {
            if let Some(print) = print_err {
                print(&files, &e.translation_unit, &e.error);
            }

            out.lexer = StageOutput::Err(e.error.kind);
            return out;
        }
    };

    let tu = lexer_res.translation_unit;
    out.lexer = StageOutput::Ok(lexer_res.tokens.kind.clone());

    let macro_expansion_res = expand_macros(lexer_res.tokens.as_slice());
    out.macro_expansion = StageOutput::Ok(macro_expansion_res.kind.clone());

    let parsed_ast = match parse(&macro_expansion_res) {
        Ok(res) => res,
        Err(e) => {
            if let Some(print) = print_err {
                print(&files, &tu, &e);
            }

            out.parsed_ast = StageOutput::Err(e.kind);
            return out;
        }
    };

    out.parsed_ast = StageOutput::Ok(
        parsed_ast
            .as_slice()
            .into_iter()
            .map(SimpleAstNode::from)
            .collect(),
    );

    let mut parsed_ast = parsed_ast;
    {
        let mut by_kind = pass::ByKindAst::new(&mut parsed_ast);

        if let Err(e) = pass::validate_declaration_nodes(&mut by_kind) {
            if let Some(print) = print_err {
                print(&files, &tu, &e);
            }

            out.ast_validation = StageOutput::Err(e.kind);
            return out;
        }
    }

    println!("{}", ast::display_tree(&parsed_ast));

    out.ast_validation = StageOutput::Ok(
        parsed_ast
            .as_slice()
            .into_iter()
            .map(SimpleAstNode::from)
            .collect(),
    );

    return out;
}

pub fn run_compiler_test_case<'a>(test_source: &'a str) -> (&'a str, PipelineData) {
    let (source, expected) = parse_test_case(test_source);

    let output = run_compiler_for_testing(source.to_string(), None);
    assert_eq!(output, expected);

    return (source, output);
}

pub fn parse_test_case(test_source: &str) -> (&str, PipelineData) {
    let (source, expected_str) = test_source
        .split_once(TEST_CASE_DELIMITER)
        .unwrap_or((test_source, "null"));

    let expected = serde_json::from_str::<Option<PipelineData>>(expected_str)
        .expect("Test case expected value didn't parse")
        .unwrap_or_default();

    return (source, expected);
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
