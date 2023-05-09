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
pub mod format;
pub mod parser;
pub mod pass;

pub mod api {
    pub use super::ast::{
        self, AstDeclaration, AstDeclarator, AstDerivedDeclarator, AstExpr, AstFunctionDefinition,
        AstInterpretData, AstNode, AstNodeKind, AstNodeVec, AstSpecifier, AstStatement,
    };
    pub use super::error::{Error, ErrorKind, FileStarts, TranslationUnitDebugInfo};
    pub use super::filedb::{File, FileDb, Symbol, SymbolTable};
    pub use super::format::display_tree;
    pub use super::parser::{expand_macros, lex, parse, Token, TokenKind, TokenSlice, TokenVec};
    pub use super::pass::{
        types::{TyDb, TyId, TyQuals},
        ByKindAst,
    };

    pub use super::run_compiler_test_case;

    pub(crate) use rayon::prelude::*;
    pub(crate) use serde::{Deserialize, Serialize};
    pub(crate) use std::collections::HashMap;

    #[cfg(test)]
    pub use ntest::*;
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub enum StageOutput<T> {
    Ok(Vec<T>),
    Err(crate::error::Error),
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

#[derive(serde::Serialize, serde::Deserialize, Debug, Default)]
pub struct PipelineData {
    #[serde(skip)]
    pub translation_unit: error::TranslationUnitDebugInfo,

    #[serde(skip)]
    pub ty_db: api::TyDb,

    #[serde(default)]
    pub lexer: StageOutput<parser::TokenKind>,

    #[serde(default)]
    pub macro_expansion: StageOutput<parser::TokenKind>,

    #[serde(default)]
    pub parsed_ast: StageOutput<ast::AstNode>,

    #[serde(default)]
    pub ast_validation: StageOutput<ast::AstNode>,
}

impl PartialEq for PipelineData {
    fn eq(&self, other: &Self) -> bool {
        return self.lexer == other.lexer
            && self.macro_expansion == other.macro_expansion
            && self.parsed_ast == other.parsed_ast
            && self.ast_validation == other.ast_validation;
    }
}

impl PipelineData {
    pub fn errors(&self) -> Vec<&error::Error> {
        let mut errors = Vec::new();

        macro_rules! add_err {
            ($id:ident) => {
                if let StageOutput::Err(e) = &self.$id {
                    errors.push(e);
                }
            };
        }

        add_err!(lexer);
        add_err!(macro_expansion);
        add_err!(parsed_ast);
        add_err!(ast_validation);

        return errors;
    }
}

const TEST_CASE_DELIMITER: &'static str = "// -- END TEST CASE --\n// ";
pub type PrintFunc<'a> =
    &'a dyn Fn(&filedb::FileDb, &error::TranslationUnitDebugInfo, &error::Error);

pub fn single_file_db(mut source: String) -> (filedb::FileDb, u32) {
    if !source.ends_with("\n") {
        source.push('\n');
    }

    let mut files = filedb::FileDb::new();
    let file_id = files
        .add_file("main.c".to_string(), source)
        .expect("file should add properly");

    return (files, file_id);
}

// NOTE: the "source" field is empty
pub fn run_compiler_for_testing(files: &filedb::FileDb, file_id: u32) -> PipelineData {
    use crate::api::*;

    let file = &files.files[file_id as usize];

    let mut out = PipelineData {
        translation_unit: TranslationUnitDebugInfo::default(),
        ty_db: TyDb::default(),
        lexer: StageOutput::Err(error!(DidntRun)),
        macro_expansion: StageOutput::Err(error!(DidntRun)),
        parsed_ast: StageOutput::Err(error!(DidntRun)),
        ast_validation: StageOutput::Err(error!(DidntRun)),
    };

    macro_rules! run_stage {
        ($id:ident, $e:expr) => {
            match $e {
                Ok(res) => res,
                Err(e) => {
                    out.$id = StageOutput::Err(e);
                    return out;
                }
            }
        };
    }

    let (translation_unit, lexer_res) = lex(&files, file);
    out.translation_unit = translation_unit;

    let lexer_res = run_stage!(lexer, lexer_res);

    out.lexer = StageOutput::Ok(lexer_res.tokens.kind.clone());

    let macro_expansion_res = run_stage!(
        macro_expansion,
        expand_macros(lexer_res.tokens.as_slice(), files, &out.translation_unit)
    );
    out.macro_expansion = StageOutput::Ok(macro_expansion_res.kind.clone());

    let parsed_ast = run_stage!(parsed_ast, parse(&macro_expansion_res));
    out.parsed_ast = StageOutput::Ok(parsed_ast.iter().map(|n| n.to_owned()).collect());

    return out;
}

pub fn run_compiler_test_case<'a>(test_source: &'a str) -> (&'a str, PipelineData) {
    let (source, expected) = parse_test_case(test_source);

    let (db, file_id) = single_file_db(source.to_string());
    let output = run_compiler_for_testing(&db, file_id);
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
