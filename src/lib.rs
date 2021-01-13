#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]

#[macro_use]
mod util;

#[macro_use]
mod runtime;

// mod clang;

mod assembler;
mod ast;
mod buckets;
mod commands;
mod filedb;
mod interpreter;
mod lexer;
mod parser;
mod preprocessor;
mod tc_ast;
mod tc_structs;
mod type_checker;

#[cfg(test)]
mod test;

use codespan_reporting::term::termcolor::{ColorChoice, StandardStream, WriteColor};
use core::mem;
use filedb::FileDb;
use interpreter::Program;
use std::collections::HashMap;
use util::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet() {
    alert("Hello!");
}

fn compile(env: &mut FileDb) -> Result<Program, Vec<Error>> {
    let mut buckets = buckets::BucketListFactory::with_capacity(2 * env.size());
    let mut tokens = lexer::TokenDb::new();
    let mut errors: Vec<Error> = Vec::new();

    let files_list = env.vec();
    let files = files_list.iter();
    files.for_each(|&id| {
        let result = lexer::lex_file(&*buckets, &mut tokens, env, id);
        match result {
            Err(err) => errors.push(err),
            Ok(_) => {}
        }
    });

    if errors.len() != 0 {
        unsafe { buckets.dealloc() };
        return Err(errors);
    }

    let mut processed_tokens = HashMap::new();
    for &file in tokens.keys() {
        match preprocessor::preprocess_file(&tokens, file) {
            Ok(toks) => std::mem::drop(processed_tokens.insert(file, toks)),
            Err(err) => errors.push(err),
        }
    }

    let mut trees = HashMap::new();
    for (file, toks) in processed_tokens {
        match parser::parse(&toks) {
            Ok(env) => std::mem::drop(trees.insert(file, env)),
            Err(err) => errors.push(err),
        }
    }

    unsafe { buckets.dealloc() };

    if errors.len() != 0 {
        return Err(errors);
    }

    let mut tu_map = HashMap::new();
    for (file, tree) in trees {
        match type_checker::check_tree(env, &tree.tree) {
            Ok(tu) => std::mem::drop(tu_map.insert(file, tu)),
            Err(err) => errors.push(err),
        }
    }

    if errors.len() != 0 {
        return Err(errors);
    }

    let mut assembler = assembler::Assembler::new();

    for (file, tu) in tu_map {
        match assembler.add_file(file, tu) {
            Ok(_) => {}
            Err(err) => return Err(vec![err]),
        }
    }

    let program = match assembler.assemble(env) {
        Ok(x) => x,
        Err(err) => return Err(vec![err]),
    };

    return Ok(program);
}

fn emit_err(errs: &[Error], files: &FileDb, writer: &mut impl WriteColor) {
    let config = codespan_reporting::term::Config::default();
    for err in errs {
        codespan_reporting::term::emit(writer, &config, files, &err.diagnostic()).unwrap();
    }
}

fn run_from_args(args: Vec<String>) -> ! {
    let args: Vec<String> = std::env::args().collect();

    let writer = StandardStream::stderr(ColorChoice::Always);

    let mut files = FileDb::new(true);
    for arg in args.iter().skip(1) {
        files.add_from_fs(&arg).unwrap();
    }
    mem::drop(args);

    let program = match compile(&mut files) {
        Ok(program) => program,
        Err(errs) => {
            let config = codespan_reporting::term::Config::default();
            for err in errs {
                codespan_reporting::term::emit(
                    &mut writer.lock(),
                    &config,
                    &files,
                    &err.diagnostic(),
                )
                .unwrap();
            }
            std::process::exit(1);
        }
    };

    mem::drop(files);

    let mut runtime = interpreter::Runtime::new(program, StringArray::new());
    let diag = runtime.run(std::io::stdout());
    let code = match diag.status {
        runtime::RuntimeStatus::Exited(code) => code,
        _ => panic!(),
    };

    std::process::exit(code);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    run_from_args(args);
}
