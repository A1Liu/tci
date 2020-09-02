#![allow(dead_code)]
#![allow(unused_variables)]

#[macro_use]
extern crate lazy_static;

#[macro_use]
mod util;

mod assembler;
mod ast;
mod buckets;
mod filedb;
mod interpreter;
mod lexer;
mod parser;
mod runtime;
mod type_checker;

#[cfg(test)]
mod test;

use crate::filedb::FileDb;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream, WriteColor};
use core::mem;
use runtime::{DefaultIO, RuntimeIO};
use std::env;
use util::Error;

fn run<'a>(env: &mut FileDb<'a>, runtime_io: impl RuntimeIO) -> Result<i32, Vec<Error>> {
    let mut buckets = buckets::BucketList::with_capacity(2 * env.size());
    let token_lists: Vec<_>;
    let mut errors: Vec<Error> = Vec::new();

    let files: Vec<(u32, &str)> = env.iter().collect();
    let files = files.iter();
    let files = files.map(|(id, source)| lexer::lex_file(buckets, env, *id, *source));
    let files = files.filter_map(|lexed| match lexed {
        Err(err) => {
            errors.push(err);
            return None;
        }
        Ok(x) => return Some(x),
    });
    token_lists = files.collect();

    while let Some(n) = buckets.next() {
        buckets = n;
    }
    buckets = buckets.force_next();

    #[rustfmt::skip]
    let iter = token_lists.into_iter().map(|tokens| Ok(parser::parse_tokens(buckets, &tokens)?));
    let iter = iter.filter_map(|parsed| match parsed {
        Ok(x) => return x,
        Err(err) => {
            errors.push(err);
            return None;
        }
    });
    let asts: Vec<ast::ASTProgram> = iter.collect();

    let mut assembler = assembler::Assembler::new();
    let iter = asts.into_iter().for_each(|ast| {
        let tfuncs = match type_checker::check_file(buckets, ast) {
            Ok(x) => x,
            Err(err) => {
                errors.push(err);
                return;
            }
        };

        match assembler.add_file(tfuncs) {
            Ok(()) => {}
            Err(err) => {
                errors.push(err);
            }
        }
    });

    if errors.len() != 0 {
        return Err(errors);
    }

    let program = match assembler.assemble(&env) {
        Ok(x) => x,
        Err(err) => return Err(err.into()),
    };

    let mut runtime = interpreter::Runtime::new(runtime_io);
    Ok(runtime.run_program(program))
}

fn run_on_file(
    runtime_io: impl RuntimeIO,
    filename: &str,
    writer: &mut impl WriteColor,
) -> Result<i32, Vec<Error>> {
    let buckets = buckets::BucketList::new();
    let mut files = FileDb::new();
    files.add(filename).unwrap();
    match run(&mut files, runtime_io) {
        Ok(x) => return Ok(x),
        Err(errs) => {
            let config = codespan_reporting::term::Config::default();
            for err in &errs {
                codespan_reporting::term::emit(writer, &config, &files, &err.diagnostic())
                    .expect("why did this fail?");
            }
            return Err(errs);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let buckets = buckets::BucketList::new();

    let writer = StandardStream::stderr(ColorChoice::Always);
    let runtime_io = DefaultIO::new();

    let mut files = FileDb::new();
    for arg in args.iter().skip(1) {
        files.add(&arg).unwrap();
    }

    mem::drop(args);
    match run(&mut files, runtime_io) {
        Err(errs) => {
            let config = codespan_reporting::term::Config::default();
            for err in errs {
                codespan_reporting::term::emit(
                    &mut writer.lock(),
                    &config,
                    &files,
                    &err.diagnostic(),
                )
                .expect("why did this fail?");
            }
        }
        Ok(ret_code) => {
            eprintln!("TCI: return code was {}", ret_code);
            std::process::exit(ret_code as i32);
        }
    }
}
