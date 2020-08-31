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

fn run<'a>(env: &mut FileDb<'a>, runtime_io: impl RuntimeIO) -> Result<i32, Error> {
    let mut buckets = buckets::BucketList::with_capacity(2 * env.size());
    let mut token_lists = Vec::new();

    let files: Vec<(u32, &str)> = env.iter().collect();
    let files = files.iter();
    let mut files = files.map(|(id, source)| (id, lexer::lex_file(buckets, env, *id, *source)));
    let files = files.try_fold((), |_, (id, t)| -> Result<(), Error> {
        Ok(token_lists.push((*id, t?)))
    })?;

    while let Some(n) = buckets.next() {
        buckets = n;
    }
    buckets = buckets.force_next();

    let iter = token_lists.into_iter();
    let mut iter = iter.map(|(file, tokens)| {
        let ast = parser::parse_tokens(buckets, file as u32, &tokens)?;
        let typed_ast = type_checker::check_file(buckets, file as u32, &ast)?;
        Ok(typed_ast)
    });

    let mut assembler = assembler::Assembler::new();
    iter.try_fold((), |prev, tenv| -> Result<(), Error> {
        assembler.add_file(tenv?)?;
        Ok(())
    })?;

    let program = assembler.assemble(&env)?;
    mem::drop(env);

    let mut runtime = interpreter::Runtime::new(runtime_io);
    Ok(runtime.run_program(program))
}

fn run_on_file(
    runtime_io: impl RuntimeIO,
    filename: &str,
    writer: &mut impl WriteColor,
) -> Result<i32, Error> {
    let buckets = buckets::BucketList::new();
    let mut files = FileDb::new(&[filename]);
    match run(&mut files, runtime_io) {
        Ok(x) => return Ok(x),
        Err(err) => {
            let config = codespan_reporting::term::Config::default();
            println!("{:?}", err);
            codespan_reporting::term::emit(writer, &config, &files, &err.diagnostic())
                .expect("why did this fail?");
            return Err(err);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let buckets = buckets::BucketList::new();

    let writer = StandardStream::stderr(ColorChoice::Always);
    let runtime_io = DefaultIO::new();

    let mut file_names: Vec<&str> = Vec::new();
    for arg in args.iter().skip(1) {
        file_names.push(&*arg);
    }

    let mut files = FileDb::new(&file_names);
    match run(&mut files, runtime_io) {
        Err(err) => {
            let config = codespan_reporting::term::Config::default();
            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &err.diagnostic())
                .expect("why did this fail?");
        }
        Ok(ret_code) => {
            eprintln!("TCI: return code was {}", ret_code);
            std::process::exit(ret_code as i32);
        }
    }
}
