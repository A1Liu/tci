#![allow(dead_code)]
#![allow(unused_variables)]

#[macro_use]
extern crate lazy_static;

use std::env;
use std::fs::read_to_string;

#[macro_use]
mod util;

mod assembler;
mod ast;
mod buckets;
mod interpreter;
mod lexer;
mod parser;
mod runtime;
mod type_checker;

#[cfg(test)]
mod test;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::{Files, SimpleFiles};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream, WriteColor};
use runtime::{DefaultIO, RuntimeIO};
pub use util::{fold_binary, r, r_from, CodeLoc, Error, Range};

pub struct Environment<'a> {
    pub buckets: buckets::BucketListRef<'a>,
    pub files: SimpleFiles<&'a str, &'a str>,
}

fn run<'a>(env: &Environment<'a>, runtime_io: impl RuntimeIO) -> Result<(), Error> {
    let mut symbols = lexer::Symbols::new();
    let files = env.files.iter();

    let token_lists: Vec<Vec<lexer::Token>> = files
        .map(|file_id| lexer::lex_file(&mut symbols, env.files.source(file_id).unwrap()))
        .collect();

    let mut next = env.buckets.next();
    let mut end = env.buckets;
    while let Some(n) = next {
        end = n;
        next = n.next();
    }
    end = end.force_next();

    let iter = token_lists.into_iter().enumerate();
    let iter = iter.map(|(file, tokens)| {
        let ast = parser::parse_tokens(end, file as u32, &tokens)?;
        let typed_ast = type_checker::check_types(end, file as u32, &ast)?;
        Ok(typed_ast)
    });

    let mut assembler = assembler::Assembler::new();
    let assember = iter.fold(Ok(()), |prev, tenv| -> Result<(), Error> {
        prev?;
        assembler.add_file(tenv?)?;
        Ok(())
    });

    Ok(())
}

fn run_on_file(
    runtime_io: impl RuntimeIO,
    filename: &str,
    writer: &mut impl WriteColor,
) -> Result<(), Error> {
    let mut env = Environment {
        buckets: buckets::BucketList::new(),
        files: SimpleFiles::new(),
    };

    let filename = env.buckets.add_str(filename);
    let input = env.buckets.add_str(&read_to_string(&filename).unwrap());
    let file_id = env.files.add(filename, input);

    if let Err(err) = run(&env, runtime_io) {
        let config = codespan_reporting::term::Config::default();
        let diagnostic = Diagnostic::error().with_message(&err.message).with_labels(
            err.sections
                .iter()
                .map(|x| Label::primary(x.location.file, x.location.range).with_message(&x.message))
                .collect(),
        );

        codespan_reporting::term::emit(writer, &config, &env.files, &diagnostic)
            .expect("why did this fail?");
        return Err(err);
    }

    return Ok(());
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut env = Environment {
        buckets: buckets::BucketList::new(),
        files: SimpleFiles::new(),
    };

    let writer = StandardStream::stderr(ColorChoice::Always);
    let runtime_io = DefaultIO::new();

    for arg in args {
        let filename = env.buckets.add_str(&arg);
        let input = env.buckets.add_str(&read_to_string(&filename).unwrap());
        env.files.add(filename, input);
    }

    if let Err(err) = run(&env, runtime_io) {
        let config = codespan_reporting::term::Config::default();
        let diagnostic = Diagnostic::error().with_message(&err.message).with_labels(
            err.sections
                .iter()
                .map(|x| Label::primary(x.location.file, x.location.range).with_message(&x.message))
                .collect(),
        );

        codespan_reporting::term::emit(&mut writer.lock(), &config, &env.files, &diagnostic)
            .expect("why did this fail?");
    }
}
