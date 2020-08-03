#![allow(dead_code)]
#![allow(unused_variables)]

use std::env;
use std::fs::read_to_string;
use std::io::Write;

mod ast;
mod ast_2;
mod buckets;
mod errors;
mod lexer;
mod parser;
mod parser_2;
mod type_checker;
mod util;

#[cfg(test)]
mod test;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use parser::Parser;

fn run_on_file<'a, 'b>(
    stdout: impl Write,
    stderr: impl Write,
    buckets: &'a buckets::BucketList<'b>,
    files: &mut SimpleFiles<&'a str, &'b str>,
    filename: &'a str,
) -> Result<(), Diagnostic<usize>> {
    let input = buckets.add_str(&read_to_string(filename).unwrap());
    let file_id = files.add(filename, input);

    return run_on_string(stdout, stderr, file_id, input);
}

fn run_on_string<'b>(
    _stdout: impl Write,
    mut stderr: impl Write,
    file_id: usize,
    input: &str,
) -> Result<(), Diagnostic<usize>> {
    write!(stderr, "---\n{}\n---\n\n", input).expect("why did this fail?");

    let mut parser = parser::Parser1::new(input);
    let mut type_checker = type_checker::TypeChecker1::new();
    let mut parse_result = Vec::new();
    loop {
        let decl = match parser.parse_global_decl() {
            Ok(x) => x,
            Err(e) => {
                return Err(Diagnostic::error().with_message(e.message).with_labels(
                    e.sections
                        .iter()
                        .map(|x| {
                            Label::primary(file_id, (x.0.start as usize)..(x.0.end as usize))
                                .with_message(&x.1)
                        })
                        .collect(),
                ))
            }
        };
        parse_result.push(decl);

        if parser.peek().kind == lexer::TokenKind::End {
            break;
        }
    }

    for stmt in parse_result.iter() {
        write!(stderr, "{:?}\n", stmt).expect("why did this fail?");
    }

    match type_checker.check_global_stmts(&parse_result) {
        Ok(()) => {}
        Err(e) => {
            return Err(Diagnostic::error().with_message(e.message).with_labels(
                e.sections
                    .into_iter()
                    .map(|x| {
                        let mut label =
                            Label::primary(file_id, (x.0.start as usize)..(x.0.end as usize));
                        label.message = x.1.clone();
                        label
                    })
                    .collect(),
            ))
        }
    }

    let (variables, functions, type_env) = (
        type_checker.values,
        type_checker.functions,
        type_checker.env,
    );

    for (function, tokens) in functions {
        let mut parser = parser_2::Parser2::new(&type_env, tokens);
        while parser.peek().kind != lexer::TokenKind::End {
            match parser.parse_stmt() {
                Ok(x) => {}
                Err(e) => {
                    return Err(Diagnostic::error().with_message(e.message).with_labels(
                        e.sections
                            .iter()
                            .map(|x| {
                                Label::primary(file_id, (x.0.start as usize)..(x.0.end as usize))
                            })
                            .collect(),
                    ))
                }
            }
        }
    }

    return Ok(());
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let buckets = buckets::BucketList::new();
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    for arg in args.iter().skip(1) {
        let mut files = SimpleFiles::new();
        match run_on_file(
            std::io::stdout(),
            std::io::stderr(),
            buckets,
            &mut files,
            arg,
        ) {
            Err(diagnostic) => {
                codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                    .expect("why did this fail?")
            }
            _ => {}
        }
    }
}
