#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]

extern crate alloc;

#[macro_use]
mod util;

#[macro_use]
mod runtime;

mod assembler;
mod ast;
mod buckets;
mod filedb;
mod lexer;
mod parser;
mod tc_ast;
mod tc_structs;
mod type_checker;

#[cfg(target_arch = "wasm32")]
mod wasm;

#[cfg(test)]
extern crate std;

#[cfg(test)]
mod test;

use filedb::FileDb;
use runtime::*;
use util::*;

#[cfg(target_arch = "wasm32")]
pub use wasm::run;

fn compile_filter<'a, In, T>(
    mut a: impl FnMut(In) -> Result<T, Error> + 'a,
    errs: &'a mut Vec<Error>,
) -> impl FnMut(In) -> Option<T> + 'a {
    return move |idx| match a(idx) {
        Ok(t) => return Some(t),
        Err(e) => {
            errs.push(e);
            return None;
        }
    };
}

fn compile(env: &FileDb) -> Result<BinaryData, Vec<Error>> {
    let mut errors: Vec<Error> = Vec::new();
    let mut lexer = lexer::Lexer::new(env);

    let files = env.impls().into_iter();
    let lexed: Vec<_> = files
        .filter_map(compile_filter(|idx| lexer.lex(idx), &mut errors))
        .collect();

    if errors.len() != 0 {
        return Err(errors);
    }

    let parsed: Vec<_> = lexed
        .into_iter()
        .filter_map(compile_filter(
            |(id, toks, locs)| parser::parse(id, toks, locs),
            &mut errors,
        ))
        .collect();

    let symbols = lexer.symbols();

    if errors.len() != 0 {
        return Err(errors);
    }

    let map = |env: parser::ParseEnv| type_checker::check_tree(env.file, &symbols, &env.tree);
    let checked: Vec<_> = parsed
        .into_iter()
        .filter_map(compile_filter(map, &mut errors))
        .collect();

    if errors.len() != 0 {
        return Err(errors);
    }

    let mut assembler = assembler::Assembler::new();
    for tu in checked {
        match assembler.add_file(tu) {
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

fn emit_err(errs: &[Error], files: &FileDb, writer: &mut impl core::fmt::Write) {
    for err in errs {
        err.render(files, writer).unwrap();
    }
}
