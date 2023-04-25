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

    let files = env.impls();
    let file_count = files.len();
    let mut lexed = Vec::with_capacity(file_count);

    for &file in &*files {
        match lexer.lex(file) {
            Ok(t) => lexed.push(t),
            Err(e) => {
                errors.push(e);
                continue;
            }
        }
    }

    if errors.len() != 0 {
        return Err(errors);
    }

    let mut parsed = Vec::with_capacity(file_count);

    for (id, toks, locs) in lexed.into_iter() {
        match parser::parse(id, toks, locs) {
            Ok(t) => parsed.push(t),
            Err(e) => {
                errors.push(e);
                continue;
            }
        }
    }

    let symbols = lexer.symbols();

    if errors.len() != 0 {
        return Err(errors);
    }

    let mut checked = Vec::with_capacity(file_count);

    for env in parsed.into_iter() {
        match type_checker::check_tree(env.file, &symbols, &env.tree) {
            Ok(t) => checked.push(t),
            Err(e) => {
                errors.push(e);
                continue;
            }
        }
    }

    if errors.len() != 0 {
        return Err(errors);
    }

    let mut assembler = assembler::Assembler::new();
    for tu in checked.into_iter() {
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
