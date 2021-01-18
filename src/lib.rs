#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]

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

#[cfg(test)]
mod test;

use codespan_reporting::term::termcolor::WriteColor;
use filedb::FileDb;
use js_sys::{Function as Func, Promise};
use runtime::*;
use std::collections::HashMap;
use util::*;
use wasm_bindgen::prelude::*;
use wasm_bindgen_futures::JsFuture;

#[derive(Debug, serde::Deserialize)]
#[serde(tag = "type", content = "payload")]
pub enum InMessage {
    Sources(HashMap<String, String>),
}

#[derive(Debug, serde::Serialize)]
#[serde(tag = "type", content = "payload")]
pub enum OutMessage {
    FileIds(HashMap<u32, String>),
    CompileError {
        rendered: String,
        errors: Vec<Error>,
    },
    InvalidInput(String),
    JumpTo(CodeLoc),
    Debug(String),
    Stdout(String),
    Stderr(String),
}

#[wasm_bindgen]
pub async fn run(send: Func, recv: Func, wait: Func) -> Result<(), JsValue> {
    use InMessage as In;
    use OutMessage as Out;

    let send = move |mes: Out| -> Result<(), JsValue> {
        let mut writer = StringWriter::new();
        serde_json::to_writer(&mut writer, &mes).unwrap();
        send.call1(&JsValue::UNDEFINED, &JsValue::from_str(&writer.to_string()))?;
        return Ok(());
    };

    let recv = || -> Result<Option<In>, JsValue> {
        let value = recv.call0(&JsValue::UNDEFINED)?;
        if value.is_undefined() || value.is_null() {
            return Ok(None);
        }

        let string = if let Some(s) = value.as_string() {
            s
        } else {
            send(Out::InvalidInput("didn't get a string".to_string()))?;
            return Ok(None);
        };

        let out = match serde_json::from_slice(string.as_bytes()) {
            Ok(o) => o,
            Err(e) => {
                send(Out::InvalidInput(format!("invalid input `{}`", string)))?;
                return Ok(None);
            }
        };

        return Ok(Some(out));
    };

    let wait = |timeout: u32| -> Result<JsFuture, JsValue> {
        let timeout = JsValue::from_f64(timeout as f64);
        let promise = Promise::from(wait.call1(&JsValue::UNDEFINED, &timeout)?);
        return Ok(JsFuture::from(promise));
    };

    #[allow(unused_macros)]
    macro_rules! debug {
        ($str:literal) => {{
            #[cfg(debug_assertions)]
            send(Out::Debug( format!($str) ))?;
        }};
        ($str:literal, $( $val:expr ),* ) => {{
            #[cfg(debug_assertions)]
            send(Out::Debug( format!($str, $( $val ),*) ))?;
        }};
        (@LOG, $str:literal) => {{
            send(Out::Debug( format!($str) ))?;
        }};
        (@LOG, $str:literal, $( $val:expr ),* ) => {{
            send(Out::Debug( format!($str, $( $val ),*) ))?;
        }};
    }

    let mut files = FileDb::new();
    let mut runtime = None;

    loop {
        debug!("running another iteration of loop...");

        while let Some(input) = recv()? {
            match input {
                In::Sources(input_files) => {
                    files = FileDb::new();
                    let mut out = HashMap::new();
                    for (name, contents) in input_files {
                        let file_id = files.add(&name, &contents).unwrap();
                        out.insert(file_id, name);
                    }

                    send(Out::FileIds(out))?;

                    let program = match compile(&mut files) {
                        Ok(p) => p,
                        Err(errors) => {
                            let mut writer = StringWriter::new();
                            emit_err(&errors, &files, &mut writer);
                            let rendered = writer.to_string();
                            send(Out::CompileError { rendered, errors })?;
                            continue;
                        }
                    };

                    runtime = Some(Runtime::new(&program));
                }
            }
        }

        let mut timeout = 1;
        if let Some(runtime) = runtime.as_mut() {
            let status = runtime.run_op_count(5000);

            match status {
                RuntimeStatus::Exited(code) => {
                    timeout = 0;
                    runtime.print_callstack(&files);
                }
                _ => {}
            }

            for TS(tag, s) in &runtime.events() {
                match tag {
                    WriteEvent::StdoutWrite => {
                        send(Out::Stdout(s.to_string()))?;
                    }
                    WriteEvent::StderrWrite => {
                        send(Out::Stderr(s.to_string()))?;
                    }
                }
            }

            send(Out::JumpTo(runtime.loc()))?;
        }

        wait(timeout)?.await?;
    }
}

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

    let files_list = env.impls();
    let files = files_list.iter();
    let lexed: Vec<_> = files
        .filter_map(compile_filter(|&idx| lexer.lex(idx), &mut errors))
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

    println!("parsed");

    let checked: Vec<_> = parsed
        .into_iter()
        .filter_map(compile_filter(
            |env: parser::ParseEnv| type_checker::check_tree(env.file, &symbols, &env.tree),
            &mut errors,
        ))
        .collect();

    if errors.len() != 0 {
        return Err(errors);
    }

    println!("type checked");

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

fn emit_err(errs: &[Error], files: &FileDb, writer: &mut impl WriteColor) {
    let config = codespan_reporting::term::Config::default();
    for err in errs {
        codespan_reporting::term::emit(writer, &config, files, &err.diagnostic()).unwrap();
    }
}
