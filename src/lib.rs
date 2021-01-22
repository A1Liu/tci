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
use runtime::*;
use std::collections::HashMap;
use util::*;

#[cfg(target_arch = "wasm32")]
mod wasm {
    pub use core::convert::TryInto;
    pub use js_sys::{Function as Func, Promise, Uint8Array};
    pub use wasm_bindgen::prelude::*;
    pub use wasm_bindgen::JsCast;
    pub use wasm_bindgen_futures::JsFuture;
}

#[cfg(target_arch = "wasm32")]
use wasm::*;

#[cfg(target_arch = "wasm32")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[derive(Debug, serde::Deserialize)]
#[serde(tag = "type", content = "payload")]
pub enum InMessage {
    Source(String, String),
    // DeleteSource(String),
    Run,
}

#[derive(Debug, serde::Serialize)]
#[serde(tag = "type", content = "payload")]
pub enum OutMessage {
    Startup,
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
    Stdlog(String),
}

#[rustfmt::skip] // rustfmt deletes the keyword async
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
extern "C" {
    pub type RunEnv;

    #[wasm_bindgen(method)]
    async fn wait(this: &RunEnv, timeout: u32);
    #[wasm_bindgen(method)]
    fn send(this: &RunEnv, message: String);
    #[wasm_bindgen(method)]
    fn recv(this: &RunEnv) -> Option<String>;

    #[wasm_bindgen(method, js_name = get)]
    async fn get_file(this: &RunEnv, key: &str) -> JsValue;
    #[wasm_bindgen(method, js_name = set)]
    async fn set_file(this: &RunEnv, key: &str, val: Vec<u8>);
    // #[wasm_bindgen(method, js_name = del)]
    // async fn del_file(this: &RunEnv, key: &str);
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub async fn run(env: RunEnv) -> Result<(), JsValue> {
    use InMessage as In;
    use OutMessage as Out;

    let for_send = env.clone();
    let send = move |mes: Out| {
        let mut writer = StringWriter::new();
        serde_json::to_writer(&mut writer, &mes).unwrap();
        let for_send: RunEnv = for_send.clone().unchecked_into();
        for_send.send(writer.into_string());
    };

    let global_send = send.clone();
    register_output(move |s| global_send(Out::Debug(s)));

    let recv = || -> Option<In> {
        let string = env.recv()?;

        let out = match serde_json::from_slice(string.as_bytes()) {
            Ok(o) => o,
            Err(e) => {
                send(Out::InvalidInput(format!("invalid input `{}`", string)));
                return None;
            }
        };

        return Some(out);
    };

    async fn get_env(env: &RunEnv, key: &str) -> Vec<u8> {
        let buf = env.get_file(key).await;
        return buf.unchecked_into::<Uint8Array>().to_vec();
    }

    let mut files = FileDb::new();
    let mut runtime = None;

    send(Out::Startup);

    loop {
        debug!("running another iteration of loop...");

        while let Some(input) = recv() {
            match input {
                In::Source(name, contents) => {
                    let file_id = files.add(&name, &contents).unwrap();
                }
                In::Run => {
                    let program = match compile(&mut files) {
                        Ok(p) => p,
                        Err(errors) => {
                            let mut writer = StringWriter::new();
                            emit_err(&errors, &files, &mut writer);
                            let rendered = writer.to_string();
                            send(Out::CompileError { rendered, errors });
                            continue;
                        }
                    };

                    runtime = Some(Runtime::new(&program));
                }
            }
        }

        let mut timeout = 0;
        if let Some(runtime) = runtime.as_mut() {
            let result = runtime.run_op_count(5000);
            let status = match result {
                Ok(s) => s,
                Err(e) => {
                    let e_str = print_error(&e, &runtime.memory, &files);
                    runtime.output.push(WriteEvent::StderrWrite, &e_str);
                    RuntimeStatus::Exited(1)
                }
            };

            match status {
                RuntimeStatus::Exited(code) => timeout = 0,
                RuntimeStatus::Running => timeout = 1,
            }

            for TS(tag, s) in &runtime.events() {
                match tag {
                    WriteEvent::StdoutWrite => {
                        send(Out::Stdout(s.to_string()));
                    }
                    WriteEvent::StderrWrite => {
                        send(Out::Stderr(s.to_string()));
                    }
                    WriteEvent::StdlogWrite => {
                        send(Out::Stdlog(s.to_string()));
                    }
                }
            }

            send(Out::JumpTo(runtime.loc()));
        }

        env.wait(timeout).await;
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
