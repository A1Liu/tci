use crate::filedb::FileDb;
use crate::runtime::*;
use crate::util::*;
use crate::{compile, emit_err};
use js_sys::Uint8Array;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

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
    #[wasm_bindgen(method, js_name = update)]
    async fn update_file(this: &RunEnv, key:&str, closure: JsValue);
    // #[wasm_bindgen(method, js_name = del)]
    // async fn del_file(this: &RunEnv, key: &str);
}

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

    async fn get_file(env: &RunEnv, key: &str) -> Vec<u8> {
        let buf = env.get_file(key).await;
        return buf.unchecked_into::<Uint8Array>().to_vec();
    }

    let mut files = FileDb::new();
    let mut memory = None;

    send(Out::Startup);

    loop {
        debug!("running another iteration of loop...");

        while let Some(input) = recv() {
            match input {
                In::Source(mut name, contents) => {
                    name += ":source";
                    let file_id = files.add(&name, &contents).unwrap();
                    env.set_file(&name, contents.into_bytes()).await;
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

                    memory = Some(Memory::new(&program));
                }
            }
        }

        if let Some(memory) = &mut memory {
            let ecall_req = match run_op_count(memory, 5000) {
                Ok(None) => {
                    env.wait(1).await;
                    continue;
                }
                Ok(Some(req)) => req,
                Err(e) => {
                    let e_str = print_error(&e, &memory, &files);
                    send(Out::Stderr(e_str));
                    EcallExt::Exit(1)
                }
            };

            match ecall_req {
                EcallExt::Exit(exit) => {
                    env.wait(0).await;
                    continue;
                }
                EcallExt::OpenFd { name, open_mode } => {}
                EcallExt::ReadFd {
                    len,
                    buf,
                    begin,
                    fd,
                } => {}
                EcallExt::WriteFd { buf, begin, fd } => {}
                EcallExt::AppendFd { buf, fd } => {}
            }
        }

        env.wait(1).await;
    }
}
