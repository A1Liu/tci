use crate::filedb::FileDb;
use crate::runtime::*;
use crate::util::*;
use crate::{compile, emit_err};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[derive(Debug, serde::Deserialize)]
#[serde(tag = "type", content = "payload")]
pub enum InMessage {
    CharIn(char),
    Run(HashMap<String, String>),
}

#[derive(Debug, serde::Serialize)]
#[serde(tag = "type", content = "payload")]
pub enum OutMessage {
    Startup,
    Compiled,
    CompileError {
        rendered: String,
        errors: Vec<Error>,
    },
    InvalidInput(String),
    JumpTo(CodeLoc),
    Debug(String),
    Stdin(String),
    Stdout(String),
    Stderr(String),
    Stdlog(String),
    WriteFd {
        begin: u32,
        fd: u32,
        buf: Vec<u8>,
    },
    AppendFd {
        fd: u32,
        buf: Vec<u8>,
    },
    CreateFile {
        fd: u32,
        name: String,
    },
    ClearFd(u32),
}

#[rustfmt::skip] // rustfmt deletes the keyword async
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = JSON)]
    #[wasm_bindgen(catch)]
    pub fn stringify(val: JsValue) -> Result<String, JsValue>;

    #[wasm_bindgen(js_namespace = JSON)]
    #[wasm_bindgen(js_name = parse)]
    #[wasm_bindgen(catch)]
    pub fn from_string(s: String) -> Result<JsValue, JsValue>;

    pub type RunEnv;

    #[wasm_bindgen(method)]
    pub async fn wait(this: &RunEnv, timeout: u32);
    #[wasm_bindgen(method)]
    pub fn send(this: &RunEnv, message: JsValue);
    #[wasm_bindgen(method)]
    pub fn recv(this: &RunEnv) -> JsValue;

    #[wasm_bindgen(method, js_name = fileData)]
    pub fn file_data(this: &RunEnv, fd: u32) -> Vec<u8>;
    #[wasm_bindgen(method, js_name = fileName)]
    pub fn file_name(this: &RunEnv, fd: u32) -> String;
    #[wasm_bindgen(method, getter, js_name = fileDescriptors)]
    pub fn file_descriptors(this: &RunEnv) -> Vec<u32>;
    #[wasm_bindgen(method, setter, js_name = fileDescriptors)]
    pub fn set_file_descriptors(this: &RunEnv, new: JsValue);
}

// #[panic_handler]
// fn panic(_info: &core::panic::PanicInfo) -> ! {
//     core::arch::wasm32::unreachable();
// }

#[wasm_bindgen]
pub async fn run(env: RunEnv) -> Result<(), JsValue> {
    use InMessage as In;
    use OutMessage as Out;

    let for_send = env.clone();
    let send = move |mes: Out| {
        let for_send: RunEnv = for_send.clone().unchecked_into();
        let string = serde_json::to_string(&mes).unwrap();
        for_send.send(from_string(string).unwrap());
    };

    let global_send = send.clone();
    register_output(move |s| global_send(Out::Debug(s)));

    let recv = || -> Result<Option<In>, JsValue> {
        let js_value = env.recv();
        if js_value.is_undefined() || js_value.is_null() {
            return Ok(None);
        }

        let js_value_string = stringify(js_value)?;
        let out = match serde_json::from_str::<In>(&js_value_string) {
            Ok(o) => o,
            Err(e) => {
                send(Out::InvalidInput(js_value_string));
                return Ok(None);
            }
        };

        return Ok(Some(out));
    };

    debug!("initializing file system...");
    let mut files = FileDb::new();
    let initial: Vec<_> = {
        let mapper = |fd: &u32| (env.file_name(*fd), *fd, env.file_data(*fd));
        let init = env.file_descriptors().iter().map(mapper).collect();
        env.set_file_descriptors(JsValue::UNDEFINED);
        init
    };
    let mut kernel = Kernel::new(initial);
    let mut term_out_buf = StringWriter::new();

    send(Out::Startup);

    macro_rules! send_events {
        () => {{
            for TE(tag, s) in &kernel.events() {
                match tag {
                    WriteEvt::StdinWrite => {
                        write_utf8_lossy(&mut term_out_buf, s).unwrap();
                        send(Out::Stdin(term_out_buf.flush_string()));
                    }
                    WriteEvt::StdoutWrite => {
                        write_utf8_lossy(&mut term_out_buf, s).unwrap();
                        send(Out::Stdout(term_out_buf.flush_string()));
                    }
                    WriteEvt::StderrWrite => {
                        write_utf8_lossy(&mut term_out_buf, s).unwrap();
                        send(Out::Stderr(term_out_buf.flush_string()));
                    }
                    WriteEvt::StdlogWrite => {
                        write_utf8_lossy(&mut term_out_buf, s).unwrap();
                        send(Out::Stdlog(term_out_buf.flush_string()));
                    }
                    &WriteEvt::WriteFd { begin, fd } => {
                        send(Out::WriteFd {
                            begin,
                            fd,
                            buf: s.to_vec(),
                        });
                    }
                    &WriteEvt::AppendFd { fd } => {
                        send(Out::AppendFd {
                            fd,
                            buf: s.to_vec(),
                        });
                    }
                    &WriteEvt::ClearFd { fd } => {
                        send(Out::ClearFd(fd));
                    }
                    &WriteEvt::CreateFile { fd } => {
                        send(Out::CreateFile {
                            fd,
                            name: String::from_utf8(s.to_vec()).unwrap(),
                        });
                    }
                }
            }
        }};
    }

    loop {
        debug!("running another iteration of loop...");

        while let Some(input) = recv()? {
            match input {
                In::CharIn(c) => {
                    write!(kernel, "{}", c).unwrap();
                }
                In::Run(sources) => {
                    files = FileDb::new();
                    for (name, contents) in sources {
                        files.add(&name, &contents).unwrap();
                    }

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

                    send(Out::Compiled);
                    kernel.load_term_program(&program);
                }
            }
        }

        if kernel.active_count == 0 {
            send_events!();

            env.wait(0).await;
            continue;
        }

        debug!("running 5000 ops...");
        let result = kernel.run_op_count(5000);

        debug!("sending events...");
        send_events!();

        debug!("checking for ecalls...");
        match result {
            Ok(()) => {
                env.wait(1).await;
                continue;
            }
            Err(e) => {
                let e_str = print_error(&e, kernel.cur_mem().unwrap(), &files);
                send(Out::Stderr(e_str));
                env.wait(0).await;
                continue;
            }
        }
    }
}
