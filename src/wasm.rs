use crate::filedb::FileDb;
use crate::runtime::*;
use crate::util::*;
use crate::{compile, emit_err};
use alloc::alloc::{GlobalAlloc, Layout};
use core::sync::atomic::{AtomicUsize, Ordering};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

#[global_allocator]
static ALLOC: Stats = Stats {
    backing: wee_alloc::WeeAlloc::INIT,
    alloced_bytes: AtomicUsize::new(0),
    freed_bytes: AtomicUsize::new(0),
    alloc_count: AtomicUsize::new(0),
    free_count: AtomicUsize::new(0),
};

struct Stats {
    pub backing: wee_alloc::WeeAlloc<'static>,
    pub alloced_bytes: AtomicUsize,
    pub freed_bytes: AtomicUsize,
    pub alloc_count: AtomicUsize,
    pub free_count: AtomicUsize,
}

unsafe impl GlobalAlloc for Stats {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let a = self.backing.alloc(layout);

        if !a.is_null() {
            self.alloced_bytes
                .fetch_add(layout.size(), Ordering::SeqCst);
            self.alloc_count.fetch_add(1, Ordering::SeqCst);
        }

        return a;
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        self.backing.dealloc(ptr, layout);

        self.freed_bytes.fetch_add(layout.size(), Ordering::SeqCst);
        self.free_count.fetch_add(1, Ordering::SeqCst);
    }
}

// turn 1024 into 1Kb, and also the more extreme cases
pub fn display_byte_size(size: usize) -> (f32, &'static str) {
    let mut size = size as f32;
    let mut size_class = 0;
    while size >= 1024.0 {
        size /= 1024.0;
        size_class += 1;
    }

    let size_classes = ["", "Kb", "Mb", "Gb"];

    return (size, size_classes[size_class]);
}

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

use a::Error as JsError;
mod a {
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen]
    extern "C" {
        #[wasm_bindgen(js_namespace = console)]
        pub fn error(msg: String);

        #[wasm_bindgen]
        pub type Error;

        #[wasm_bindgen(constructor)]
        pub fn new() -> Error;

        #[wasm_bindgen(structural, method, getter)]
        pub fn stack(error: &Error) -> String;
    }
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

    std::panic::set_hook(Box::new(|info: &std::panic::PanicInfo| {
        let mut msg = info.to_string();

        // Add the error stack to our message.
        //
        // This ensures that even if the `console` implementation doesn't
        // include stacks for `console.error`, the stack is still available
        // for the user. Additionally, Firefox's console tries to clean up
        // stack traces, and ruins Rust symbols in the process
        // (https://bugzilla.mozilla.org/show_bug.cgi?id=1519569) but since
        // it only touches the logged message's associated stack, and not
        // the message's contents, by including the stack in the message
        // contents we make sure it is available to the user.
        msg.push_str("\n\nStack:\n\n");
        let e = JsError::new();
        let stack = e.stack();
        msg.push_str(&stack);

        // Safari's devtools, on the other hand, _do_ mess with logged
        // messages' contents, so we attempt to break their heuristics for
        // doing that by appending some whitespace.
        // https://github.com/rustwasm/console_error_panic_hook/issues/7
        msg.push_str("\n\n");

        // Finally, log the panic with `console.error`!
        out!(@CLEAN, "{}", msg);
    }));

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

        let alloced = ALLOC.alloced_bytes.load(Ordering::SeqCst);
        let freed = ALLOC.freed_bytes.load(Ordering::SeqCst);
        if alloced < freed {
            let (alloced, alloced_suffix) = display_byte_size(alloced);
            let (freed, freed_suffix) = display_byte_size(freed);

            debug!(
                "stats: alloced {}{}, freed {}{}",
                alloced, alloced_suffix, freed, freed_suffix
            );
        }

        let (used, used_suffix) = display_byte_size(alloced - freed);
        debug!("stats: using {}{}", used, used_suffix);

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

        if kernel.current_proc == !0 {
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
