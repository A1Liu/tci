#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]

#[macro_use]
mod util;

#[macro_use]
mod net_io;

mod assembler;
mod ast;
mod buckets;
mod commands;
mod filedb;
mod interpreter;
mod lexer;
mod parser;
mod runtime;
mod type_checker;

#[cfg(test)]
mod test;

use codespan_reporting::term::termcolor::{ColorChoice, StandardStream, WriteColor};
use core::mem;
use core::ops::Deref;
use embedded_websocket::{HttpHeader, WebSocketReceiveMessageType, WebSocketSendMessageType};
use filedb::FileDb;
use interpreter::Program;
use net_io::WebServerError;
use runtime::DefaultIO;
use rust_embed::RustEmbed;
use std::borrow::Cow;
use util::*;

fn compile<'a>(env: &mut FileDb<'a>) -> Result<Program<'static>, Vec<Error>> {
    let mut buckets = buckets::BucketList::with_capacity(2 * env.size());
    let mut buckets_begin = buckets;
    let mut tokens = lexer::TokenDb::new();
    let mut asts = parser::AstDb::new();
    let mut errors: Vec<Error> = Vec::new();

    let files_list = env.vec();
    let files = files_list.iter();
    files.for_each(|&(id, source)| {
        let result = lexer::lex_file(buckets, &mut tokens, env, id, source);
        match result {
            Err(err) => {
                errors.push(err);
            }
            Ok(_) => {}
        }
    });

    if errors.len() != 0 {
        return Err(errors);
    }

    while let Some(n) = buckets.next() {
        buckets = n;
    }
    buckets = buckets.force_next();

    let iter = files_list.into_iter().filter_map(|(file, _)| {
        while let Some(n) = buckets.next() {
            buckets = n;
        }

        match parser::parse_tokens(buckets, &tokens, &mut asts, file) {
            Ok(x) => return Some(x),
            Err(err) => {
                errors.push(err);
                return None;
            }
        }
    });
    let asts: Vec<ast::ASTProgram> = iter.collect();

    while let Some(n) = buckets.next() {
        buckets = n;
    }
    buckets = buckets.force_next();

    let mut assembler = assembler::Assembler::new();
    asts.into_iter().for_each(|ast| {
        while let Some(n) = buckets.next() {
            buckets = n;
        }

        let tfuncs = match type_checker::check_file(buckets, ast) {
            Ok(x) => x,
            Err(err) => {
                errors.push(err);
                return;
            }
        };

        match assembler.add_file(tfuncs) {
            Ok(()) => {}
            Err(err) => {
                errors.push(err);
            }
        }
    });

    if errors.len() != 0 {
        return Err(errors);
    }

    let program = match assembler.assemble(&env) {
        Ok(x) => x,
        Err(err) => return Err(err.into()),
    };

    while let Some(b) = unsafe { buckets_begin.dealloc() } {
        buckets_begin = b;
    }

    Ok(program)
}

fn emit_err(errs: &[Error], files: &FileDb, writer: &mut impl WriteColor) {
    let config = codespan_reporting::term::Config::default();
    for err in errs {
        codespan_reporting::term::emit(writer, &config, files, &err.diagnostic())
            .expect("why did this fail?");
    }
}

fn run_from_args(args: Vec<String>) -> ! {
    let args: Vec<String> = std::env::args().collect();

    let writer = StandardStream::stderr(ColorChoice::Always);
    let runtime_io = DefaultIO::new();

    let mut files = FileDb::new(true);
    for arg in args.iter().skip(1) {
        files.add_from_fs(&arg).unwrap();
    }
    mem::drop(args);

    let program = match compile(&mut files) {
        Ok(program) => program,
        Err(errs) => {
            let config = codespan_reporting::term::Config::default();
            for err in errs {
                codespan_reporting::term::emit(
                    &mut writer.lock(),
                    &config,
                    &files,
                    &err.diagnostic(),
                )
                .expect("why did this fail?");
            }
            std::process::exit(1);
        }
    };

    mem::drop(files);

    let mut runtime = interpreter::Runtime::new(program, runtime_io, StringArray::new());
    match runtime.run() {
        Ok(code) => std::process::exit(code),
        Err(err) => {
            let print = interpreter::render_err(&err, &runtime.memory.callstack, &program);
            print!("{}", print);

            std::process::exit(1);
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        let server = net_io::WebServer {
            http_handler: respond_to_http_request,
            ws_handler: ws_respond,
        };
        let addr = "0.0.0.0:3000";
        server.serve(addr).expect("server errored");

        return;
    }

    run_from_args(args);
}

#[derive(RustEmbed)]
#[folder = "frontend/build/"]
struct Asset;

fn respond_to_http_request<'a>(
    http_header: HttpHeader,
    buffer2: &'a mut [u8],
) -> Result<net_io::HttpResponse<'a>, WebServerError> {
    const ROOT_HTML: &str = "<!doctype html><html></html>";
    let mut path_len = http_header.path.len();
    buffer2[..path_len].copy_from_slice(http_header.path.as_bytes());
    if buffer2[path_len - 1] == b'/' {
        buffer2[path_len..path_len + 10].copy_from_slice("index.html".as_bytes());
        path_len += 10;
    }

    let path = unsafe { core::str::from_utf8_unchecked(&buffer2[..path_len]) };
    println!("received request for path {}", path);
    let text = Asset::get(&path[1..]);

    if let Some(text) = text {
        let (body_buf, ct_buf);
        if let Cow::Borrowed(borrowed) = text {
            body_buf = borrowed;
            ct_buf = buffer2;
        } else {
            if text.len() > buffer2.len() {
                return Err(WebServerError::ResponseTooLarge(text.len()));
            }

            let tup = buffer2.split_at_mut(text.len());
            tup.0.copy_from_slice(&text);
            body_buf = tup.0;
            ct_buf = tup.1;
        }

        let info = infer::Infer::new();
        let content_type_opt = info.get(body_buf).map(|kind| kind.mime);
        let content_type = content_type_opt
            .as_ref()
            .map(|mime| mime.deref())
            .unwrap_or(net_io::CT_TEXT_HTML);
        if content_type.len() >= ct_buf.len() {
            return Err(WebServerError::ResponseTooLarge(
                text.len() + content_type.len(),
            ));
        }

        let ct_buf = &mut ct_buf[..content_type.len()];
        ct_buf.copy_from_slice(content_type.as_bytes());
        let content_type = unsafe { core::str::from_utf8_unchecked(ct_buf) };

        return Ok(net_io::HttpResponse {
            status: 200,
            body: body_buf,
            content_type,
        });
    }

    return Ok(net_io::HttpResponse {
        status: 404,
        body: ROOT_HTML.as_bytes(),
        content_type: net_io::CT_TEXT_HTML,
    });
}

pub struct WSRuntime<'a> {
    pub state: commands::WSState<'a>,
    pub results: Vec<commands::CommandResult>,
    pub results_idx: usize,
}

impl<'a> Default for WSRuntime<'a> {
    fn default() -> Self {
        Self {
            state: commands::WSState::default(),
            results: Vec::new(),
            results_idx: 0,
        }
    }
}

fn ws_respond<'a>(
    message_type: WebSocketReceiveMessageType,
    state: &mut WSRuntime,
    ws_buffer: &[u8],
    out_buffer: &'a mut [u8],
) -> Result<net_io::WSResponse<'a>, WebServerError> {
    if state.results_idx > state.results.len() {
        return Ok(net_io::WSResponse::None);
    }

    if state.results.len() == 0 {
        match message_type {
            WebSocketReceiveMessageType::Text => {
                let command = match serde_json::from_slice(ws_buffer) {
                    Ok(c) => c,
                    Err(err) => {
                        let len =
                            write_b!(out_buffer, "deserialization of command failed ({})", err)
                                .unwrap();
                        state.results_idx = 1;
                        return Ok(net_io::WSResponse::Response {
                            message_type: WebSocketSendMessageType::Text,
                            message: &out_buffer[..len],
                        });
                    }
                };

                let results = state.state.run_command(command);
                if results.len() == 0 {
                    return Ok(net_io::WSResponse::None);
                }

                state.results = results;
                state.results_idx = 0;
            }
            WebSocketReceiveMessageType::CloseCompleted => return Ok(net_io::WSResponse::Close),
            WebSocketReceiveMessageType::CloseMustReply => {
                let message = &mut out_buffer[..ws_buffer.len()];
                message.copy_from_slice(ws_buffer);

                state.results_idx = 1;
                return Ok(net_io::WSResponse::Response {
                    message_type: WebSocketSendMessageType::CloseReply,
                    message,
                });
            }
            WebSocketReceiveMessageType::Ping => {
                let message = &mut out_buffer[..ws_buffer.len()];
                message.copy_from_slice(ws_buffer);

                state.results_idx = 1;
                return Ok(net_io::WSResponse::Response {
                    message_type: WebSocketSendMessageType::Pong,
                    message,
                });
            }
            _ => return Ok(net_io::WSResponse::None),
        }
    }

    if state.results_idx == state.results.len() {
        state.results = Vec::new();
        state.results_idx = 0;
        return Ok(net_io::WSResponse::None);
    }

    let mut cursor = std::io::Cursor::new(&mut out_buffer[..]);
    serde_json::to_writer(&mut cursor, &state.results[state.results_idx]).unwrap();
    let len = cursor.position() as usize;
    state.results_idx += 1;

    return Ok(net_io::WSResponse::Response {
        message_type: WebSocketSendMessageType::Text,
        message: &out_buffer[..len],
    });
}
