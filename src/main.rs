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
use embedded_websocket as ws;
use embedded_websocket::{
    HttpHeader, WebSocketReceiveMessageType, WebSocketSendMessageType, WebSocketServer,
};
use filedb::FileDb;
use interpreter::Program;
use net_io::WebServerError;
use runtime::{DefaultIO, RuntimeIO};
use std::io::Read;
use std::net::{TcpListener, TcpStream};
use std::sync::Mutex;
use std::thread;
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

fn emit_err(errs: &Vec<Error>, files: &FileDb, writer: &mut impl WriteColor) {
    let config = codespan_reporting::term::Config::default();
    for err in errs {
        codespan_reporting::term::emit(writer, &config, files, &err.diagnostic())
            .expect("why did this fail?");
    }
}

fn run(program: interpreter::Program, runtime_io: impl RuntimeIO) -> i32 {
    let mut runtime = interpreter::Runtime::new(runtime_io);
    runtime.run_program(program)
}

fn compile_from_program_args() {
    let args: Vec<String> = std::env::args().collect();

    let writer = StandardStream::stderr(ColorChoice::Always);
    let runtime_io = DefaultIO::new();

    let mut files = FileDb::new();
    for arg in args.iter().skip(1) {
        files.add(&arg).unwrap();
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
            return;
        }
    };

    mem::drop(files);
}

enum GlobalState {
    Uninit,
    Args(Vec<String>),
    Compiled(Program<'static>),
}

static GLOBALS: LazyStatic<Mutex<GlobalState>> = lazy_static!(globals, Mutex<GlobalState>, {
    Mutex::new(GlobalState::Uninit)
});

fn main() -> std::io::Result<()> {
    let addr = "127.0.0.1:3000";
    let listener = TcpListener::bind(addr)?;
    println!("listening on: {}", addr);

    // accept connections and process them serially
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                thread::spawn(|| match handle_client(stream) {
                    Ok(()) => {}
                    Err(e) => println!("error: {:?}", e),
                });
            }
            Err(e) => println!("failed to establish a connection: {}", e),
        }
    }

    Ok(())
}

fn handle_client(mut stream: TcpStream) -> Result<(), WebServerError> {
    println!("received connection");

    let buckets = buckets::BucketList::with_capacity(12288);

    // In lieu of defer
    struct BucketDealloc<'a>(buckets::BucketListRef<'a>);
    impl<'a> Drop for BucketDealloc<'a> {
        fn drop(&mut self) {
            unsafe { self.0.dealloc() };
        }
    }
    let dealloc_buckets = BucketDealloc(buckets);
    let tcp_recv = buckets.uninit(4096).unwrap();
    let ws_buf = buckets.uninit(4096).unwrap();
    let scratch_buf = buckets.uninit(4096).unwrap();

    let mut web_socket = WebSocketServer::new_server();
    let mut num_bytes = 0;

    loop {
        let mut headers = [ws::httparse::EMPTY_HEADER; 32];

        if num_bytes >= tcp_recv.len() {
            return Err(WebServerError::RequestTooLarge(num_bytes));
        }

        num_bytes += stream.read(&mut tcp_recv[num_bytes..])?;

        // read until the stream is closed (zero bytes read from the stream)
        if num_bytes == 0 {
            return Ok(());
        }

        // assume that the client has sent us an http request. Since we may not read the
        // header all in one go we need to check for HttpHeaderIncomplete and continue reading
        let http_header = match ws::read_http_header(&mut headers, &tcp_recv[..num_bytes]) {
            Ok(header) => {
                num_bytes = 0;
                header
            }
            Err(ws::Error::HttpHeaderIncomplete) => continue,
            Err(e) => return Err(WebServerError::WebSocket(e)),
        };

        if let Some(ws_context) = &http_header.websocket_context {
            let to_send = web_socket.server_accept(&ws_context.sec_websocket_key, None, ws_buf)?;
            net_io::write_to_stream(&mut stream, &ws_buf[..to_send])?;
            break;
        } else {
            respond_to_http_request(http_header, ws_buf, &mut stream)?;
            return Ok(());
        }
    }

    let mut ws_num_bytes = 0;
    loop {
        if num_bytes >= tcp_recv.len() {
            return Err(WebServerError::PayloadTooLarge(num_bytes));
        }

        num_bytes += stream.read(&mut tcp_recv[num_bytes..])?;
        if num_bytes == 0 {
            return Ok(());
        }

        let ws_result = web_socket.read(&tcp_recv[..num_bytes], &mut ws_buf[ws_num_bytes..])?;
        ws_num_bytes += ws_result.len_to;

        if ws_num_bytes == ws_buf.len() {
            return Err(WebServerError::RequestTooLarge(ws_num_bytes));
        }

        num_bytes -= ws_result.len_from;
        for i in 0..num_bytes {
            tcp_recv[i] = tcp_recv[i + ws_result.len_from];
        }

        if !ws_result.end_of_message {
            continue;
        }

        let ws_buffer = &ws_buf[..ws_num_bytes];
        ws_num_bytes = 0;
        let response = ws_respond(ws_result.message_type, ws_buffer, scratch_buf)?;
        match response {
            net_io::WSResponse::Response {
                message_type,
                message,
            } => {
                let to_send = web_socket.write(message_type, true, message, ws_buf)?;
                net_io::write_to_stream(&mut stream, &ws_buf[..to_send])?;
            }
            net_io::WSResponse::None => {}
            net_io::WSResponse::Close => return Ok(()),
        }
    }
}

// returns true to keep the connection open
fn respond_to_http_request(
    http_header: HttpHeader,
    buffer2: &mut [u8],
    stream: &mut TcpStream,
) -> Result<(), WebServerError> {
    match http_header.path {
        "/" => net_io::write_to_stream(stream, &ROOT_HTML.as_bytes())?,
        _ => {
            let html = "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
            net_io::write_to_stream(stream, &html.as_bytes())?;
        }
    }

    return Ok(());
}

fn ws_respond<'a>(
    message_type: WebSocketReceiveMessageType,
    ws_buffer: &[u8],
    out_buffer: &'a mut [u8],
) -> Result<net_io::WSResponse<'a>, WebServerError> {
    match message_type {
        WebSocketReceiveMessageType::Text => {
            let message = &mut out_buffer[..ws_buffer.len()];
            message.copy_from_slice(ws_buffer);

            return Ok(net_io::WSResponse::Response {
                message_type: WebSocketSendMessageType::Text,
                message,
            });
        }
        WebSocketReceiveMessageType::CloseCompleted => return Ok(net_io::WSResponse::Close),
        WebSocketReceiveMessageType::CloseMustReply => {
            let message = &mut out_buffer[..ws_buffer.len()];
            message.copy_from_slice(ws_buffer);

            return Ok(net_io::WSResponse::Response {
                message_type: WebSocketSendMessageType::CloseReply,
                message,
            });
        }
        WebSocketReceiveMessageType::Ping => {
            let message = &mut out_buffer[..ws_buffer.len()];
            message.copy_from_slice(ws_buffer);

            return Ok(net_io::WSResponse::Response {
                message_type: WebSocketSendMessageType::Pong,
                message,
            });
        }
        _ => return Ok(net_io::WSResponse::None),
    }
}

const ROOT_HTML : &str = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\nContent-Length: 2590\r\nConnection: close\r\n\r\n<!doctype html><html></html>";
