#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]

#[macro_use]
mod util;

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
    WebSocketState,
};
use filedb::FileDb;
use interpreter::Program;
use runtime::{DefaultIO, RuntimeIO};
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::str::Utf8Error;
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
        // println!("Tokens: {:?}", result);
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

enum GlobalState {
    Uninit,
    Args(Vec<String>),
    Compiled(Program<'static>),
}

static GLOBALS: LazyStatic<Mutex<GlobalState>> = lazy_static!(globals, Mutex<GlobalState>, {
    Mutex::new(GlobalState::Uninit)
});

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

// Below is licensed via the MIT License (MIT)
// Below is Copyright (c) 2019 David Haig

fn main() -> std::io::Result<()> {
    let addr = "127.0.0.1:3000";
    let listener = TcpListener::bind(addr)?;
    println!("Listening on: {}", addr);

    // accept connections and process them serially
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                thread::spawn(|| {
                    let buckets = buckets::BucketList::with_capacity(8192);
                    let send = buckets.uninit(4096).unwrap();
                    let receive = buckets.uninit(4096).unwrap();

                    match handle_client(stream, send, receive) {
                        Ok(()) => {}
                        Err(e) => println!("Error: {:?}", e),
                    }

                    unsafe { buckets.dealloc() };
                });
            }
            Err(e) => println!("Failed to establish a connection: {}", e),
        }
    }

    Ok(())
}

#[derive(Debug)]
pub enum WebServerError {
    Io(std::io::Error),
    WebSocket(ws::Error),
    RequestTooLarge(usize),
    Utf8Error,
}

impl From<std::io::Error> for WebServerError {
    fn from(err: std::io::Error) -> WebServerError {
        WebServerError::Io(err)
    }
}

impl From<ws::Error> for WebServerError {
    fn from(err: ws::Error) -> WebServerError {
        WebServerError::WebSocket(err)
    }
}

impl From<Utf8Error> for WebServerError {
    fn from(_: Utf8Error) -> WebServerError {
        WebServerError::Utf8Error
    }
}

fn handle_client(
    mut stream: TcpStream,
    receive: &mut [u8],
    send: &mut [u8],
) -> Result<(), WebServerError> {
    let mut web_socket = WebSocketServer::new_server();
    let mut num_bytes = 0;

    // read until the stream is closed (zero bytes read from the stream)
    loop {
        if num_bytes >= receive.len() {
            println!("Not a valid http request or buffer too small");
            return Err(WebServerError::RequestTooLarge(num_bytes));
        }

        num_bytes = num_bytes + stream.read(&mut receive[num_bytes..])?;
        if num_bytes == 0 {
            return Ok(());
        }

        if web_socket.state == WebSocketState::Open {
            // if the tcp stream has already been upgraded to a websocket connection
            if !web_socket_read(&mut web_socket, &mut stream, receive, send, num_bytes)? {
                println!("Websocket closed");
                return Ok(());
            }
            num_bytes = 0;
        } else {
            // assume that the client has sent us an http request. Since we may not read the
            // header all in one go we need to check for HttpHeaderIncomplete and continue reading
            if !match ws::read_http_header(&receive[..num_bytes]) {
                Ok(http_header) => {
                    num_bytes = 0;
                    respond_to_http_request(http_header, &mut web_socket, send, &mut stream)
                }
                Err(ws::Error::HttpHeaderIncomplete) => Ok(true),
                Err(e) => Err(WebServerError::WebSocket(e)),
            }? {
                return Ok(());
            }
        }
    }
}

// returns true to keep the connection open
fn respond_to_http_request(
    http_header: HttpHeader,
    web_socket: &mut WebSocketServer,
    buffer2: &mut [u8],
    stream: &mut TcpStream,
) -> Result<bool, WebServerError> {
    if let Some(websocket_context) = http_header.websocket_context {
        // this is a web socket upgrade request
        println!("Received websocket upgrade request");
        let to_send =
            web_socket.server_accept(&websocket_context.sec_websocket_key, None, buffer2)?;
        write_to_stream(stream, &buffer2[..to_send])?;
        Ok(true)
    } else {
        // this is a regular http request
        match http_header.path.as_str() {
            "/" => write_to_stream(stream, &ROOT_HTML.as_bytes())?,
            _ => {
                let html =
                    "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n";
                write_to_stream(stream, &html.as_bytes())?;
            }
        };
        Ok(false)
    }
}

fn write_to_stream(stream: &mut TcpStream, buffer: &[u8]) -> Result<(), WebServerError> {
    let mut start = 0;
    loop {
        let bytes_sent = stream.write(&buffer[start..])?;
        start += bytes_sent;

        if start == buffer.len() {
            return Ok(());
        }
    }
}

fn web_socket_read(
    web_socket: &mut WebSocketServer,
    stream: &mut TcpStream,
    tcp_buffer: &mut [u8],
    ws_buffer: &mut [u8],
    num_bytes: usize,
) -> Result<bool, WebServerError> {
    let ws_result = web_socket.read(&tcp_buffer[..num_bytes], ws_buffer)?;
    match ws_result.message_type {
        WebSocketReceiveMessageType::Text => {
            let s = std::str::from_utf8(&ws_buffer[..ws_result.len_to])?;
            println!("Received Text: {}", s);
            let to_send = web_socket.write(
                WebSocketSendMessageType::Text,
                true,
                &ws_buffer[..ws_result.len_to],
                tcp_buffer,
            )?;
            write_to_stream(stream, &tcp_buffer[..to_send])?;
            Ok(true)
        }
        WebSocketReceiveMessageType::Binary => {
            // ignored
            Ok(true)
        }
        WebSocketReceiveMessageType::CloseCompleted => Ok(false),
        WebSocketReceiveMessageType::CloseMustReply => {
            let to_send = web_socket.write(
                WebSocketSendMessageType::CloseReply,
                true,
                &ws_buffer[..ws_result.len_to],
                tcp_buffer,
            )?;
            write_to_stream(stream, &tcp_buffer[..to_send])?;
            Ok(true)
        }
        WebSocketReceiveMessageType::Ping => {
            let to_send = web_socket.write(
                WebSocketSendMessageType::Pong,
                true,
                &ws_buffer[..ws_result.len_to],
                tcp_buffer,
            )?;
            write_to_stream(stream, &tcp_buffer[..to_send])?;
            Ok(true)
        }
        WebSocketReceiveMessageType::Pong => Ok(true),
    }
}

const ROOT_HTML : &str = "HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\nContent-Length: 2590\r\nConnection: close\r\n\r\n<!doctype html><html></html>";
