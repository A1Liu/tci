#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(incomplete_features)]

#[macro_use]
mod util;

#[macro_use]
mod net_io;

#[macro_use]
mod runtime;

mod assembler;
mod ast;
mod buckets;
mod clang;
mod commands;
mod filedb;
mod interpreter;
mod lexer;
mod parser;
mod preprocessor;
mod tc_ast;
mod type_checker;

#[cfg(test)]
mod test;

use codespan_reporting::term::termcolor::{ColorChoice, StandardStream, WriteColor};
use core::mem;
use core::ops::Deref;
use embedded_websocket::{HttpHeader, WebSocketReceiveMessageType};
use filedb::FileDb;
use interpreter::Program;
use net_io::WebServerError;
use rust_embed::RustEmbed;
use std::borrow::Cow;
use util::*;

fn compile(env: &mut FileDb) -> Result<Program<'static>, Vec<Error>> {
    let mut buckets = buckets::BucketList::with_capacity(2 * env.size());
    let mut buckets_begin = buckets;
    let mut tokens = lexer::TokenDb::new();
    let mut errors: Vec<Error> = Vec::new();

    let files_list = env.vec();
    let files = files_list.iter();
    files.for_each(|&id| {
        let result = lexer::lex_file(buckets, &mut tokens, env, id);
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

    tokens = tokens
        .keys()
        .filter_map(|&file| match preprocessor::preprocess_file(&tokens, file) {
            Ok(toks) => {
                if let Some(n) = buckets.next() {
                    buckets = n;
                }

                Some((file, &*buckets.add_array(toks)))
            }
            Err(err) => {
                errors.push(err);
                None
            }
        })
        .collect();

    if errors.len() != 0 {
        return Err(errors);
    }

    let mut parser = parser::Parser::new();
    let iter = tokens.keys().filter_map(|&file| {
        while let Some(n) = buckets.next() {
            buckets = n;
        }

        match parser.parse_tokens(buckets, &tokens, file) {
            Ok(x) => return Some((x, file)),
            Err(err) => {
                errors.push(err);
                return None;
            }
        }
    });
    let asts: Vec<(ast::ASTProgram, u32)> = iter.collect();

    while let Some(n) = buckets.next() {
        buckets = n;
    }
    buckets = buckets.force_next();

    let mut assembler = assembler::Assembler::new();
    asts.into_iter().for_each(|(ast, file)| {
        while let Some(n) = buckets.next() {
            buckets = n;
        }

        let tfuncs = match type_checker::check_file(buckets, ast, file, env) {
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
        codespan_reporting::term::emit(writer, &config, files, &err.diagnostic()).unwrap();
    }
}

fn run_from_args(args: Vec<String>) -> ! {
    let args: Vec<String> = std::env::args().collect();

    let writer = StandardStream::stderr(ColorChoice::Always);

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
                .unwrap();
            }
            std::process::exit(1);
        }
    };

    mem::drop(files);

    let mut runtime = interpreter::Runtime::new(program, StringArray::new());
    let diag = runtime.run(std::io::stdout(), &mut smol::io::repeat(0));
    let code = match diag.status {
        runtime::RuntimeStatus::Exited(code) => code,
        _ => panic!(),
    };

    std::process::exit(code);
}

#[cfg(not(target_pointer_width = "64"))]
std::compile_error!("can only compile TCI on a 64 bit platform");

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        let addr = "0.0.0.0:4000";
        if let Err(err) = net_io::run_server(addr, respond_to_http_request, ws_handle) {
            debug!(err);
            std::process::exit(1);
        }
        return;
    }

    run_from_args(args);
}

#[derive(RustEmbed)]
#[folder = "frontend/build/"]
struct Asset;

fn respond_to_http_request<'a>(
    http_header: HttpHeader,
    buffer: &'a mut [u8],
) -> Result<net_io::HttpResponse<'a>, WebServerError> {
    let mut path_len = http_header.path.len();
    buffer[..path_len].copy_from_slice(http_header.path.as_bytes());
    if buffer[path_len - 1] == b'/' {
        buffer[path_len..(path_len + 10)].copy_from_slice("index.html".as_bytes());
        path_len += 10;
    }

    let (path_buf, buffer) = buffer.split_at_mut(path_len);
    let path = unsafe { core::str::from_utf8_unchecked(path_buf) };
    println!("received request for path {}", path);
    let text = Asset::get(&path[1..]);

    let text = match text {
        None => {
            const ROOT_HTML: &str = "<!doctype html><html><body><p>404 not found</p></body></html>";
            return Ok(net_io::HttpResponse {
                status: 404,
                body: ROOT_HTML.as_bytes(),
                content_type: net_io::CT_TEXT_HTML,
            });
        }
        Some(text) => text,
    };

    let (body_buf, ct_buf);
    if let Cow::Borrowed(borrowed) = text {
        body_buf = borrowed;
        ct_buf = buffer;
    } else {
        if text.len() > buffer.len() {
            return Err(WebServerError::ResponseTooLarge(text.len()));
        }

        let tup = buffer.split_at_mut(text.len());
        tup.0.copy_from_slice(&text);
        body_buf = tup.0;
        ct_buf = tup.1;
    }

    let content_type = match path {
        x if x.ends_with(".html") => "text/html",
        x if x.ends_with(".js") => "text/javascript",
        x if x.ends_with(".css") => "text/css",
        _ => {
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
            unsafe { core::str::from_utf8_unchecked(ct_buf) }
        }
    };

    return Ok(net_io::HttpResponse {
        status: 200,
        body: body_buf,
        content_type,
    });
}

async fn read_stream(
    stream: &mut net_io::WSStream,
    stdin: &mut smol::channel::Sender<String>,
    commands: &mut smol::channel::Sender<commands::Command>,
) -> Result<(), WebServerError> {
    let (message_type, bytes) = stream.read_frame().await?;
    match message_type {
        Some(WebSocketReceiveMessageType::Text) => {}
        Some(_) => return Ok(()),
        None => return Ok(()),
    }

    let mut out_buffer = Vec::new();

    let command = match serde_json::from_slice(bytes) {
        Ok(c) => c,
        Err(err) => {
            let mut string = String::new();
            string_append_utf8_lossy(&mut string, bytes);
            let error = commands::CommandResult::DeserializationError(string);
            serde_json::to_writer(&mut out_buffer, &error).unwrap();

            let out_bytes = net_io::WSResponse::text(&out_buffer[..]);
            stream.write_frames(vec![out_bytes].into_iter()).await?;
            out_buffer.clear();
            return Ok(());
        }
    };

    if let commands::Command::WriteStdin(string) = command {
        stdin.send(string).await?;
    } else {
        commands.send(command).await?;
    }

    return Ok(());
}

async fn send_result(
    stream: &mut net_io::WSStream,
    result: commands::CommandResult,
) -> Result<(), WebServerError> {
    use net_io::WSResponse as Resp;

    let mut out_buffer = Vec::new();
    serde_json::to_writer(&mut out_buffer, &result).unwrap();

    return stream.write_frame(Resp::text(&out_buffer)).await;
}

pub async fn ws_handle(mut stream: net_io::WSStream) -> Result<(), WebServerError> {
    use commands::{Command, CommandResult};
    use smol::channel::bounded;
    use smol::future::FutureExt;
    use smol::stream::StreamExt;

    async fn timeout<T>(time: u64) -> Option<T> {
        smol::Timer::after(core::time::Duration::from_millis(time)).await;
        return None;
    }

    let (mut stdin_sender, stdin_receiver) = bounded::<String>(50);
    let (mut command_sender, mut command_receiver) = bounded::<Command>(50);
    let (result_sender, mut result_receiver) = bounded::<CommandResult>(100);

    tokio::spawn(async move {
        loop {
            while let Some(value) = result_receiver.next().or(timeout(10)).await {
                if let Err(err) = send_result(&mut stream, value).await {
                    println!("{:?}", err);
                }
            }

            if let Err(err) = read_stream(&mut stream, &mut stdin_sender, &mut command_sender).await
            {
                println!("{:?}", err);
            }
        }
    });

    let mut engine = commands::CommandEngine::new();
    let transform = |s: String| Ok(s.into_bytes().into_iter());
    let mut stdin = net_io::StreamToBytes::new(stdin_receiver, transform);

    loop {
        let command = match command_receiver.next().await {
            Some(c) => c,
            None => return Ok(()),
        };

        for result in engine.run_command(command, &mut stdin).await {
            result_sender.send(result).await?;
        }
    }
}
