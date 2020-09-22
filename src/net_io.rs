use embedded_websocket as ws;
use embedded_websocket::WebSocketSendMessageType;
use std::io::Write;
use std::net::TcpStream;
use std::str::Utf8Error;

#[derive(Debug)]
pub enum WebServerError {
    Io(std::io::Error),
    WebSocket(ws::Error),
    RequestTooLarge(usize),
    PayloadTooLarge(usize),
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

pub enum WSResponse<'a> {
    Response {
        message_type: WebSocketSendMessageType,
        message: &'a [u8],
    },
    None,
    Close,
}

// macro_rules! server {
//     ($id:ident) => {
//         let listener = TcpListener::bind("127.0.0.1:3000")?;
//         println!("listening on: {}", addr);
//
//         // accept connections and process them serially
//         for stream in listener.incoming() {
//             match stream {
//                 Ok(stream) => {
//                     thread::spawn(|| match $id(stream) {
//                         Ok(()) => {}
//                         Err(e) => println!("error: {:?}", e),
//                     });
//                 }
//                 Err(e) => println!("failed to establish a connection: {}", e),
//             }
//         }
//     };
// }

// Below is licensed via the MIT License (MIT)
// Below is Copyright (c) 2019 David Haig

pub fn write_to_stream(stream: &mut TcpStream, buffer: &[u8]) -> Result<(), WebServerError> {
    let mut start = 0;
    loop {
        let bytes_sent = stream.write(&buffer[start..])?;
        start += bytes_sent;

        if start == buffer.len() {
            return Ok(());
        }
    }
}
