use crate::buckets::*;
use crate::commands::{Command, CommandResult};
use core::pin::Pin;
use core::task::{Context, Poll};
use embedded_websocket as ws;
use embedded_websocket::{HttpHeader, WebSocketReceiveMessageType, WebSocketSendMessageType};
use smol::io::{AsyncRead, AsyncReadExt, AsyncWriteExt};
use smol::stream::Stream;
use std::collections::VecDeque;
use std::future::Future;
use std::str::Utf8Error;
use tokio::runtime::Runtime;

// macro_rules! write_b {
//     ($dst:expr, $($arg:tt)*) => {{
//         let mut cursor = std::io::Cursor::new(&mut ($dst)[..]);
//         match std::io::Write::write_fmt(&mut cursor, std::format_args!($($arg)*)) {
//             Ok(()) => Ok(cursor.position() as usize),
//             Err(err) => Err(err),
//         }
//     }};
// }

#[cfg(debug)]
const TCP_BUCKET_SIZE: usize = 1048576;
#[cfg(not(debug))]
const TCP_BUCKET_SIZE: usize = 1048576;

#[cfg(debug)]
const WS_BUCKET_SIZE: usize = 1048576 * 4;
#[cfg(not(debug))]
const WS_BUCKET_SIZE: usize = 1048576 * 4;

#[cfg(debug)]
const SCRATCH_BUCKET_SIZE: usize = 1048576 * 4;
#[cfg(not(debug))]
const SCRATCH_BUCKET_SIZE: usize = 1048576 * 4;

const TOTAL_BUCKET_SIZE: usize = TCP_BUCKET_SIZE + WS_BUCKET_SIZE + SCRATCH_BUCKET_SIZE;

#[derive(Debug)]
pub enum WebServerError {
    Io(std::io::Error),
    WebSocket(ws::Error),
    StdinChannel(smol::channel::SendError<String>),
    CommandChannel(smol::channel::SendError<Command>),
    ResultChannel(smol::channel::SendError<CommandResult>),
    RequestTooLarge(usize),
    PayloadTooLarge(usize),
    ResponseTooLarge(usize),
    Utf8Error,
}

impl From<smol::channel::SendError<Command>> for WebServerError {
    fn from(err: smol::channel::SendError<Command>) -> WebServerError {
        WebServerError::CommandChannel(err)
    }
}

impl From<smol::channel::SendError<CommandResult>> for WebServerError {
    fn from(err: smol::channel::SendError<CommandResult>) -> WebServerError {
        WebServerError::ResultChannel(err)
    }
}

impl From<smol::channel::SendError<String>> for WebServerError {
    fn from(err: smol::channel::SendError<String>) -> WebServerError {
        WebServerError::StdinChannel(err)
    }
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

#[derive(Debug)]
pub enum WSResponse<'a> {
    Response {
        message_type: WebSocketSendMessageType,
        message: &'a [u8],
    },
    None,
    Close,
}

impl<'a> WSResponse<'a> {
    pub fn text(message: &'a [u8]) -> Self {
        Self::Response {
            message_type: WebSocketSendMessageType::Text,
            message,
        }
    }
}

pub struct WSStream {
    pub buckets: BucketListRef<'static>,
    pub tcp_stream: smol::net::TcpStream,
    pub web_socket: ws::WebSocketServer,
    pub num_bytes: usize,
    pub tcp_bytes: usize,
    pub ws_bytes: usize,
    pub tcp_buf: &'static mut [u8],
    pub ws_buf: &'static mut [u8],
    pub out: Vec<u8>,
}

impl Drop for WSStream {
    fn drop(&mut self) {
        let mut buckets = self.buckets;
        while let Some(b) = unsafe { buckets.dealloc() } {
            buckets = b;
        }
    }
}

impl WSStream {
    pub async fn new(
        mut tcp_stream: smol::net::TcpStream,
        ws_key: &ws::WebSocketKey,
        buckets: BucketListRef<'static>,
        num_bytes: usize,
        tcp_buf: &'static mut [u8],
        ws_buf: &'static mut [u8],
    ) -> Result<WSStream, WebServerError> {
        let mut web_socket = ws::WebSocketServer::new_server();
        let to_send = web_socket.server_accept(ws_key, None, ws_buf)?;
        tcp_stream.write_all(&ws_buf[..to_send]).await?;

        Ok(WSStream {
            tcp_stream,
            web_socket,
            buckets,
            num_bytes,
            tcp_bytes: 0,
            ws_bytes: 0,
            tcp_buf,
            ws_buf,
            out: Vec::new(),
        })
    }

    pub async fn write_frame<'resp>(
        &mut self,
        resp: WSResponse<'resp>,
    ) -> Result<(), WebServerError> {
        match resp {
            WSResponse::Response {
                message_type,
                message,
            } => {
                let to_send = self
                    .web_socket
                    .write(message_type, true, message, self.ws_buf)?;
                for &byte in &self.ws_buf[0..to_send] {
                    self.out.push(byte);
                }
            }
            WSResponse::None => return Ok(()),
            WSResponse::Close => return Ok(()),
        }

        self.tcp_stream.write_all(&self.out).await?;
        self.out.clear();
        return Ok(());
    }

    pub async fn write_frames<'resp>(
        &mut self,
        responses: impl Iterator<Item = WSResponse<'resp>>,
    ) -> Result<(), WebServerError> {
        for resp in responses {
            match resp {
                WSResponse::Response {
                    message_type,
                    message,
                } => {
                    let to_send =
                        self.web_socket
                            .write(message_type, true, message, self.ws_buf)?;
                    for &byte in &self.ws_buf[0..to_send] {
                        self.out.push(byte);
                    }
                }
                WSResponse::None => break,
                WSResponse::Close => return Ok(()),
            }
        }

        self.tcp_stream.write_all(&self.out).await?;
        self.out.clear();
        return Ok(());
    }

    pub async fn read_frame(
        &mut self,
    ) -> Result<(Option<WebSocketReceiveMessageType>, &[u8]), WebServerError> {
        macro_rules! stream_read {
            () => {{
                let buf = &mut self.tcp_buf[self.num_bytes..];
                let read_bytes = self.tcp_stream.read(buf).await?;
                if read_bytes == 0 {
                    return Ok((None, &[]));
                }

                self.num_bytes += read_bytes;
            }};
        }

        if self.num_bytes == 0 {
            stream_read!();
        }

        loop {
            if self.num_bytes >= self.tcp_buf.len() {
                return Err(WebServerError::PayloadTooLarge(self.num_bytes));
            }

            let tcp_buf = &self.tcp_buf[self.tcp_bytes..self.num_bytes];
            let ws_buf = &mut self.ws_buf[self.ws_bytes..];
            let ws_result = self.web_socket.read(tcp_buf, ws_buf)?;
            self.tcp_bytes += ws_result.len_from;
            self.ws_bytes += ws_result.len_to;
            if self.ws_bytes >= ws_buf.len() {
                return Err(WebServerError::RequestTooLarge(self.ws_bytes));
            }

            if !ws_result.end_of_message {
                stream_read!();
                continue;
            }

            self.num_bytes -= self.tcp_bytes;
            for i in 0..self.num_bytes {
                self.tcp_buf[i] = self.tcp_buf[i + self.tcp_bytes];
            }
            self.tcp_bytes = 0;
            let ws_buf = &self.ws_buf[..self.ws_bytes];
            self.ws_bytes = 0;

            return Ok((Some(ws_result.message_type), ws_buf));
        }
    }
}
pub struct StreamToBytes<S, I, F, Iter>
where
    S: Stream<Item = I>,
    Iter: Iterator<Item = u8>,
    F: Fn(I) -> Result<Iter, std::io::Error>,
{
    pub bytes: VecDeque<u8>,
    pub stream: S,
    pub transform: F,
}

impl<S, I, F, Iter> StreamToBytes<S, I, F, Iter>
where
    S: Stream<Item = I>,
    Iter: Iterator<Item = u8>,
    F: Fn(I) -> Result<Iter, std::io::Error>,
{
    pub fn new(stream: S, transform: F) -> Self {
        Self {
            bytes: VecDeque::new(),
            stream,
            transform,
        }
    }
}

impl<S, I, F, Iter> AsyncRead for StreamToBytes<S, I, F, Iter>
where
    S: Stream<Item = I>,
    Iter: Iterator<Item = u8>,
    F: Fn(I) -> Result<Iter, std::io::Error>,
{
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut [u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        let mut counter = 0;
        let sel = unsafe { self.get_unchecked_mut() };
        while counter < buf.len() {
            if let Some(byte) = sel.bytes.pop_front() {
                buf[counter] = byte;
                counter += 1;
            } else {
                break;
            }
        }

        if counter > 0 {
            return Poll::Ready(Ok(counter));
        }

        let poll = unsafe { Pin::new_unchecked(&mut sel.stream) }.poll_next(cx);
        let value = match poll {
            Poll::Pending => return Poll::Pending,
            Poll::Ready(None) => return Poll::Ready(Ok(counter)),
            Poll::Ready(Some(value)) => value,
        };

        let transform = &sel.transform;
        let iter = match transform(value) {
            Ok(iter) => iter,
            Err(err) => return Poll::Ready(Err(err)),
        };

        for byte in iter {
            if counter == buf.len() {
                sel.bytes.push_back(byte);
                continue;
            }

            buf[counter] = byte;
            counter += 1;
        }

        return Poll::Ready(Ok(counter));
    }
}

pub struct HttpResponse<'a> {
    pub status: u16,
    pub content_type: &'a str,
    pub body: &'a [u8],
}

pub const CT_TEXT_HTML: &'static str = "text; html; charset=UTF-8";

pub type HttpHandler =
    for<'a> fn(HttpHeader, &'a mut [u8]) -> Result<HttpResponse<'a>, WebServerError>;

pub fn run_server<F, Out>(
    addr: &str,
    http_handler: HttpHandler,
    ws_handler: F,
) -> Result<(), WebServerError>
where
    Out: Future<Output = Result<(), WebServerError>> + Send + 'static,
    F: Fn(WSStream) -> Out + Copy + Send + 'static,
{
    let runtime = Runtime::new()?;
    smol::block_on(async {
        let listener = smol::net::TcpListener::bind(addr).await?;
        println!("listening on: {}", addr);

        loop {
            let (stream, _) = listener.accept().await?;
            runtime.spawn(async move {
                if let Err(e) = handle_connection(stream, http_handler, ws_handler).await {
                    debug!(e);
                }
            });
        }
    })
}

pub async fn handle_connection<F, Out>(
    mut stream: smol::net::TcpStream,
    http_handler: HttpHandler,
    ws_handler: F,
) -> Result<(), WebServerError>
where
    Out: Future<Output = Result<(), WebServerError>>,
    F: Fn(WSStream) -> Out,
{
    let buckets = BucketList::with_capacity(TOTAL_BUCKET_SIZE);

    let tcp_buf = buckets.uninit(TCP_BUCKET_SIZE).unwrap();
    let scratch_buf = buckets.uninit(SCRATCH_BUCKET_SIZE).unwrap();

    let mut num_bytes = 0;
    macro_rules! stream_read {
        () => {{
            let read_bytes = stream.read(&mut tcp_buf[num_bytes..]).await?;
            if read_bytes == 0 {
                return Ok(());
            }

            num_bytes += read_bytes;
        }};
    }

    loop {
        let mut headers = [ws::httparse::EMPTY_HEADER; 32];

        if num_bytes >= tcp_buf.len() {
            return Err(WebServerError::RequestTooLarge(num_bytes));
        }

        stream_read!();

        // assume that the client has sent us an http request. Since we may not read the
        // header all in one go we need to check for HttpHeaderIncomplete and continue reading
        let http_header = match ws::read_http_header(&mut headers, &tcp_buf[..num_bytes]) {
            Ok(header) => header,
            Err(ws::Error::HttpHeaderIncomplete) => continue,
            Err(e) => return Err(WebServerError::WebSocket(e)),
        };

        if let Some(ws_context) = &http_header.websocket_context {
            let bytes_read = http_header.bytes_read;
            num_bytes -= bytes_read;
            for i in 0..num_bytes {
                tcp_buf[i] = tcp_buf[i + bytes_read];
            }

            let ws_stream = WSStream::new(
                stream,
                &ws_context.sec_websocket_key,
                buckets,
                num_bytes,
                tcp_buf,
                scratch_buf,
            )
            .await?;

            let future = ws_handler(ws_stream);
            let result = future.await;
            return result;
        } else {
            let resp = http_handler(http_header, scratch_buf)?;
            let bytes = serialize_http_response(resp, tcp_buf)?;
            stream.write_all(bytes).await?;
            return Ok(());
        }
    }
}

pub fn serialize_http_response<'a>(
    resp: HttpResponse,
    bytes: &'a mut [u8],
) -> Result<&'a [u8], WebServerError> {
    let mut num_bytes = 0;
    macro_rules! write_to_bytes {
        ($str:expr) => {
            let string = $str.as_bytes();

            for byte in string {
                let map_err = || WebServerError::ResponseTooLarge(num_bytes);
                let slot = bytes.get_mut(num_bytes).ok_or_else(map_err)?;
                *slot = *byte;
                num_bytes += 1;
            }
        };
    };

    write_to_bytes!("HTTP/1.1 ");
    num_bytes += itoa::write(&mut bytes[num_bytes..], resp.status)?;
    write_to_bytes!(" Ok\r\nContent-Length: ");
    num_bytes += itoa::write(&mut bytes[num_bytes..], resp.body.len())?;
    write_to_bytes!("\r\nContent-Type: ");
    write_to_bytes!(resp.content_type);
    write_to_bytes!("\r\nConnection: close\r\n\r\n");

    if resp.body.len() + num_bytes > bytes.len() {
        return Err(WebServerError::ResponseTooLarge(num_bytes));
    }

    bytes[num_bytes..(resp.body.len() + num_bytes)].copy_from_slice(resp.body);
    num_bytes += resp.body.len();

    return Ok(&bytes[..num_bytes]);
}
