use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::termcolor::{ColorSpec, WriteColor};
use core::mem::MaybeUninit;
use core::{fmt, ops, slice};
use std::sync::atomic::{AtomicU8, Ordering};
use std::{io, marker};

macro_rules! error {
    ($arg1:expr) => {
        $crate::util::Error::new($arg1, vec![])
    };

    ($msg:expr, $loc1:expr, $msg1:expr) => {
        $crate::util::Error::new(
            $msg,
            vec![$crate::util::ErrorSection {
                location: $loc1,
                message: $msg1.to_string(),
            }],
        )
    };

    ($msg:expr, $loc1:expr, $msg1:expr, $loc2:expr, $msg2:expr) => {
        $crate::util::Error::new(
            $msg,
            vec![
                $crate::util::ErrorSection {
                    location: $loc1,
                    message: $msg1.to_string(),
                },
                $crate::util::ErrorSection {
                    location: $loc2,
                    message: $msg2.to_string(),
                },
            ],
        )
    };
}

pub struct LazyStatic<Obj, F: Fn() -> Obj> {
    init: AtomicU8,
    constructor: F,
    data: MaybeUninit<Obj>,
}

impl<Obj, F: Fn() -> Obj> From<F> for LazyStatic<Obj, F> {
    fn from(f: F) -> Self {
        Self::new(f)
    }
}

impl<Obj, F: Fn() -> Obj> LazyStatic<Obj, F> {
    const UNINIT: u8 = 0;
    const INIT_RUN: u8 = 1;
    const INIT: u8 = 2;
    const KILL_RUN: u8 = 3;

    pub const fn new(constructor: F) -> Self {
        Self {
            init: AtomicU8::new(0),
            constructor,
            data: MaybeUninit::uninit(),
        }
    }

    pub fn init(&self) -> bool {
        let init_ref = &self.init;
        loop {
            let state = init_ref.compare_and_swap(Self::UNINIT, Self::INIT_RUN, Ordering::SeqCst);

            match state {
                Self::INIT_RUN => {
                    while Self::INIT_RUN == init_ref.load(Ordering::SeqCst) {}
                    continue;
                }
                Self::INIT => {
                    return true;
                }
                Self::UNINIT => break,
                Self::KILL_RUN => return false,
                _ => unreachable!(),
            }
        }

        let constructor = &self.constructor;
        let data = constructor();
        unsafe { (self.data.as_ptr() as *mut Obj).write(data) };

        let state = init_ref.compare_and_swap(Self::INIT_RUN, Self::INIT, Ordering::SeqCst);
        debug_assert!(state == Self::INIT_RUN);
        return true;
    }

    pub unsafe fn kill(&self) {
        let init_ref = &self.init;
        loop {
            let state = init_ref.compare_and_swap(Self::INIT, Self::KILL_RUN, Ordering::SeqCst);

            match state {
                Self::INIT_RUN => {
                    while Self::INIT_RUN == init_ref.load(Ordering::SeqCst) {}
                    continue;
                }
                Self::INIT => break,
                Self::UNINIT | Self::KILL_RUN => return,
                _ => unreachable!(),
            }
        }

        let data = &mut *(self.data.as_ptr() as *mut Obj);

        let state = init_ref.compare_and_swap(Self::KILL_RUN, Self::UNINIT, Ordering::SeqCst);
        debug_assert!(state == Self::KILL_RUN);
    }
}

impl<Obj, F: Fn() -> Obj> ops::Deref for LazyStatic<Obj, F> {
    type Target = Obj;

    fn deref(&self) -> &Obj {
        assert!(self.init());
        return unsafe { &*self.data.as_ptr() };
    }
}

#[derive(Debug)]
pub struct ErrorSection {
    pub location: CodeLoc,
    pub message: String,
}

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub sections: Vec<ErrorSection>,
}

impl Into<Label<u32>> for &ErrorSection {
    fn into(self) -> Label<u32> {
        Label::primary(self.location.file, self.location).with_message(&self.message)
    }
}

impl Error {
    pub fn new(message: &str, sections: Vec<ErrorSection>) -> Error {
        Self {
            message: message.to_string(),
            sections,
        }
    }

    pub fn diagnostic(&self) -> Diagnostic<u32> {
        Diagnostic::error()
            .with_message(&self.message)
            .with_labels(self.sections.iter().map(|x| x.into()).collect())
    }
}

impl Into<Vec<Error>> for Error {
    fn into(self) -> Vec<Error> {
        vec![self]
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CodeLoc {
    pub start: u32, // TODO Top 20 bits for start, bottom 12 bits for length?
    pub end: u32,
    pub file: u32,
}

#[inline]
pub fn l(start: u32, end: u32, file: u32) -> CodeLoc {
    debug_assert!(start <= end);

    CodeLoc { start, end, file }
}

impl Into<ops::Range<usize>> for CodeLoc {
    fn into(self) -> ops::Range<usize> {
        (self.start as usize)..(self.end as usize)
    }
}

#[inline]
pub fn l_from(loc1: CodeLoc, loc2: CodeLoc) -> CodeLoc {
    debug_assert_eq!(loc1.file, loc2.file);
    l(loc1.start, loc2.end, loc1.file)
}

pub fn align_usize(size: usize, align: usize) -> usize {
    if size == 0 {
        return 0;
    }

    ((size - 1) / align * align) + align
}

pub fn align_u32(size: u32, align: u32) -> u32 {
    if size == 0 {
        return 0;
    }

    ((size - 1) / align * align) + align
}

// https://stackoverflow.com/questions/28127165/how-to-convert-struct-to-u8
pub unsafe fn any_as_u8_slice_mut<T: Sized + Copy>(p: &mut T) -> &mut [u8] {
    std::slice::from_raw_parts_mut(p as *mut T as *mut u8, std::mem::size_of::<T>())
}

pub fn any_as_u8_slice<T: Sized + Copy>(p: &T) -> &[u8] {
    unsafe { std::slice::from_raw_parts(p as *const T as *const u8, std::mem::size_of::<T>()) }
}

pub fn u32_to_u32_tup(value: u32) -> (u32, u32) {
    ((value >> 16) as u32, value as u32)
}

pub fn fold_binary<I, Iter: Iterator<Item = I>>(
    mut iter: Iter,
    mut reducer: impl FnMut(I, I) -> I,
) -> Option<I> {
    let first = iter.next()?;
    let second = match iter.next() {
        Some(s) => s,
        None => return Some(first),
    };

    let mut source = Vec::new();
    source.push(reducer(first, second));

    loop {
        let first = match iter.next() {
            Some(f) => f,
            None => break,
        };

        let val = match iter.next() {
            Some(e) => reducer(first, e),
            None => first,
        };

        source.push(val);
    }

    let mut target = Vec::new();
    loop {
        let mut iter = source.into_iter();

        let first = iter.next().unwrap();
        let second = match iter.next() {
            Some(s) => s,
            None => return Some(first),
        };

        target.push(reducer(first, second));

        loop {
            let first = match iter.next() {
                Some(f) => f,
                None => break,
            };

            let val = match iter.next() {
                Some(e) => reducer(first, e),
                None => first,
            };

            target.push(val);
        }

        source = target;
        target = Vec::new();
    }
}

pub struct StringWriter {
    buf: Vec<u8>,
}

impl StringWriter {
    pub fn new() -> StringWriter {
        StringWriter {
            buf: Vec::with_capacity(8 * 1024),
        }
    }

    pub fn into_string(self) -> String {
        return unsafe { String::from_utf8_unchecked(self.buf) };
    }

    pub fn to_string(&self) -> String {
        return unsafe { String::from_utf8_unchecked(self.buf.clone()) };
    }
}

impl io::Write for StringWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let map_err = |err| io::Error::new(io::ErrorKind::InvalidInput, err);
        core::str::from_utf8(buf).map_err(map_err)?;
        self.buf.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl WriteColor for StringWriter {
    fn supports_color(&self) -> bool {
        false
    }

    fn set_color(&mut self, _color: &ColorSpec) -> io::Result<()> {
        return Ok(());
    }

    fn reset(&mut self) -> io::Result<()> {
        return Ok(());
    }
}

pub struct RecordingWriter<W>
where
    W: io::Write,
{
    pub string: StringWriter,
    pub writer: W,
}

impl<W> RecordingWriter<W>
where
    W: io::Write,
{
    pub fn new(writer: W) -> Self {
        Self {
            string: StringWriter::new(),
            writer,
        }
    }
}

impl<W> io::Write for RecordingWriter<W>
where
    W: io::Write,
{
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.string.write(buf).expect("should not fail");
        self.writer.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
    }
}

impl<W> WriteColor for RecordingWriter<W>
where
    W: WriteColor,
{
    fn supports_color(&self) -> bool {
        false
    }

    fn set_color(&mut self, _color: &ColorSpec) -> io::Result<()> {
        return Ok(());
    }

    fn reset(&mut self) -> io::Result<()> {
        return Ok(());
    }
}

pub struct Void {
    unused: (),
}

impl Void {
    pub fn new() -> Self {
        return Self { unused: () };
    }
}

impl io::Write for Void {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

// https://tools.ietf.org/html/rfc3629
static UTF8_CHAR_WIDTH: [u8; 256] = [
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, // 0x1F
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, // 0x3F
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, // 0x5F
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, // 0x7F
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, // 0x9F
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, // 0xBF
    0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, // 0xDF
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // 0xEF
    4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 0xFF
];

pub struct Utf8Lossy {
    bytes: [u8],
}

impl Utf8Lossy {
    pub fn from_str(s: &str) -> &Utf8Lossy {
        Utf8Lossy::from_bytes(s.as_bytes())
    }

    pub fn from_bytes(bytes: &[u8]) -> &Utf8Lossy {
        // SAFETY: Both use the same memory layout, and UTF-8 correctness isn't required.
        unsafe { core::mem::transmute(bytes) }
    }

    pub fn chunks(&self) -> Utf8LossyChunksIter<'_> {
        Utf8LossyChunksIter {
            source: &self.bytes,
        }
    }
}

pub struct Utf8LossyChunksIter<'a> {
    source: &'a [u8],
}

pub struct Utf8LossyChunk<'a> {
    /// Sequence of valid chars.
    /// Can be empty between broken UTF-8 chars.
    pub valid: &'a str,
    /// Single broken char, empty if none.
    /// Empty iff iterator item is last.
    pub broken: &'a [u8],
}

impl<'a> Utf8LossyChunksIter<'a> {
    fn next(&mut self) -> Utf8LossyChunk<'a> {
        if self.source.is_empty() {
            return Utf8LossyChunk {
                valid: "",
                broken: self.source,
            };
        }

        const TAG_CONT_U8: u8 = 128;
        fn safe_get(xs: &[u8], i: usize) -> u8 {
            *xs.get(i).unwrap_or(&0)
        }

        let mut i = 0;
        while i < self.source.len() {
            let i_ = i;

            // SAFETY: `i` starts at `0`, is less than `self.source.len()`, and
            // only increases, so `0 <= i < self.source.len()`.
            let byte = unsafe { *self.source.get_unchecked(i) };
            i += 1;

            if byte < 128 {
            } else {
                let w = UTF8_CHAR_WIDTH[byte as usize];

                macro_rules! error {
                    () => {{
                        // SAFETY: We have checked up to `i` that source is valid UTF-8.
                        unsafe {
                            let r = Utf8LossyChunk {
                                valid: core::str::from_utf8_unchecked(&self.source[0..i_]),
                                broken: &self.source[i_..i],
                            };
                            self.source = &self.source[i..];
                            return r;
                        }
                    }};
                }

                match w {
                    2 => {
                        if safe_get(self.source, i) & 192 != TAG_CONT_U8 {
                            error!();
                        }
                        i += 1;
                    }
                    3 => {
                        match (byte, safe_get(self.source, i)) {
                            (0xE0, 0xA0..=0xBF) => (),
                            (0xE1..=0xEC, 0x80..=0xBF) => (),
                            (0xED, 0x80..=0x9F) => (),
                            (0xEE..=0xEF, 0x80..=0xBF) => (),
                            _ => {
                                error!();
                            }
                        }
                        i += 1;
                        if safe_get(self.source, i) & 192 != TAG_CONT_U8 {
                            error!();
                        }
                        i += 1;
                    }
                    4 => {
                        match (byte, safe_get(self.source, i)) {
                            (0xF0, 0x90..=0xBF) => (),
                            (0xF1..=0xF3, 0x80..=0xBF) => (),
                            (0xF4, 0x80..=0x8F) => (),
                            _ => {
                                error!();
                            }
                        }
                        i += 1;
                        if safe_get(self.source, i) & 192 != TAG_CONT_U8 {
                            error!();
                        }
                        i += 1;
                        if safe_get(self.source, i) & 192 != TAG_CONT_U8 {
                            error!();
                        }
                        i += 1;
                    }
                    _ => {
                        error!();
                    }
                }
            }
        }

        let r = Utf8LossyChunk {
            // SAFETY: We have checked that the entire source is valid UTF-8.
            valid: unsafe { core::str::from_utf8_unchecked(self.source) },
            broken: &[],
        };
        self.source = &[];
        r
    }
}

pub fn string_append_utf8_lossy(string: &mut String, bytes: &[u8]) {
    string.reserve(bytes.len());
    let mut iter = Utf8Lossy::from_bytes(bytes).chunks();

    const REPLACEMENT: &str = "\u{FFFD}";

    loop {
        let Utf8LossyChunk { valid, broken } = iter.next();
        string.push_str(valid);
        if !broken.is_empty() {
            string.push_str(REPLACEMENT);
        } else {
            return;
        }
    }
}

pub fn write_utf8_lossy(mut write: impl io::Write, bytes: &[u8]) -> io::Result<usize> {
    let mut iter = Utf8Lossy::from_bytes(bytes).chunks();

    const REPLACEMENT: &str = "\u{FFFD}";

    let mut total = 0;
    loop {
        let Utf8LossyChunk { valid, broken } = iter.next();
        write.write(valid.as_bytes())?;
        total += valid.len();
        if !broken.is_empty() {
            write.write(REPLACEMENT.as_bytes())?;
            total += REPLACEMENT.len();
        } else {
            return Ok(total);
        }
    }
}

#[derive(Clone, Copy)]
pub struct ShortSlice<'a, T> {
    pub data: *const T,
    pub len: u32,
    pub meta: u32,
    pub phantom: marker::PhantomData<&'a T>,
}

impl<'a, T> ShortSlice<'a, T> {
    pub fn new(data: &'a [T], meta: u32) -> Self {
        Self {
            data: data.as_ptr(),
            len: data.len() as u32, // TODO check for overflow
            meta,
            phantom: marker::PhantomData,
        }
    }
}

// impl<'a, T> ops::Deref for &ShortSlice<'a, T> {
//     type Target = [T];
//
//     fn deref(&self) -> &[T] {
//         return unsafe { slice::from_raw_parts(self.data, self.len as usize) };
//     }
// }

impl<'a, T> ops::Deref for ShortSlice<'a, T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        return unsafe { slice::from_raw_parts(self.data, self.len as usize) };
    }
}

impl<'a, T> fmt::Debug for ShortSlice<'a, T>
where
    T: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use ops::Deref;
        write!(fmt, "{:?}", self.deref())
    }
}
