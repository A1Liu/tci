use crate::buckets::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::termcolor::{ColorSpec, WriteColor};
use core::borrow::Borrow;
use core::{fmt, marker, mem, ops, ptr, slice, str};
use serde::ser::{Serialize, SerializeMap, SerializeStruct, Serializer};
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::hash::{BuildHasher, Hash, Hasher};
use std::io;
use std::sync::atomic::AtomicUsize;

pub use lazy_static::lazy_static;
pub use std::collections::hash_map::Entry;
pub use std::collections::HashMap;
pub use std::io::Write;

#[allow(unused_macros)]
macro_rules! panic {
    ( $( $arg:tt )* ) => {{
        debug!( $( $arg )* );
        std::panic!();
    }};
}

#[allow(unused_macros)]
macro_rules! unimplemented {
    ( $( $arg:tt )* ) => {{
        debug!( $( $arg )* );
        std::panic!();
    }};
}

#[allow(unused_macros)]
macro_rules! unreachable {
    ( $( $arg:tt )* ) => {{
        debug!( $( $arg )* );
        std::panic!();
    }};
}

#[allow(unused_macros)]
macro_rules! println {
    ( $( $arg:tt )* ) => {{
        debug!( $( $arg )* );
    }};
}

#[allow(unused_macros)]
macro_rules! debug {
    ($fmt:literal) => {{
         out!($fmt);
    }};
    ($fmt:literal, $( $e:expr ),+ ) => {{
         out!(@DEBUG, $fmt, $( $e ),+ );
    }};
    ($expr:expr) => {{
         out!(@DEBUG, "{} = {:?}", stringify!($expr), $expr);
    }};
    () => {{
        out!("Nothing to see here");
    }};
}

thread_local! {
    pub static OUTPUT: RefCell<Option<Box<dyn Fn(String)>>>= RefCell::new(None);
}

pub static COUNTER: AtomicUsize = AtomicUsize::new(0);
pub const LIMIT: usize = (-1isize) as usize;

#[allow(unused_macros)]
macro_rules! out {
    ($str:literal) => {{
        out!(@DEBUG, "{}", $str);
    }};
    (@DEBUG, $str:expr, $( $e:expr ),+ ) => {{
        if cfg!(debug_assertions) {
            out!(@LIMITED, std::concat!("DEBUG ({}:{}): ", $str, "\n"), file!(), line!(), $( $e ),+ );
        }
    }};
    (@LOG, $str:expr, $( $e:expr ),+ ) => {{
        out!(@CLEAN, std::concat!("LOG ({}:{}): ", $str, "\n"), file!(), line!(), $( $e ),+ );
    }};
    (@LIMITED, $str:expr, $( $e:expr ),+ ) => {{
        let count = $crate::COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        if count > $crate::LIMIT {
            out!(@CLEAN, "{}", "debug statement limit reached");
            std::panic!();
        }

        out!(@CLEAN, std::concat!("{} ", $str), count, $( $e ),+ );
    }};
    (@CLEAN, $str:expr, $( $e:expr ),+ ) => {{
        let s = std::format!( $str, $( $e ),+ );

        $crate::OUTPUT.with(|out| {
            let borrow = out.borrow();
            if let Some(func) = &*borrow {
                func(s);
            } else {
                std::print!("{}", s);
            }
        });
    }};
}

pub fn register_output(f: impl Fn(String) + 'static) {
    OUTPUT.with(|out| out.borrow_mut().replace(Box::new(f)));
}

macro_rules! error {
    ($arg1:expr) => {{
        let mut s = $arg1.to_string();
        if cfg!(debug_assertions) {
            s += &format!(" (in compiler at {}:{})", file!(), line!());
        }

        $crate::util::Error::new(s, vec![])
    }};

    ($msg:expr, $loc1:expr, $msg1:expr) => {{
        let mut s = $msg.to_string();
        if cfg!(debug_assertions) {
            s += &format!(" (in compiler at {}:{})", file!(), line!());
        }

        $crate::util::Error::new(
            s,
            vec![$crate::util::ErrorSection {
                location: $loc1,
                message: $msg1.to_string(),
            }],
        )
    }};

    ($msg:expr, $loc1:expr, $msg1:expr, $loc2:expr, $msg2:expr) => {{
        let mut s = $msg.to_string();
        if cfg!(debug_assertions) {
            s += &format!(" (in compiler at {}:{})", file!(), line!());
        }

        $crate::util::Error::new(
            s,
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
    }};
}

#[derive(Debug, serde::Serialize)]
pub struct ErrorSection {
    pub location: CodeLoc,
    pub message: String,
}

#[derive(Debug, serde::Serialize)]
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
    pub fn new(message: String, sections: Vec<ErrorSection>) -> Error {
        Self {
            message: message,
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

pub const NO_FILE: CodeLoc = CodeLoc {
    start: 0,
    end: 0,
    file: !0,
};

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct CodeLoc {
    pub start: u32, // TODO Top 20 bits for start, bottom 12 bits for length?
    pub end: u32,
    pub file: u32,
}

impl Serialize for CodeLoc {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if self == &NO_FILE {
            return serializer.serialize_none();
        }

        let mut state = serializer.serialize_struct("CodeLoc", 3)?;
        state.serialize_field("start", &self.start)?;
        state.serialize_field("end", &self.end)?;
        state.serialize_field("file", &self.file)?;
        return state.end();
    }
}

impl fmt::Debug for CodeLoc {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{}:({},{})", self.file, self.start, self.end)
    }
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
    if loc1 == NO_FILE {
        return loc2;
    }

    if loc2 == NO_FILE {
        return loc1;
    }

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

    let result = ((size - 1) / align * align) + align;
    return result;
}

pub fn align_u64(size: u64, align: u64) -> u64 {
    if size == 0 {
        return 0;
    }

    ((size - 1) / align * align) + align
}

// https://stackoverflow.com/questions/28127165/how-to-convert-struct-to-u8
pub unsafe fn any_as_u8_slice_mut<T: Sized + Copy>(p: &mut T) -> &mut [u8] {
    std::slice::from_raw_parts_mut(p as *mut T as *mut u8, mem::size_of::<T>())
}

pub fn any_as_u8_slice<T: Sized>(p: &T) -> &[u8] {
    unsafe { std::slice::from_raw_parts(p as *const T as *const u8, mem::size_of::<T>()) }
}

pub fn u8_slice_as_any<T: Sized>(p: &[u8]) -> &T {
    assert_eq!(mem::size_of::<T>(), p.len());
    unsafe { &*(p.as_ptr() as *const T) }
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

pub struct Cursor<IO: io::Write> {
    pub io: IO,
    pub len: usize,
}

impl<IO: io::Write> Cursor<IO> {
    pub fn new(io: IO) -> Self {
        Self { io, len: 0 }
    }
}

impl<IO: io::Write> io::Write for Cursor<IO> {
    fn write(&mut self, buf: &[u8]) -> Result<usize, io::Error> {
        let len = self.io.write(buf)?;
        self.len += len;
        return Ok(len);
    }

    fn flush(&mut self) -> Result<(), io::Error> {
        return self.io.flush();
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

    pub fn flush_string(&mut self) -> String {
        let ret_val = unsafe { String::from_utf8_unchecked(self.buf.clone()) };
        self.buf.clear();
        return ret_val;
    }
}

impl io::Write for StringWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let map_err = |err| io::Error::new(io::ErrorKind::InvalidInput, err);
        str::from_utf8(buf).map_err(map_err)?;
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

#[repr(C)]
#[derive(Debug)]
pub struct TE<T, E>(pub T, pub [E]);

pub struct TaggedMultiArray<T, E> {
    elements: Vec<u8>,
    tags: Vec<(usize, usize)>,
    phantom: marker::PhantomData<(T, E)>,
}

pub struct TMAIter<'a, T, E> {
    tma: &'a TaggedMultiArray<T, E>,
    idx: usize,
}

pub struct TMAIterMut<'a, T, E> {
    tma: &'a mut TaggedMultiArray<T, E>,
    idx: usize,
}

impl<T, E> TaggedMultiArray<T, E> {
    pub fn new() -> Self {
        Self {
            elements: Vec::new(),
            tags: Vec::new(),
            phantom: marker::PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        return self.tags.len();
    }

    pub fn push(&mut self, tag: T, data: Vec<E>) {
        let begin = align_usize(self.elements.len(), mem::align_of::<T>());
        let len = data.len();
        self.elements.resize(begin, 0);
        self.elements.extend_from_slice(any_as_u8_slice(&tag));
        mem::forget(tag);
        let elem_begin = align_usize(self.elements.len(), mem::align_of::<E>());
        self.elements.resize(elem_begin, 0);

        for elem in data {
            self.elements.extend_from_slice(any_as_u8_slice(&elem));
            mem::forget(elem);
        }

        self.tags.push((begin, len));
    }

    pub fn push_from(&mut self, tag: T, data: &[E])
    where
        E: Clone,
    {
        let begin = align_usize(self.elements.len(), mem::align_of::<T>());
        let len = data.len();
        self.elements.resize(begin, 0);
        self.elements.extend_from_slice(any_as_u8_slice(&tag));
        mem::forget(tag);
        let elem_begin = align_usize(self.elements.len(), mem::align_of::<E>());
        self.elements.resize(elem_begin, 0);

        for elem in data {
            let elem = elem.clone();
            self.elements.extend_from_slice(any_as_u8_slice(&elem));
            mem::forget(elem);
        }

        self.tags.push((begin, len));
    }

    pub fn pop(&mut self) -> Option<T> {
        let (idx, len) = self.tags.pop()?;

        let ptr = &mut self.elements[idx] as *mut u8 as *mut T;
        let TE(tag, elems) = unsafe {
            let fat_ptr = slice::from_raw_parts_mut(ptr, len);
            mem::transmute::<&mut [T], &mut TE<T, E>>(fat_ptr)
        };

        let tag = unsafe { ptr::read(tag) };
        for e in elems.iter_mut() {
            unsafe { ptr::drop_in_place(e) };
        }

        self.elements.resize(idx, 0);
        return Some(tag);
    }

    pub fn last_mut(&mut self) -> Option<&mut TE<T, E>> {
        let (idx, len) = *self.tags.last()?;
        let ptr = &mut self.elements[idx] as *mut u8 as *mut T;
        unsafe {
            let fat_ptr = slice::from_raw_parts_mut(ptr, len);
            return Some(mem::transmute::<&mut [T], &mut TE<T, E>>(fat_ptr));
        }
    }

    pub fn get_mut(&mut self, idx: usize) -> Option<&mut TE<T, E>> {
        let (idx, len) = *self.tags.get(idx)?;
        let ptr = &mut self.elements[idx] as *mut u8 as *mut T;
        unsafe {
            let fat_ptr = slice::from_raw_parts_mut(ptr, len);
            return Some(mem::transmute::<&mut [T], &mut TE<T, E>>(fat_ptr));
        }
    }

    pub fn get(&self, idx: usize) -> Option<&TE<T, E>> {
        let (idx, len) = *self.tags.get(idx)?;
        let ptr = &self.elements[idx] as *const u8 as *const T;
        unsafe {
            let fat_ptr = slice::from_raw_parts(ptr, len);
            return Some(mem::transmute::<&[T], &TE<T, E>>(fat_ptr));
        }
    }
}
impl<T, E> fmt::Debug for TaggedMultiArray<T, E>
where
    T: fmt::Debug,
    E: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.debug_list().entries(self.into_iter()).finish()
    }
}

impl<T, E> ops::Index<u32> for TaggedMultiArray<T, E> {
    type Output = TE<T, E>;

    fn index(&self, idx: u32) -> &TE<T, E> {
        return self.get(idx as usize).unwrap();
    }
}

impl<T, E> ops::IndexMut<u32> for TaggedMultiArray<T, E> {
    fn index_mut(&mut self, idx: u32) -> &mut TE<T, E> {
        return self.get_mut(idx as usize).unwrap();
    }
}

impl<T, E> ops::Index<usize> for TaggedMultiArray<T, E> {
    type Output = TE<T, E>;

    fn index(&self, idx: usize) -> &TE<T, E> {
        return self.get(idx).unwrap();
    }
}

impl<T, E> ops::IndexMut<usize> for TaggedMultiArray<T, E> {
    fn index_mut(&mut self, idx: usize) -> &mut TE<T, E> {
        return self.get_mut(idx).unwrap();
    }
}

impl<'a, T, E> Iterator for TMAIterMut<'a, T, E> {
    type Item = &'a mut TE<T, E>;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.tma.get_mut(self.idx)? as *mut TE<T, E>;
        self.idx += 1;
        return Some(unsafe { &mut *val });
    }
}

impl<'a, T, E> Iterator for TMAIter<'a, T, E> {
    type Item = &'a TE<T, E>;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.tma.get(self.idx)?;
        self.idx += 1;
        return Some(val);
    }
}

impl<'a, T, E> IntoIterator for &'a TaggedMultiArray<T, E> {
    type Item = &'a TE<T, E>;
    type IntoIter = TMAIter<'a, T, E>;

    fn into_iter(self) -> Self::IntoIter {
        return TMAIter { tma: self, idx: 0 };
    }
}

impl<'a, T, E> IntoIterator for &'a mut TaggedMultiArray<T, E> {
    type Item = &'a mut TE<T, E>;
    type IntoIter = TMAIterMut<'a, T, E>;

    fn into_iter(self) -> Self::IntoIter {
        return TMAIterMut { tma: self, idx: 0 };
    }
}

impl<T, E> Drop for TaggedMultiArray<T, E> {
    fn drop(&mut self) {
        while let Some(_) = self.pop() {}
    }
}

#[repr(C)]
pub struct TS<T>(pub T, pub str);

#[derive(Debug)]
pub struct StringArray<T> {
    pub data: TaggedMultiArray<T, u8>,
}

pub struct SAIter<'a, T> {
    sa: &'a StringArray<T>,
    idx: usize,
}

impl<T> StringArray<T> {
    pub fn new() -> Self {
        Self {
            data: TaggedMultiArray::new(),
        }
    }

    pub fn len(&self) -> usize {
        return self.data.len();
    }

    pub fn push(&mut self, tag: T, data: &str) {
        self.data.push_from(tag, data.as_bytes());
    }

    pub fn get(&self, idx: usize) -> Option<&TS<T>> {
        let ts = self.data.get(idx)?;
        return Some(unsafe { mem::transmute(ts) });
    }
}

impl<T> ops::Index<usize> for StringArray<T> {
    type Output = TS<T>;

    fn index(&self, idx: usize) -> &TS<T> {
        return self.get(idx).unwrap();
    }
}

impl<'a, T> Iterator for SAIter<'a, T> {
    type Item = &'a TS<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.sa.get(self.idx)?;
        self.idx += 1;
        return Some(val);
    }
}

impl<'a, T> IntoIterator for &'a StringArray<T> {
    type Item = &'a TS<T>;
    type IntoIter = SAIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        return SAIter { sa: self, idx: 0 };
    }
}

#[derive(Clone, Copy)]
pub struct DetState;

impl BuildHasher for DetState {
    type Hasher = DefaultHasher;
    #[inline]
    fn build_hasher(&self) -> DefaultHasher {
        return DefaultHasher::new();
    }
}

#[derive(Clone, Copy)]
pub enum HashRefSlot<Key, Value>
where
    Key: Copy,
    Value: Copy,
{
    Some(Key, Value),
    None,
}

#[derive(Clone, Copy)]
pub struct HashRef<'a, Key, Value, State = DetState>
where
    Key: Eq + Hash + Copy,
    Value: Copy,
    State: BuildHasher,
{
    pub slots: &'a [HashRefSlot<Key, Value>],
    pub size: usize,
    pub state: State,
}

impl<'a, K, V> HashRef<'a, K, V, DetState>
where
    K: Eq + Hash + Copy,
    V: Copy,
{
    pub fn new(frame: impl Allocator<'a>, data: &HashMap<K, V>) -> Self {
        return Self::with_state(frame, data, DetState);
    }

    pub fn new_iter<I>(frame: impl Allocator<'a>, capa: usize, data: I) -> Self
    where
        I: Iterator<Item = (K, V)>,
    {
        return Self::with_state_iter(frame, capa, data, DetState);
    }

    pub fn empty() -> Self {
        Self {
            slots: &[],
            size: 0,
            state: DetState,
        }
    }
}

impl<'a, K, V, State> HashRef<'a, K, V, State>
where
    K: Eq + Hash + Copy,
    V: Copy,
    State: BuildHasher,
{
    pub fn with_state(frame: impl Allocator<'a>, data: &HashMap<K, V>, state: State) -> Self {
        let capa = data.len() * 3 / 2;
        return Self::with_state_iter(frame, capa, data.iter().map(|(&k, &v)| (k, v)), state);
    }

    pub fn with_state_iter<I>(frame: impl Allocator<'a>, capa: usize, data: I, state: State) -> Self
    where
        I: Iterator<Item = (K, V)>,
    {
        let slots = frame.build_array(capa, |_idx| HashRefSlot::None).unwrap();
        let mut size = 0;

        for (key, value) in data {
            if size == capa {
                panic!(
                    "allocated too little capacity for size (size = capacity = {})",
                    size
                );
            }

            let mut hasher = state.build_hasher();
            key.hash(&mut hasher);
            let mut slot_idx = hasher.finish() as usize % slots.len();

            loop {
                match &mut slots[slot_idx] {
                    HashRefSlot::Some(slot_key, slot_value) => {
                        if slot_key == &key {
                            *slot_key = key;
                            *slot_value = value;
                            break;
                        }
                    }
                    slot @ HashRefSlot::None => {
                        *slot = HashRefSlot::Some(key, value);
                        size += 1;
                        break;
                    }
                }

                slot_idx += 1;
                slot_idx = slot_idx % slots.len();
            }
        }

        Self { slots, size, state }
    }

    #[inline]
    pub fn len(&self) -> usize {
        return self.size;
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        return self.slots.len();
    }

    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        let mut hasher = self.state.build_hasher();
        key.hash(&mut hasher);
        let mut slot_idx = hasher.finish() as usize % self.slots.len();
        let original_slot_idx = slot_idx;
        match &self.slots[slot_idx] {
            HashRefSlot::Some(slot_key, slot_value) => {
                if slot_key.borrow() == key {
                    return Some(slot_value);
                }
            }
            HashRefSlot::None => return None,
        }

        loop {
            slot_idx += 1;
            slot_idx = slot_idx % self.slots.len();

            if slot_idx == original_slot_idx {
                return None;
            }

            match &self.slots[slot_idx] {
                HashRefSlot::Some(slot_key, slot_value) => {
                    if slot_key.borrow() == key {
                        return Some(slot_value);
                    }
                }
                HashRefSlot::None => return None,
            }
        }
    }
}

pub struct HashRefIter<'a, Key, Value>
where
    Key: Copy,
    Value: Copy,
{
    pub slots: &'a [HashRefSlot<Key, Value>],
    pub slot_idx: usize,
}

impl<'a, Key, Value> Iterator for HashRefIter<'a, Key, Value>
where
    Key: Eq + Hash + Copy,
    Value: Copy,
{
    type Item = (&'a Key, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.slot_idx == self.slots.len() {
                return None;
            } else if let HashRefSlot::Some(key, value) = &self.slots[self.slot_idx] {
                self.slot_idx += 1;
                return Some((key, value));
            }

            self.slot_idx += 1;
        }
    }
}

impl<'a, K, V, State> IntoIterator for &HashRef<'a, K, V, State>
where
    K: Eq + Hash + Copy,
    V: Copy,
    State: BuildHasher,
{
    type Item = (&'a K, &'a V);
    type IntoIter = HashRefIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        HashRefIter {
            slots: self.slots,
            slot_idx: 0,
        }
    }
}

impl<'a, K, V, State> IntoIterator for HashRef<'a, K, V, State>
where
    K: Eq + Hash + Copy,
    V: Copy,
    State: BuildHasher,
{
    type Item = (&'a K, &'a V);
    type IntoIter = HashRefIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        HashRefIter {
            slots: self.slots,
            slot_idx: 0,
        }
    }
}

impl<'a, Key, Value, State> fmt::Debug for HashRef<'a, Key, Value, State>
where
    Key: Eq + Hash + Copy + fmt::Debug,
    Value: fmt::Debug + Copy,
    State: BuildHasher,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.debug_map().entries(self.into_iter()).finish()
    }
}

impl<'a, Key, Value, State> Serialize for HashRef<'a, Key, Value, State>
where
    Key: Eq + Hash + Copy + Serialize,
    Value: Copy + Serialize,
    State: BuildHasher,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.len()))?;
        for (key, value) in self {
            map.serialize_entry(key, value)?;
        }
        map.end()
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct n32 {
    pub data: u32,
}

impl n32 {
    pub const NULL: n32 = n32 { data: !0 };

    pub fn new(data: u32) -> Self {
        if data == Self::NULL.data {
            panic!("NullPointerException");
        }

        Self { data }
    }

    pub fn opt(self) -> Option<u32> {
        if self.data == Self::NULL.data {
            return None;
        }

        return Some(self.data);
    }

    pub fn ok_or_else<T, F>(self, f: F) -> Result<u32, T>
    where
        F: FnOnce() -> T,
    {
        if self == Self::NULL {
            return Err(f());
        }

        return Ok(self.data);
    }

    pub fn unwrap(self) -> u32 {
        if self == Self::NULL {
            panic!("NullPointerException");
        }

        return self.data;
    }

    pub fn unwrap_or(self, f: u32) -> u32 {
        if self == Self::NULL {
            return f;
        }

        return self.data;
    }

    pub fn unwrap_or_else<F>(self, f: F) -> u32
    where
        F: FnOnce() -> u32,
    {
        if self == Self::NULL {
            return f();
        }

        return self.data;
    }
}

impl Into<u32> for n32 {
    fn into(self) -> u32 {
        if self == Self::NULL {
            panic!("NullPointerException");
        }

        return self.data;
    }
}

impl Into<u32> for &n32 {
    fn into(self) -> u32 {
        if self == &n32::NULL {
            panic!("NullPointerException");
        }

        return self.data;
    }
}

impl Into<usize> for n32 {
    fn into(self) -> usize {
        if self == Self::NULL {
            panic!("NullPointerException");
        }

        return self.data as usize;
    }
}

impl Into<usize> for &n32 {
    fn into(self) -> usize {
        if self == &n32::NULL {
            panic!("NullPointerException");
        }

        return self.data as usize;
    }
}

impl From<u32> for n32 {
    fn from(data: u32) -> Self {
        Self::new(data)
    }
}

impl From<usize> for n32 {
    fn from(data: usize) -> Self {
        Self::new(data as u32)
    }
}

impl ops::Add<u32> for n32 {
    type Output = n32;

    fn add(mut self, rhs: u32) -> n32 {
        self.data += rhs;
        if self == Self::NULL {
            panic!("NullPointerException");
        }
        return self;
    }
}

impl fmt::Debug for n32 {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if *self == Self::NULL {
            write!(fmt, "null")
        } else {
            write!(fmt, "{}", self.data)
        }
    }
}

impl Serialize for n32 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if *self == Self::NULL {
            serializer.serialize_none()
        } else {
            serializer.serialize_u32(self.data)
        }
    }
}

pub struct Defer<F: FnOnce()> {
    f: Option<F>,
}

impl<F: FnOnce()> Drop for Defer<F> {
    fn drop(&mut self) {
        let f = self.f.take().unwrap();
        f();
    }
}

pub fn defer<F: FnOnce()>(f: F) -> Defer<F> {
    return Defer { f: Some(f) };
}

macro_rules! let_expr {
    ($left:pat = $right:expr) => {{
        if let $left = $right {
            true
        } else {
            false
        }
    }};
}

pub struct StackLL<'a, E> {
    pub parent: Option<&'a StackLL<'a, E>>,
    pub item: E,
}

impl<'a, E> StackLL<'a, E> {
    pub fn new(item: E) -> Self {
        Self { parent: None, item }
    }

    pub fn get(&self) -> &E {
        &self.item
    }

    pub fn child<'b>(&'b self, item: E) -> StackLL<'b, E>
    where
        'a: 'b,
    {
        StackLL {
            parent: Some(self),
            item,
        }
    }
}

pub struct StackLLIter<'a, 'b, E>
where
    'b: 'a,
{
    pub ll: Option<&'a StackLL<'b, E>>,
}

impl<'a, 'b, E> Iterator for StackLLIter<'a, 'b, E> {
    type Item = &'a E;
    fn next(&mut self) -> Option<&'a E> {
        let ll = self.ll?;
        let item = &ll.item;
        self.ll = ll.parent;
        return Some(item);
    }
}

impl<'a, 'b, E> IntoIterator for &'a StackLL<'b, E> {
    type Item = &'a E;
    type IntoIter = StackLLIter<'a, 'b, E>;

    fn into_iter(self) -> Self::IntoIter {
        StackLLIter { ll: Some(self) }
    }
}

pub struct VecU8 {
    pub data: Vec<u8>,
}

impl VecU8 {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn push<T: Copy + 'static>(&mut self, t: T) {
        let from_bytes = any_as_u8_slice(&t);
        self.data.extend_from_slice(from_bytes);
    }

    pub fn push_aligned<T: Copy + 'static>(&mut self, t: T) {
        self.align(mem::align_of::<T>());
        let from_bytes = any_as_u8_slice(&t);
        self.data.extend_from_slice(from_bytes);
    }

    pub fn append(&mut self, data: &mut Vec<u8>) {
        self.data.append(data);
    }

    pub fn align(&mut self, align: usize) {
        let aligned = align_usize(self.data.len(), align);
        self.data.resize(aligned, 0);
    }
}

pub struct TMVec<'a, T, E> {
    tmv: &'a mut TaggedMultiVec<T, E>,
    idx: usize,
}

#[derive(Clone, Copy)]
pub struct TMVBlock {
    tag_idx: usize,
    elem_idx: usize, // TODO technically computable from tag_idx, but I'm too lazy
    elem_len: usize,
    elem_capa: usize,
}

pub struct TMVIter<'a, T, E> {
    tmv: &'a TaggedMultiVec<T, E>,
    idx: usize,
}

pub struct TaggedMultiVec<T, E> {
    elements: Vec<u8>,
    tags: Vec<TMVBlock>,
    phantom: marker::PhantomData<(T, E)>,
}

impl<T, E> TaggedMultiVec<T, E> {
    pub fn new() -> Self {
        Self {
            elements: Vec::new(),
            tags: Vec::new(),
            phantom: marker::PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        return self.tags.len();
    }

    pub fn push(&mut self, tag: T, data: Vec<E>) {
        let begin = align_usize(self.elements.len(), mem::align_of::<T>());
        let (len, capa) = (data.len(), data.capacity());
        self.elements.resize(begin, 0);
        self.elements.extend_from_slice(any_as_u8_slice(&tag));
        mem::forget(tag);
        let elem_begin = align_usize(self.elements.len(), mem::align_of::<E>());
        self.elements.resize(elem_begin, 0);

        self.elements.reserve(mem::size_of::<E>() * capa);
        let elem_idx = self.elements.len();

        for elem in data {
            self.elements.extend_from_slice(any_as_u8_slice(&elem));
            mem::forget(elem);
        }

        unsafe { self.elements.set_len(self.elements.len() + capa - len) };

        let block = TMVBlock {
            tag_idx: begin,
            elem_idx,
            elem_len: len,
            elem_capa: capa,
        };

        self.tags.push(block);
    }

    pub fn push_from(&mut self, tag: T, data: &[E])
    where
        E: Clone,
    {
        let begin = align_usize(self.elements.len(), mem::align_of::<T>());
        let (len, capa) = (data.len(), data.len() * 3 / 2);
        self.elements.resize(begin, 0);
        self.elements.extend_from_slice(any_as_u8_slice(&tag));
        mem::forget(tag);
        let elem_begin = align_usize(self.elements.len(), mem::align_of::<E>());
        self.elements.resize(elem_begin, 0);

        self.elements.reserve(mem::size_of::<E>() * capa);
        let elem_idx = self.elements.len();
        for elem in data {
            let elem = elem.clone();
            self.elements.extend_from_slice(any_as_u8_slice(&elem));
            mem::forget(elem);
        }

        unsafe { self.elements.set_len(self.elements.len() + capa - len) };

        let block = TMVBlock {
            tag_idx: begin,
            elem_idx,
            elem_len: len,
            elem_capa: capa,
        };

        self.tags.push(block);
    }

    pub fn pop(&mut self) -> Option<T> {
        let block = self.tags.pop()?;

        let ptr = &mut self.elements[block.tag_idx] as *mut u8 as *mut T;
        let TE(tag, elems) = unsafe {
            let fat_ptr = slice::from_raw_parts_mut(ptr, block.elem_len);
            mem::transmute::<&mut [T], &mut TE<T, E>>(fat_ptr)
        };

        let tag = unsafe { ptr::read(tag) };
        for e in elems.iter_mut() {
            unsafe { ptr::drop_in_place(e) };
        }

        return Some(tag);
    }

    #[inline]
    pub fn last_mut(&mut self) -> Option<TMVec<T, E>> {
        self.tags.last()?;
        Some(TMVec {
            idx: self.tags.len() - 1,
            tmv: self,
        })
    }

    #[inline]
    pub fn get_mut(&mut self, idx: usize) -> Option<TMVec<T, E>> {
        self.tags.get(idx)?;
        Some(TMVec { tmv: self, idx })
    }

    pub fn get(&self, idx: usize) -> Option<&TE<T, E>> {
        let block = *self.tags.get(idx)?;
        let ptr = &self.elements[block.tag_idx] as *const u8 as *const T;
        unsafe {
            let fat_ptr = slice::from_raw_parts(ptr, block.elem_len);
            return Some(mem::transmute::<&[T], &TE<T, E>>(fat_ptr));
        }
    }
}

impl<T, E> Drop for TaggedMultiVec<T, E> {
    fn drop(&mut self) {
        while let Some(t) = self.pop() {}
    }
}

impl<'a, T, E> TMVec<'a, T, E> {
    pub fn reserve(&mut self, len: usize) {
        let mut block = self.tmv.tags[self.idx];
        if block.elem_capa - block.elem_len >= len {
            return;
        }

        let new_capa = std::cmp::max(block.elem_len * 3 / 2 + len, block.elem_capa * 3 / 2);
        let padded_len = align_usize(self.tmv.elements.len(), mem::align_of::<T>());
        let bytes = padded_len + mem::size_of::<T>();
        let new_elem_idx = align_usize(bytes, mem::align_of::<E>());
        let final_len = new_elem_idx + new_capa * mem::size_of::<E>();
        let bytes = final_len - self.tmv.elements.len();

        self.tmv.elements.reserve(bytes);
        unsafe { self.tmv.elements.set_len(padded_len) };

        let vec_end = block.elem_idx + block.elem_len * mem::size_of::<E>();
        for idx in block.tag_idx..vec_end {
            self.tmv.elements.push(self.tmv.elements[idx]);
        }

        unsafe { self.tmv.elements.set_len(final_len) };

        block.tag_idx = padded_len;
        block.elem_idx = new_elem_idx;
        block.elem_capa = new_capa;

        self.tmv.tags[self.idx] = block;
    }

    pub fn push(&mut self, elem: E) {
        self.reserve(1);

        let block = &mut self.tmv.tags[self.idx];
        let next_idx = block.elem_idx + block.elem_len * mem::size_of::<E>();
        let next_ptr = &mut self.tmv.elements[next_idx] as *mut u8 as *mut E;

        unsafe { ptr::write(next_ptr, elem) };
        block.elem_len += 1;
    }

    pub fn extend_from_slice(&mut self, elems: &[E])
    where
        E: Clone,
    {
        self.reserve(elems.len());
        for e in elems {
            self.push(e.clone());
        }
    }
}

impl<T, E> fmt::Debug for TaggedMultiVec<T, E>
where
    T: fmt::Debug,
    E: fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.debug_list().entries(self.into_iter()).finish()
    }
}

impl<T, E> ops::Index<u32> for TaggedMultiVec<T, E> {
    type Output = TE<T, E>;

    fn index(&self, idx: u32) -> &TE<T, E> {
        return self.get(idx as usize).unwrap();
    }
}

impl<T, E> ops::Index<usize> for TaggedMultiVec<T, E> {
    type Output = TE<T, E>;

    fn index(&self, idx: usize) -> &TE<T, E> {
        return self.get(idx).unwrap();
    }
}

impl<'a, T, E> Iterator for TMVIter<'a, T, E> {
    type Item = &'a TE<T, E>;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.tmv.get(self.idx)?;
        self.idx += 1;
        return Some(val);
    }
}

impl<'a, T, E> IntoIterator for &'a TaggedMultiVec<T, E> {
    type Item = &'a TE<T, E>;
    type IntoIter = TMVIter<'a, T, E>;

    fn into_iter(self) -> Self::IntoIter {
        return TMVIter { tmv: self, idx: 0 };
    }
}
