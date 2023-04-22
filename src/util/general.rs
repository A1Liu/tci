use super::term::*;
use crate::filedb::*;
use alloc::alloc::Layout;
use core::sync::atomic::AtomicUsize;
use core::{fmt, mem, ops, slice, str};
use serde::ser::{Serialize, SerializeStruct, Serializer};

pub use aliu::*;
pub use core::fmt::Write;
pub use std::collections::VecDeque;

#[allow(unused_macros)]
macro_rules! panic {
    ( $( $arg:tt )* ) => {{
        debug!( $( $arg )* );
        core::panic!();
    }};
}

#[allow(unused_macros)]
macro_rules! unimplemented {
    ( $( $arg:tt )* ) => {{
        debug!( $( $arg )* );
        core::panic!();
    }};
}

#[allow(unused_macros)]
macro_rules! unreachable {
    ( $( $arg:tt )* ) => {{
        debug!( $( $arg )* );
        core::panic!();
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

#[cfg(target_arch = "wasm32")]
pub static OUTPUT: Option<&'static (dyn Fn(String) + Sync)> = None;

pub static COUNTER: AtomicUsize = AtomicUsize::new(0);
pub const LIMIT: usize = usize::MAX;

#[allow(unused_macros)]
macro_rules! out {
    ($str:literal) => {{
        out!(@DEBUG, "{}", $str);
    }};
    (@DEBUG, $str:expr, $( $e:expr ),+ ) => {{
        if cfg!(debug_assertions) {
            out!(@LIMITED, core::concat!("DEBUG ({}:{}): ", $str, "\n"), file!(), line!(), $( $e ),+ );
        }
    }};
    (@LOG, $str:expr, $( $e:expr ),+ ) => {{
        out!(@CLEAN, core::concat!("LOG ({}:{}): ", $str, "\n"), file!(), line!(), $( $e ),+ );
    }};
    (@LIMITED, $str:expr, $( $e:expr ),+ ) => {{
        let count = $crate::COUNTER.fetch_add(1, core::sync::atomic::Ordering::SeqCst);
        if count > $crate::LIMIT {
            out!(@CLEAN, "{}", "debug statement limit reached");
            core::panic!();
        }

        out!(@CLEAN, core::concat!("{:<3} ", $str), count, $( $e ),+ );
    }};
    (@CLEAN, $str:expr, $( $e:expr ),+ ) => {{
        let s = alloc::format!( $str, $( $e ),+ );

        #[cfg(all(not(target_arch = "wasm32"), not(test)))]
        libc_print::libc_print!("{}", s);

        #[cfg(test)]
        std::print!("{}", s);

        #[allow(unused_unsafe)]
        #[cfg(target_arch = "wasm32")]
        if let Some(func) = unsafe { core::ptr::read_volatile(&$crate::OUTPUT) } {
            func(s);

        }
    }};
}

#[cfg(target_arch = "wasm32")]
pub fn register_output(f: impl Fn(String) + 'static) {
    let out = &OUTPUT as *const Option<_> as *mut Option<&'static (dyn Fn(String) + Sync)>;
    let value: &(dyn Fn(String) + 'static) = Box::leak(Box::new(f));

    unsafe { core::ptr::write_volatile(out, Some(mem::transmute(value))) };
}

macro_rules! error {
    ($arg1:expr) => {{
        let mut s = $arg1.to_string();
        if cfg!(debug_assertions) {
            s.push_str(&format!(" (in compiler at {}:{})", file!(), line!()));
        }

        $crate::util::Error::new(s, vec![])
    }};

    ($msg:expr, $loc1:expr, $msg1:expr) => {{
        let mut s = $msg.to_string();
        if cfg!(debug_assertions) {
            s.push_str(&format!(" (in compiler at {}:{})", file!(), line!()));
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

impl Into<Label> for &ErrorSection {
    fn into(self) -> Label {
        Label::new(self.location.file, self.location).with_message(&self.message)
    }
}

#[derive(Debug, serde::Serialize)]
pub struct Error {
    pub message: String,
    pub sections: Vec<ErrorSection>,
}

impl Error {
    pub fn new(message: String, sections: Vec<ErrorSection>) -> Error {
        Self {
            message: message,
            sections,
        }
    }

    pub fn render(&self, files: &FileDb, out: &mut impl Write) -> fmt::Result {
        Diagnostic::new()
            .with_message(&self.message)
            .with_labels(self.sections.iter().map(|x| x.into()).collect())
            .render(files, out)
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
    core::slice::from_raw_parts_mut(p as *mut T as *mut u8, mem::size_of::<T>())
}

pub unsafe fn any_as_u8_slice<T: Sized>(p: &T) -> &[u8] {
    unsafe { core::slice::from_raw_parts(p as *const T as *const u8, mem::size_of::<T>()) }
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

// pub struct Cursor<IO: fmt::Write> {
//     pub io: IO,
//     pub len: usize,
// }
//
// impl<IO: fmt::Write> Cursor<IO> {
//     pub fn new(io: IO) -> Self {
//         Self { io, len: 0 }
//     }
// }
//
// impl<IO: fmt::Write> fmt::Write for Cursor<IO> {
//     fn write_str(&mut self, buf: &str) -> fmt::Result {
//         let len = self.io.write_str(buf)?;
//         self.len += len;
//         return Ok(());
//     }
// }

pub struct Void {
    unused: (),
}

impl Void {
    pub fn new() -> Self {
        return Self { unused: () };
    }
}

impl fmt::Write for Void {
    fn write_str(&mut self, buf: &str) -> fmt::Result {
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

pub fn write_utf8_lossy(mut write: impl fmt::Write, bytes: &[u8]) -> fmt::Result {
    let mut iter = Utf8Lossy::from_bytes(bytes).chunks();

    const REPLACEMENT: &str = "\u{FFFD}";

    let mut total = 0;
    loop {
        let Utf8LossyChunk { valid, broken } = iter.next();
        write.write_str(valid)?;
        total += valid.len();
        if !broken.is_empty() {
            write.write_str(REPLACEMENT)?;
            total += REPLACEMENT.len();
        } else {
            return Ok(());
        }
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

#[allow(unused_macros)]
macro_rules! let_expr {
    ($left:pat = $right:expr) => {{
        if let $left = $right {
            true
        } else {
            false
        }
    }};
}

pub struct UnsafeCell<T> {
    t: T,
}

impl<T> UnsafeCell<T> {
    pub const fn new(t: T) -> Self {
        UnsafeCell { t }
    }

    pub unsafe fn get_mut(&self) -> &mut T {
        return &mut *(&self.t as *const T as *mut T);
    }
}

unsafe impl<T> Sync for UnsafeCell<T> {}
unsafe impl<T: Send> Send for UnsafeCell<T> {}

#[repr(C)]
pub struct IStr {
    pub len: usize,
    chars: (),
}

impl PartialEq for IStr {
    fn eq(&self, other: &Self) -> bool {
        return self.as_str() == other.as_str();
    }
}

impl fmt::Debug for IStr {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        return self.as_str().fmt(fmt);
    }
}

impl IStr {
    pub fn new<'a>(string: &str) -> &'a mut Self {
        return Self::with_allocator(string, Global);
    }

    pub fn with_allocator<'a>(string: &str, alloc: impl Allocator) -> &'a mut Self {
        let (size, align) = (mem::size_of::<usize>(), mem::align_of::<usize>());
        let layout = unsafe { Layout::from_size_align_unchecked(size + string.len(), align) };
        let (bytes, string) = (alloc.allocate(layout).unwrap(), string.as_bytes());
        let (i_str, len) = (bytes.as_ptr(), string.len());
        let i_str = unsafe { &mut *((&mut (&mut *i_str)[0]) as *mut u8 as *mut IStr) };

        i_str.len = len;
        unsafe { i_str.as_bytes_mut() }.copy_from_slice(string);
        return i_str;
    }

    pub unsafe fn as_bytes_mut(&mut self) -> &mut [u8] {
        let begin = &mut self.chars as *mut () as *mut u8;
        return slice::from_raw_parts_mut(begin, self.len);
    }

    pub fn as_str(&self) -> &str {
        let begin = &self.chars as *const () as *const u8;
        let slice = unsafe { slice::from_raw_parts(begin, self.len) };
        return unsafe { str::from_utf8_unchecked(slice) };
    }
}

pub trait CloneInto<'a> {
    type CloneOutput;

    fn clone_into_alloc(&self, alloc: &impl Allocator) -> Self::CloneOutput;
}

impl<'a, T> CloneInto<'a> for Option<T>
where
    T: CloneInto<'a>,
{
    type CloneOutput = Option<T::CloneOutput>;

    fn clone_into_alloc(&self, alloc: &impl Allocator) -> Self::CloneOutput {
        if let Some(a) = self.as_ref() {
            return Some(a.clone_into_alloc(alloc));
        }

        return None;
    }
}
