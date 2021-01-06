use crate::buckets::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::termcolor::{ColorSpec, WriteColor};
use core::borrow::Borrow;
use core::{fmt, marker, ops, slice, str};
use serde::ser::{Serialize, SerializeMap, SerializeStruct, Serializer};
use std::collections::hash_map::DefaultHasher;
use std::hash::{BuildHasher, Hash, Hasher};
use std::io;
use std::sync::*;

pub use lazy_static::lazy_static;
pub use std::collections::HashMap;

#[allow(unused_macros)]
macro_rules! debug {
    ($expr:expr) => {{
        let expr = &$expr;
        println!(
            "DEBUG ({}:{}): {} = {:?}",
            file!(),
            line!(),
            stringify!($expr),
            expr
        );
    }};
}

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

pub struct StringArray {
    bytes: Vec<u8>,
    indices: Vec<ops::Range<usize>>,
}

impl StringArray {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
            indices: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        return self.indices.len();
    }

    pub fn push(&mut self, string: &str) {
        let begin = self.bytes.len();
        self.bytes.extend_from_slice(string.as_bytes());
        let end = self.bytes.len();
        self.indices.push(begin..end);
    }
}

impl ops::Index<usize> for StringArray {
    type Output = str;

    fn index(&self, idx: usize) -> &str {
        unsafe { str::from_utf8_unchecked(&self.bytes[self.indices[idx].clone()]) }
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

    pub fn iter(&self) -> HashRefIter<'a, K, V> {
        HashRefIter {
            slots: self.slots,
            slot_idx: 0,
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

impl<'a, Key, Value, State> fmt::Debug for HashRef<'a, Key, Value, State>
where
    Key: Eq + Hash + Copy + fmt::Debug,
    Value: fmt::Debug + Copy,
    State: BuildHasher,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.debug_map().entries(self.iter()).finish()
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
        for (key, value) in self.iter() {
            map.serialize_entry(key, value)?;
        }
        map.end()
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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
}

impl Into<u32> for n32 {
    fn into(self) -> u32 {
        if self == Self::NULL {
            panic!("NullPointerException");
        }

        return self.data;
    }
}

impl From<u32> for n32 {
    fn from(data: u32) -> Self {
        Self::new(data)
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

lazy_static! {
    pub static ref INDENT: Mutex<String> = Mutex::new("".to_string());
}

#[allow(unused_macros)]
macro_rules! indent {
    ($format:literal) => {{
        print!("{}", INDENT.lock().unwrap());
        println!($format);
    }};
    ($format:literal,$($e:expr),* ) => {{
        print!("{}", INDENT.lock().unwrap());
        println!($format, $( $e ),*);
    }};
}

pub fn indent_go_down() {
    *INDENT.lock().unwrap() += "  ";
}

pub fn indent_return() {
    let mut i = INDENT.lock().unwrap();
    i.pop();
    i.pop();
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
