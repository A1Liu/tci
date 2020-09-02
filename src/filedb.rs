use crate::buckets::*;
use crate::util::*;
use codespan_reporting::files::{line_starts, Files};
use core::{mem, ops, str};
use std::collections::HashMap;
use std::fs::{canonicalize, read_to_string};
use std::io;
use std::path::Path;

#[derive(Debug, Clone)]
pub struct File<'a> {
    pub _name: &'a str,
    /// The source code of the file.
    pub _source: &'a str,
    /// The starting byte indices in the source code.
    pub _line_starts: &'a [usize],
}

impl<'a> File<'a> {
    pub fn new(buckets: BucketListRef<'a>, name: &str, source: &str) -> Self {
        let line_starts: Vec<usize> = line_starts(source).collect();
        File {
            _name: buckets.add_str(name),
            _source: buckets.add_str(source),
            _line_starts: buckets.add_array(line_starts),
        }
    }

    pub fn new_frame(
        frame: &mut Frame<'a>,
        name: &str,
        source: &str,
        line_starts: &[usize],
    ) -> Self {
        File {
            _name: frame.add_str(name),
            _source: frame.add_str(source),
            _line_starts: frame.add_slice(line_starts),
        }
    }

    pub fn size(&self) -> usize {
        return align_usize(self._name.len() + self._source.len(), 8) + self._line_starts.len() * 8;
    }

    fn line_index(&self, byte_index: usize) -> Option<usize> {
        match self._line_starts.binary_search(&byte_index) {
            Ok(line) => Some(line),
            Err(next_line) => Some(next_line - 1),
        }
    }

    fn line_start(&self, line_index: usize) -> Option<usize> {
        use std::cmp::Ordering;

        match line_index.cmp(&self._line_starts.len()) {
            Ordering::Less => self._line_starts.get(line_index).cloned(),
            Ordering::Equal => Some(self._source.len()),
            Ordering::Greater => None,
        }
    }

    fn line_range(&self, line_index: usize) -> Option<core::ops::Range<usize>> {
        let line_start = self.line_start(line_index)?;
        let next_line_start = self.line_start(line_index + 1)?;

        Some(line_start..next_line_start)
    }
}

pub struct FileDb<'a> {
    pub buckets: BucketListRef<'a>,
    pub _size: usize,
    pub files: Vec<File<'a>>,
    pub translate: HashMap<&'a str, u32>,
    pub names: Vec<CodeLoc>,
}

pub const MAIN_SYMBOL: u32 = 0;
pub const VA_LIST_SYMBOL: u32 = 1;
pub const PRINTF_SYMBOL: u32 = 2;
pub const EXIT_SYMBOL: u32 = 3;

pub const STATIC_FILE: u32 = !0;
pub const NOT_A_FILE: u32 = !0;

impl<'a> FileDb<'a> {
    /// Create a new files database.
    pub fn new() -> Self {
        let mut string = String::new();
        macro_rules! add_static_name {
            ($arg:expr) => {{
                let begin = string.len();
                string.push_str($arg);
                let end = string.len();
                begin..end
            }};
        }

        let main = add_static_name!("main");
        let va_list = add_static_name!("va_list");
        let printf = add_static_name!("printf");
        let exit = add_static_name!("exit");

        let line_starts: Vec<usize> = line_starts(&string).collect();
        let _size = align_usize(string.len(), 8) + line_starts.len() * 8 + mem::size_of::<File>();
        let buckets = BucketList::with_capacity(16 * 1024 * 1024);
        let file = File::new(buckets, "", &string);

        let mut files = Vec::new();
        files.push(file);

        let mut new_self = Self {
            buckets,
            _size,
            files,
            translate: HashMap::new(),
            names: Vec::new(),
        };

        debug_assert_eq!(MAIN_SYMBOL, new_self.translate_add(main, 0));

        debug_assert_eq!(VA_LIST_SYMBOL, new_self.translate_add(va_list, 0));

        debug_assert_eq!(PRINTF_SYMBOL, new_self.translate_add(printf, 0));
        debug_assert_eq!(EXIT_SYMBOL, new_self.translate_add(exit, 0));

        new_self
    }

    pub fn iter(&self) -> impl Iterator<Item = (u32, &'a str)> {
        let iter = self.files.iter();
        let sources: Vec<(u32, &'a str)> = iter
            .enumerate()
            .skip(1)
            .map(|(id, file)| (id as u32, file._source))
            .collect();

        return sources.into_iter();
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again.
    pub fn add(&mut self, file_name: &str) -> Result<u32, io::Error> {
        let file_name = canonicalize(file_name)?;
        let file_id = self.files.len() as u32;
        let source = read_to_string(&file_name)?;
        let file = File::new(self.buckets, file_name.to_str().unwrap(), &source);
        self._size += file.size();
        self.files.push(file);
        Ok(file_id)
    }

    pub fn add_from_symbols(&mut self, base_file: u32, symbol: u32) -> Result<u32, io::Error> {
        let base_path = self.files[base_file as usize]._name;
        let cloc = self.names[symbol as usize];
        let range: ops::Range<usize> = cloc.into();
        let text = self.files[cloc.file as usize]._source;
        let text = unsafe { str::from_utf8_unchecked(&text.as_bytes()[range]) };
        let path = path_clean(Path::new(parent_if_file(base_path)).join(text).to_str().unwrap());
        return self.add(&path);
    }

    #[inline]
    pub fn translate_add(&mut self, range: ops::Range<usize>, file: u32) -> u32 {
        let cloc = l(range.start as u32, range.end as u32, file); // TODO check for overflow
        return self.translate_add_cloc(cloc);
    }

    #[inline]
    pub fn translate_add_cloc(&mut self, cloc: CodeLoc) -> u32 {
        let range: ops::Range<usize> = cloc.into();
        let text = self.files[cloc.file as usize]._source;
        let text = unsafe { str::from_utf8_unchecked(&text.as_bytes()[range]) };

        if let Some(id) = self.translate.get(text) {
            return *id;
        } else {
            let idx = self.names.len() as u32;
            self.names.push(cloc);
            self.translate.insert(text, idx);
            self._size += mem::size_of::<ShortSlice<u8>>();
            return idx;
        }
    }

    pub fn size(&self) -> usize {
        return self._size + mem::size_of::<File>() + mem::size_of::<&str>();
    }
}

impl<'a> Files<'a> for FileDb<'a> {
    type FileId = u32;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&self, file_id: u32) -> Option<&'a str> {
        Some(self.files.get(file_id as usize)?._name)
    }

    fn source(&self, file_id: u32) -> Option<&'a str> {
        Some(self.files.get(file_id as usize)?._source)
    }

    fn line_index(&self, file_id: u32, byte_index: usize) -> Option<usize> {
        let file = self.files.get(file_id as usize)?;
        return file.line_index(byte_index);
    }

    fn line_range(&self, file_id: u32, line_index: usize) -> Option<core::ops::Range<usize>> {
        let file = self.files.get(file_id as usize)?;
        return file.line_range(line_index);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FileDbRef<'a> {
    pub files: &'a [File<'a>],
    pub symbols: &'a [&'a str],
}

impl<'a> FileDbRef<'a> {
    /// Create a new files database.
    pub fn new_from_frame(frame: &mut Frame<'a>, db: &FileDb) -> Self {
        let mut file_sources = Vec::new();

        for file in db.files.iter() {
            let file = File::new_frame(frame, file._name, file._source, file._line_starts);
            file_sources.push(file);
        }

        let mut symbols = Vec::new();
        for (id, symbol) in db.names.iter().enumerate() {
            let id = id as u32;
            let range: ops::Range<usize> = (*symbol).into();
            let bytes = &file_sources[symbol.file as usize]._source.as_bytes()[range];
            symbols.push(unsafe { str::from_utf8_unchecked(bytes) });
        }

        let files = frame.add_array(file_sources);
        let symbols = frame.add_array(symbols);
        Self { files, symbols }
    }

    pub fn get_symbol(&self, symbol: u32) -> Option<&'a str> {
        return Some(self.symbols[symbol as usize]);
    }
}

impl<'a> Files<'a> for FileDbRef<'a> {
    type FileId = u32;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&self, file_id: u32) -> Option<&'a str> {
        self.get_symbol(file_id)
    }

    fn source(&self, file_id: u32) -> Option<&'a str> {
        Some(self.files.get(file_id as usize)?._source)
    }

    fn line_index(&self, file_id: u32, byte_index: usize) -> Option<usize> {
        let file = self.files.get(file_id as usize)?;
        return file.line_index(byte_index);
    }

    fn line_range(&self, file_id: u32, line_index: usize) -> Option<core::ops::Range<usize>> {
        let file = self.files.get(file_id as usize)?;
        return file.line_range(line_index);
    }
}

#[cfg(target_os = "macos")]
const PATH_SEP: u8 = b'/';
#[cfg(target_os = "linux")]
const PATH_SEP: u8 = b'/';
#[cfg(target_os = "windows")]
const PATH_SEP: u8 = b'\\';

pub fn parent_if_file<'a>(path: &'a str) -> &'a str {
    let bytes = path.as_bytes();
    let mut idx = bytes.len() - 1;
    while bytes[idx] != PATH_SEP {
        idx -= 1;
    }

    if idx == 0 {
        panic!("got relative file path {}", path);
    }

    unsafe { str::from_utf8_unchecked(&bytes[..idx]) }
}

// https://github.com/danreeves/path-clean/blob/master/src/lib.rs
pub fn path_clean(path: &str) -> String {
    let out = clean_internal(path.as_bytes());
    unsafe { String::from_utf8_unchecked(out) }
}

// https://github.com/danreeves/path-clean/blob/master/src/lib.rs
fn clean_internal(path: &[u8]) -> Vec<u8> {
    static DOT: u8 = b'.';

    if path.is_empty() {
        return vec![DOT];
    }

    let rooted = path[0] == PATH_SEP;
    let n = path.len();

    // Invariants:
    //  - reading from path; r is index of next byte to process.
    //  - dotdot is index in out where .. must stop, either because it is the
    //    leading slash or it is a leading ../../.. prefix.
    //
    // The go code this function is based on handles already-clean paths without
    // an allocation, but I haven't done that here because I think it
    // complicates the return signature too much.
    let mut out: Vec<u8> = Vec::with_capacity(n);
    let mut r = 0;
    let mut dotdot = 0;

    if rooted {
        out.push(PATH_SEP);
        r = 1;
        dotdot = 1
    }

    while r < n {
        if path[r] == PATH_SEP || path[r] == DOT && (r + 1 == n || path[r + 1] == PATH_SEP) {
            // empty path element || . element: skip
            r += 1;
        } else if path[r] == DOT && path[r + 1] == DOT && (r + 2 == n || path[r + 2] == PATH_SEP) {
            // .. element: remove to last separator
            r += 2;
            if out.len() > dotdot {
                // can backtrack, truncate to last separator
                let mut w = out.len() - 1;
                while w > dotdot && out[w] != PATH_SEP {
                    w -= 1;
                }
                out.truncate(w);
            } else if !rooted {
                // cannot backtrack, but not rooted, so append .. element
                if !out.is_empty() {
                    out.push(PATH_SEP);
                }
                out.push(DOT);
                out.push(DOT);
                dotdot = out.len();
            }
        } else {
            // real path element
            // add slash if needed
            if rooted && out.len() != 1 || !rooted && !out.is_empty() {
                out.push(PATH_SEP);
            }
            while r < n && path[r] != PATH_SEP {
                out.push(path[r]);
                r += 1;
            }
        }
    }

    // Turn empty string into "."
    if out.is_empty() {
        out.push(DOT);
    }
    out
}
