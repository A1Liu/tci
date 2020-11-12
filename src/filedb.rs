use crate::buckets::*;
use crate::util::*;
use codespan_reporting::files::{line_starts, Files};
use core::include_bytes;
use core::{mem, ops, str};
use serde::Serialize;
use std::collections::HashMap;
use std::fs::read_to_string;
use std::io;
use std::path::Path;

#[derive(Debug, Clone, Copy, Serialize)]
pub struct File<'a> {
    pub _name: &'a str,
    /// The source code of the file.
    pub _source: &'a str,
    /// The starting byte indices in the source code.
    pub _line_starts: &'a [usize],
}

impl<'a> File<'a> {
    pub fn new_static(name: &'static str, source: &'static str) -> Self {
        let line_starts: Vec<usize> = line_starts(source).collect();
        let line_starts: Box<[usize]> = line_starts.into();
        File {
            _name: name,
            _source: source,
            _line_starts: Box::leak(line_starts),
        }
    }

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

pub const NO_SYMBOL: u32 = !0;

pub struct FileDb {
    buckets: BucketListRef<'static>,
    pub buckets_next: BucketListRef<'static>,
    pub _size: usize,
    pub file_names: HashMap<&'static str, u32>,
    pub files: Vec<File<'static>>,
    pub translate: HashMap<&'static str, u32>,
    pub names: Vec<CodeLoc>,
    pub fs_read_access: bool,
}

pub struct InitSyms {
    pub names: Vec<&'static str>,
    pub translate: HashMap<&'static str, u32>,
    pub files: Vec<File<'static>>,
}

pub static INIT_SYMS: LazyStatic<InitSyms> = lazy_static!(init_syms_lazy_static, InitSyms, {
    let mut names = Vec::new();
    let mut translate = HashMap::new();
    let mut files = Vec::new();
    macro_rules! add_sym {
        ($arg:expr) => {
            let begin = names.len() as u32;
            names.push($arg);
            translate.insert($arg, begin);
        };
    }

    macro_rules! add_syslib_sym {
        ($arg:expr) => {
            let begin = names.len() as u32;
            names.push($arg);
            translate.insert($arg, begin);
            let source = include_bytes!(concat!("../includes/", $arg));
            let source = unsafe { str::from_utf8_unchecked(source) };
            files.push(File::new_static($arg, source));
        };
    }

    // System files have the same symbol id as their file id. These lines need to come first.
    add_syslib_sym!("stdio.h");
    add_syslib_sym!("stdlib.h");

    add_sym!("main");
    add_sym!("va_list");
    add_sym!("printf");
    add_sym!("exit");
    add_sym!("malloc");
    add_sym!("free");

    InitSyms {
        names,
        translate,
        files,
    }
});

impl Drop for FileDb {
    fn drop(&mut self) {
        while let Some(b) = unsafe { self.buckets.dealloc() } {
            self.buckets = b;
        }
    }
}

impl FileDb {
    /// Create a new files database.
    pub fn new(fs_read_access: bool) -> Self {
        let mut string = String::new();
        let mut symbols = Vec::new();
        for name in INIT_SYMS.names.iter() {
            let begin = string.len();
            string.push_str(name);
            let end = string.len();
            symbols.push(begin..end);
        }

        let buckets = BucketList::with_capacity(16 * 1024 * 1024);
        let file = File::new(buckets, "", &string);
        let mut _size = file.size() + mem::size_of::<File>();

        let mut files = Vec::new();

        for file in INIT_SYMS.files.iter() {
            files.push(*file);
            _size += file.size() + mem::size_of::<File>();
        }
        files.push(file);

        let mut new_self = Self {
            buckets,
            buckets_next: buckets,
            _size,
            files,
            file_names: HashMap::new(),
            translate: HashMap::new(),
            names: Vec::new(),
            fs_read_access,
        };

        for symbol in symbols {
            new_self.translate_add(symbol, INIT_SYMS.files.len() as u32);
        }

        new_self
    }

    pub fn iter(&self) -> impl Iterator<Item = u32> {
        return self.vec().into_iter();
    }

    pub fn vec(&self) -> Vec<u32> {
        let iter = self.files.iter();
        return (0..self.files.len() as u32)
            .skip(INIT_SYMS.files.len() + 1) // +1 here is for the init syms initial file
            .collect();
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again. Errors if the file already exists in the database.
    pub fn add(&mut self, file_name: &str, source: &str) -> Result<u32, io::Error> {
        if let Some(id) = self.file_names.get(file_name) {
            return Err(io::ErrorKind::AlreadyExists.into());
        }

        let file_id = self.files.len() as u32;
        let file = File::new(self.buckets_next, file_name, &source);
        self._size += file.size() + mem::size_of::<File>();
        self.files.push(file);

        while let Some(b) = self.buckets_next.next() {
            self.buckets_next = b;
        }

        Ok(file_id)
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again. Returns existing file handle if file already exists in
    /// the database
    pub fn add_from_fs(&mut self, file_name: &str) -> Result<u32, io::Error> {
        if let Some(id) = self.file_names.get(file_name) {
            return Ok(*id);
        }

        if !self.fs_read_access {
            return Err(io::ErrorKind::PermissionDenied.into());
        }

        let file_id = self.files.len() as u32;
        let source = read_to_string(&file_name)?;
        let file = File::new(self.buckets_next, file_name, &source);
        self._size += file.size() + mem::size_of::<File>();
        self.files.push(file);

        while let Some(b) = self.buckets_next.next() {
            self.buckets_next = b;
        }

        Ok(file_id)
    }

    pub fn symbol_to_str(&self, symbol: u32) -> &str {
        let cloc = self.names[symbol as usize];
        return self.cloc_to_str(cloc);
    }

    pub fn cloc_to_str(&self, cloc: CodeLoc) -> &str {
        let range: ops::Range<usize> = cloc.into();
        let text = self.files[cloc.file as usize]._source;
        return unsafe { str::from_utf8_unchecked(&text.as_bytes()[range]) };
    }

    pub fn add_from_symbols(&mut self, base_file: u32, symbol: u32) -> Result<u32, io::Error> {
        let cloc = self.names[symbol as usize];
        let range: ops::Range<usize> = cloc.into();
        let text = self.files[cloc.file as usize]._source;
        let text = unsafe { str::from_utf8_unchecked(&text.as_bytes()[range]) };

        if Path::new(text).is_relative() {
            let base_path = parent_if_file(self.files[base_file as usize]._name);
            let real_path = Path::new(base_path).join(text);
            let path_str = real_path.to_str().unwrap();
            return self.add_from_fs(&path_str);
        }

        return self.add_from_fs(&text);
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
            self._size += mem::size_of::<&str>();
            return idx;
        }
    }

    pub fn size(&self) -> usize {
        return self._size + mem::size_of::<File>() + mem::size_of::<&str>();
    }
}

impl<'a> Files<'a> for FileDb {
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

#[derive(Debug, Clone, Copy, Serialize)]
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
        for symbol in db.names.iter() {
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
        if idx == 0 {
            panic!("got relative file path {}", path);
        }
        idx -= 1;
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
