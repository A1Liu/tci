use crate::buckets::*;
use crate::util::*;
use codespan_reporting::files::{line_starts, Files};
use core::include_bytes;
use core::str;
use std::io;
use std::path::Path;

#[derive(Debug, Clone, Copy)]
pub enum FileType {
    System,
    Header,
    Impl,
}

#[derive(Debug, Clone, Copy)]
pub struct File<'a> {
    pub ty: FileType,
    pub name: &'a str,
    /// The source code of the file.
    pub source: &'a str,
    /// The starting byte indices in the source code.
    pub line_starts: &'a [usize],
}

impl<'a> File<'a> {
    pub fn new(buckets: impl Allocator<'a>, name: &str, source: &str) -> Self {
        let ty = if name.ends_with(".h") {
            FileType::Header
        } else {
            FileType::Impl
        };

        let line_starts: Vec<usize> = line_starts(source).collect();
        File {
            ty,
            name: buckets.add_str(name),
            source: buckets.add_str(source),
            line_starts: buckets.add_array(line_starts),
        }
    }

    pub fn new_static(
        ty: FileType,
        buckets: impl Allocator<'static>,
        name: &'static str,
        source: &'static str,
    ) -> Self {
        let line_starts: Vec<usize> = line_starts(source).collect();
        File {
            ty,
            name,
            source,
            line_starts: buckets.add_array(line_starts),
        }
    }

    fn line_index(&self, byte_index: usize) -> Option<usize> {
        match self.line_starts.binary_search(&byte_index) {
            Ok(line) => Some(line),
            Err(next_line) => Some(next_line - 1),
        }
    }

    fn line_start(&self, line_index: usize) -> Option<usize> {
        use std::cmp::Ordering;

        match line_index.cmp(&self.line_starts.len()) {
            Ordering::Less => self.line_starts.get(line_index).cloned(),
            Ordering::Equal => Some(self.source.len()),
            Ordering::Greater => None,
        }
    }

    fn line_range(&self, line_index: usize) -> Option<core::ops::Range<usize>> {
        let line_start = self.line_start(line_index)?;
        let next_line_start = self.line_start(line_index + 1)?;

        Some(line_start..next_line_start)
    }
}

pub struct InitSyms {
    pub names: Vec<&'static str>,
    pub translate: HashMap<&'static str, u32>,
}

lazy_static! {
    pub static ref SYS_LIBS: Vec<File<'static>> = {
        let buckets = BucketListFactory::new();
        let mut m = Vec::new();

        macro_rules! new_file {
            (@IMPL, $file:literal) => {{
                let file = concat!("libs/", $file);
                let lib: &[u8] = include_bytes!(concat!("../lib/impl/", $file));
                let lib = unsafe { str::from_utf8_unchecked(lib) };
                m.push(File::new_static(FileType::System, &*buckets, file, lib));
            }};
            (@HEADER, $file:literal) => {{
                let header: &[u8] = include_bytes!(concat!("../lib/header/", $file));
                let header = unsafe { str::from_utf8_unchecked(header) };
                m.push(File::new_static(FileType::Header, &*buckets, $file, header));
            }};
        }

        new_file!(@HEADER, "tci.h");
        new_file!(@HEADER, "stdio.h");
        new_file!(@HEADER, "stdlib.h");
        new_file!(@HEADER, "string.h");
        new_file!(@HEADER, "stddef.h");
        new_file!(@HEADER, "stdint.h");
        new_file!(@HEADER, "stdarg.h");
        new_file!(@HEADER, "stdbool.h");
        new_file!(@HEADER, "float.h");

        new_file!(@IMPL, "tci.c");
        new_file!(@IMPL, "printf.c");
        new_file!(@IMPL, "stdlib.h");
        new_file!(@IMPL, "string.h");
        new_file!(@IMPL, "stdarg.h");

        m
    };
}

pub const NO_SYMBOL: u32 = !0;

pub struct FileDb {
    pub buckets: BucketListFactory,
    pub names: HashMap<(bool, &'static str), u32>,
    pub files: Vec<File<'static>>,
}

impl Drop for FileDb {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

impl FileDb {
    #[inline]
    pub fn new() -> Self {
        let mut new_self = Self {
            buckets: BucketListFactory::new(),
            files: Vec::new(),
            names: HashMap::new(),
        };

        for (idx, file) in SYS_LIBS.iter().enumerate() {
            new_self.files.push(*file);
            new_self.names.insert((true, file.name), idx as u32);
        }

        new_self
    }

    pub fn impls(&self) -> Vec<u32> {
        let mut out = Vec::with_capacity(self.files.len());
        for (idx, file) in self.files.iter().enumerate() {
            if let FileType::Header = file.ty {
                continue;
            }

            out.push(idx as u32);
        }

        return out;
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again. Errors if the file already exists in the database.
    pub fn add(&mut self, file_name: &str, source: &str) -> Result<u32, io::Error> {
        if let Some(id) = self.names.get(&(false, file_name)) {
            return Err(io::ErrorKind::AlreadyExists.into());
        }

        let file_id = self.files.len() as u32;
        let file = File::new(&*self.buckets, file_name, &source);
        self.files.push(file);
        self.names.insert((false, file.name), file_id);

        Ok(file_id)
    }

    pub fn display_loc(&self, loc: CodeLoc) -> Option<String> {
        use codespan_reporting::diagnostic::*;
        use codespan_reporting::term::*;

        let mut out = StringWriter::new();
        let config = Config {
            display_style: DisplayStyle::Rich,
            tab_width: 4,
            styles: Styles::default(),
            chars: Chars::default(),
            start_context_lines: 3,
            end_context_lines: 1,
        };

        let diagnostic =
            Diagnostic::new(Severity::Void).with_labels(vec![Label::primary(loc.file, loc)]);
        emit(&mut out, &config, self, &diagnostic).unwrap();

        return Some(out.into_string());
    }

    pub fn resolve_include(&self, include: &str, file: u32) -> Result<u32, io::Error> {
        if Path::new(include).is_relative() {
            let or_else = || -> io::Error { io::ErrorKind::NotFound.into() };
            let base_path = parent_if_file(self.files.get(file as usize).ok_or_else(or_else)?.name);
            let real_path = Path::new(base_path).join(include);
            let path_str = real_path.to_str().unwrap();

            if let Some(id) = self.names.get(&(false, &path_str)) {
                return Ok(*id);
            }

            return Err(io::ErrorKind::NotFound.into());
        }

        if let Some(id) = self.names.get(&(false, include)) {
            return Ok(*id);
        }

        return Err(io::ErrorKind::NotFound.into());
    }

    pub fn resolve_system_include(&self, include: &str) -> Result<u32, io::Error> {
        if let Some(id) = self.names.get(&(true, include)) {
            return Ok(*id);
        }

        return Err(io::ErrorKind::NotFound.into());
    }
}

impl<'a> Files<'a> for FileDb {
    type FileId = u32;
    type Name = &'a str;
    type Source = &'a str;

    fn name(&self, file_id: u32) -> Option<&'a str> {
        Some(self.files.get(file_id as usize)?.name)
    }

    fn source(&self, file_id: u32) -> Option<&'a str> {
        Some(self.files.get(file_id as usize)?.source)
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

pub struct Symbols {
    pub buckets: BucketListFactory,
    pub to_symbol: HashMap<&'static str, u32>,
    pub to_name: Vec<&'static str>,
}

impl Drop for Symbols {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

#[repr(u32)]
pub enum BuiltinSymbol {
    Main = 0,

    MacroDefined,

    BuiltinPush,
    BuiltinPushDyn,
    BuiltinEcall,
}

impl Symbols {
    pub fn new() -> Self {
        let mut new_self = Self {
            buckets: BucketListFactory::new(),
            to_symbol: HashMap::new(),
            to_name: Vec::new(),
        };

        new_self.add_str("main");

        new_self.add_str("defined");

        new_self.add_str("__tci_builtin_push");
        new_self.add_str("__tci_builtin_push_dyn");
        new_self.add_str("__tci_builtin_ecall");

        new_self
    }

    pub fn add_str(&mut self, s: &str) -> u32 {
        if let Some(id) = self.to_symbol.get(s) {
            return *id;
        }

        let s = self.buckets.add_str(s);
        let id = self.to_name.len() as u32;
        self.to_symbol.insert(s, id);
        self.to_name.push(s);
        return id;
    }

    pub fn from_str(&self, s: &str) -> n32 {
        if let Some(id) = self.to_symbol.get(s) {
            return (*id).into();
        }

        return n32::NULL;
    }

    pub fn to_str(&self, id: u32) -> Option<&str> {
        return self.to_name.get(id as usize).map(|a| *a);
    }
}

#[cfg(not(target_os = "windows"))]
const PATH_SEP: u8 = b'/';
#[cfg(target_os = "windows")]
const PATH_SEP: u8 = b'\\';

pub fn parent_if_file<'a>(path: &'a str) -> &'a str {
    let bytes = path.as_bytes();
    let mut idx = bytes.len() - 1;
    while bytes[idx] != PATH_SEP {
        if idx == 0 {
            return ""; // idk man this works
        }
        idx -= 1;
    }

    unsafe { str::from_utf8_unchecked(&bytes[..(idx + 1)]) }
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
