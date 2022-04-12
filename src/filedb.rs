use crate::util::*;
use aliu::{AllocExt, Allocator};
use core::include_bytes;
use core::{fmt, mem, str};

/// The column index at the given byte index in the source file.
/// This is the number of characters to the given byte index.
///
/// If the byte index is smaller than the start of the line, then `0` is returned.
/// If the byte index is past the end of the line, the column index of the last
/// character `+ 1` is returned.
pub fn column_index(source: &str, line_range: core::ops::Range<usize>, byte_index: usize) -> usize {
    let end_index = core::cmp::min(byte_index, core::cmp::min(line_range.end, source.len()));

    (line_range.start..end_index)
        .filter(|byte_index| source.is_char_boundary(byte_index + 1))
        .count()
}

pub fn line_starts<'source>(source: &'source str) -> impl 'source + Iterator<Item = usize> {
    core::iter::once(0).chain(source.match_indices('\n').map(|(i, _)| i + 1))
}

/// A user-facing location in a source file.
///
/// Returned by [`Files::location`].
///
/// [`Files::location`]: Files::location
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Location {
    /// The user-facing line number.
    pub line_number: usize,
    /// The user-facing column number.
    pub column_number: usize,
}

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
    pub fn new(buckets: &impl Allocator, name: &str, source: &str) -> Self {
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
            line_starts: buckets.add_slice(&line_starts),
        }
    }

    pub fn new_static(
        ty: FileType,
        buckets: &impl Allocator,
        name: &'static str,
        source: &'static str,
    ) -> Self {
        let line_starts: Vec<usize> = line_starts(source).collect();
        File {
            ty,
            name,
            source,
            line_starts: buckets.add_slice(&line_starts),
        }
    }

    fn line_index(&self, byte_index: usize) -> Option<usize> {
        match self.line_starts.binary_search(&byte_index) {
            Ok(line) => Some(line),
            Err(next_line) => Some(next_line - 1),
        }
    }

    fn line_start(&self, line_index: usize) -> Option<usize> {
        use core::cmp::Ordering;

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
        let buckets = aliu::BucketList::new();

        let mut m = Vec::new();

        macro_rules! new_file {
            (@IMPL, $file:literal) => {{
                let file = concat!("libs/", $file);
                let lib: &[u8] = include_bytes!(concat!("../lib/impl/", $file));
                let lib = unsafe { str::from_utf8_unchecked(lib) };
                m.push(File::new_static(FileType::System, &buckets, file, lib));
            }};
            (@IMPL_HEADER, $file:literal) => {{
                let file = concat!("libs/", $file);
                let lib: &[u8] = include_bytes!(concat!("../lib/impl/", $file));
                let lib = unsafe { str::from_utf8_unchecked(lib) };
                m.push(File::new_static(FileType::Header, &buckets, file, lib));
            }};
            (@HEADER, $file:literal) => {{
                let header: &[u8] = include_bytes!(concat!("../lib/header/", $file));
                let header = unsafe { str::from_utf8_unchecked(header) };
                m.push(File::new_static(FileType::Header, &buckets, $file, header));
            }};
        }

        new_file!(@HEADER, "tci.h");

        new_file!(@HEADER, "string.h");
        new_file!(@HEADER, "strings.h");

        new_file!(@HEADER, "stdio.h");
        new_file!(@HEADER, "stdlib.h");
        new_file!(@HEADER, "stddef.h");
        new_file!(@HEADER, "stdint.h");
        new_file!(@HEADER, "stdarg.h");
        new_file!(@HEADER, "stdbool.h");

        new_file!(@HEADER, "float.h");
        new_file!(@HEADER, "ctype.h");
        new_file!(@HEADER, "errno.h");
        new_file!(@HEADER, "limits.h");

        new_file!(@HEADER, "inttypes.h");

        new_file!(@HEADER, "sys/types.h");

        new_file!(@IMPL, "tci.c");
        new_file!(@IMPL, "printf.c");
        new_file!(@IMPL, "sscanf.c");
        new_file!(@IMPL, "scanf.c");
        new_file!(@IMPL, "stdlib.c");
        new_file!(@IMPL, "str_to_x.c");
        new_file!(@IMPL, "string.c");
        new_file!(@IMPL, "strings.c");
        new_file!(@IMPL, "ctype.c");
        new_file!(@IMPL, "files.c");
        new_file!(@IMPL, "errors.c");

        mem::forget(buckets);

        m
    };
}

pub const NO_SYMBOL: u32 = !0;

pub struct FileDb {
    pub buckets: aliu::BucketList,
    pub names: HashMap<(bool, &'static str), u32>,
    pub files: Vec<File<'static>>,
}

impl FileDb {
    #[inline]
    pub fn new() -> Self {
        let mut new_self = Self {
            buckets: aliu::BucketList::new(),
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
    pub fn add(&mut self, file_name: &str, source: &str) -> Result<u32, &'static str> {
        if let Some(id) = self.names.get(&(false, file_name)) {
            return Err("already exists");
        }

        let file_id = self.files.len() as u32;
        let file = File::new(&self.buckets, file_name, &source);
        self.files.push(file);
        self.names.insert((false, file.name), file_id);

        Ok(file_id)
    }

    pub fn display_loc(&self, out: &mut impl fmt::Write, loc: CodeLoc) -> fmt::Result {
        let file = self.files[loc.file as usize];
        let start_line = file.line_index(loc.start as usize).unwrap();
        let end_line = file.line_index(loc.end as usize).unwrap();

        let start = file.line_start(start_line).unwrap();
        let end = file.line_start(end_line + 1).unwrap();
        let bytes = &file.source.as_bytes()[start..end];

        return write!(out, "{}", unsafe { str::from_utf8_unchecked(bytes) });
    }

    pub fn write_loc(&self, out: &mut impl fmt::Write, loc: CodeLoc) -> fmt::Result {
        let file = self.files[loc.file as usize];
        let line = file.line_index(loc.start as usize).unwrap() + 1;
        return write!(out, "{}:{}", file.name, line);
    }

    pub fn loc_to_string(&self, loc: CodeLoc) -> String {
        let mut out = StringWriter::new();
        self.write_loc(&mut out, loc).unwrap();
        return out.into_string();
    }

    pub fn resolve_include(&self, include: &str, file: u32) -> Result<u32, &'static str> {
        if !include.starts_with("/") {
            let or_else = || -> &'static str { "not found" };
            let mut path =
                parent_if_file(self.files.get(file as usize).ok_or_else(or_else)?.name).to_string();
            if !path.ends_with("/") && path != "" {
                path.push_str("/");
            }
            path.push_str(include);

            if let Some(id) = self.names.get(&(false, &*path)) {
                return Ok(*id);
            }

            return Err("not found");
        }

        if let Some(id) = self.names.get(&(false, include)) {
            return Ok(*id);
        }

        return Err("not found");
    }

    pub fn resolve_system_include(&self, include: &str) -> Result<u32, &'static str> {
        if let Some(id) = self.names.get(&(true, include)) {
            return Ok(*id);
        }

        return Err("not found");
    }

    pub fn name(&self, file_id: u32) -> Option<&str> {
        Some(self.files.get(file_id as usize)?.name)
    }

    pub fn source(&self, file_id: u32) -> Option<&str> {
        Some(self.files.get(file_id as usize)?.source)
    }

    pub fn line_index(&self, file_id: u32, byte_index: usize) -> Option<usize> {
        let file = self.files.get(file_id as usize)?;
        return file.line_index(byte_index);
    }

    pub fn line_range(&self, file_id: u32, line_index: usize) -> Option<core::ops::Range<usize>> {
        let file = self.files.get(file_id as usize)?;
        return file.line_range(line_index);
    }

    /// The user-facing line number at the given line index.
    /// It is not necessarily checked that the specified line index
    /// is actually in the file.
    ///
    /// # Note for trait implementors
    ///
    /// This is usually 1-indexed from the beginning of the file, but
    /// can be useful for implementing something like the
    /// [C preprocessor's `#line` macro][line-macro].
    ///
    /// [line-macro]: https://en.cppreference.com/w/c/preprocessor/line
    #[allow(unused_variables)]
    pub fn line_number(&self, id: u32, line_index: usize) -> Option<usize> {
        Some(line_index + 1)
    }

    /// The user-facing column number at the given line index and byte index.
    ///
    /// # Note for trait implementors
    ///
    /// This is usually 1-indexed from the the start of the line.
    /// A default implementation is provided, based on the [`column_index`]
    /// function that is exported from the [`files`] module.
    ///
    /// [`files`]: crate::files
    /// [`column_index`]: crate::files::column_index
    pub fn column_number(&self, id: u32, line_index: usize, byte_index: usize) -> Option<usize> {
        let source = self.source(id)?;
        let line_range = self.line_range(id, line_index)?;
        let column_index = column_index(source.as_ref(), line_range, byte_index);

        Some(column_index + 1)
    }

    /// Convenience method for returning line and column number at the given
    /// byte index in the file.
    pub fn location(&self, id: u32, byte_index: usize) -> Option<Location> {
        let line_index = self.line_index(id, byte_index)?;

        Some(Location {
            line_number: self.line_number(id, line_index)?,
            column_number: self.column_number(id, line_index, byte_index)?,
        })
    }
}

pub struct Symbols {
    pub buckets: aliu::BucketList,
    pub to_symbol: HashMap<&'static str, u32>,
    pub to_name: Vec<&'static str>,
}

#[repr(u32)]
pub enum BuiltinSymbol {
    Main = 0,

    MacroDefined,

    BuiltinPush,
    BuiltinOp,
}

impl Symbols {
    pub fn new() -> Self {
        let mut new_self = Self {
            buckets: aliu::BucketList::new(),
            to_symbol: HashMap::new(),
            to_name: Vec::new(),
        };

        new_self.add_str("main");

        new_self.add_str("defined");

        new_self.add_str("__tci_builtin_push");
        new_self.add_str("__tci_builtin_op");

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
