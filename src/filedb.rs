use crate::buckets::*;
use crate::util::*;
use codespan_reporting::files::{line_starts, Files};
use core::include_bytes;
use core::{mem, ops, str};
use serde::Serialize;
use std::fs::read_to_string;
use std::io;
use std::path::Path;

#[derive(Debug, Clone, Copy, Serialize)]
pub struct File<'a> {
    pub _is_system: bool,
    pub _name: &'a str,
    /// The source code of the file.
    pub _source: &'a str,
    /// The starting byte indices in the source code.
    pub _line_starts: &'a [usize],
}

impl<'a> File<'a> {
    pub fn new_sys(name: &'static str, source: &'static str) -> Self {
        let line_starts: Vec<usize> = line_starts(source).collect();
        let line_starts: Box<[usize]> = line_starts.into();
        File {
            _is_system: true,
            _name: name,
            _source: source,
            _line_starts: Box::leak(line_starts),
        }
    }

    pub fn new(buckets: impl Allocator<'a>, name: &str, source: &str) -> Self {
        let line_starts: Vec<usize> = line_starts(source).collect();
        File {
            _is_system: false,
            _name: buckets.add_str(name),
            _source: buckets.add_str(source),
            _line_starts: buckets.add_array(line_starts),
        }
    }

    pub fn new_alloc(frame: impl Allocator<'a>, file: File) -> Self {
        File {
            _is_system: file._is_system,
            _name: frame.add_str(file._name),
            _source: frame.add_str(file._source),
            _line_starts: frame.add_slice(file._line_starts),
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

pub struct InitSyms {
    pub names: Vec<&'static str>,
    pub translate: HashMap<&'static str, u32>,
}

pub struct SysLib {
    pub header: &'static [u8],
    pub lib_file: &'static str,
    pub lib: &'static [u8],
}

lazy_static! {
    pub static ref SYS_LIBS: HashMap<&'static str, SysLib> = {
        let mut m = HashMap::new();
        macro_rules! sys_lib {
            ($file:literal) => {{
                let header: &[u8] = include_bytes!(concat!("../clib/header/", $file));
                let lib: &[u8] = include_bytes!(concat!("../clib/impl/", $file));
                let lib_file: &str = concat!("/libs/", $file);
                m.insert(
                    $file,
                    SysLib {
                        header,
                        lib_file,
                        lib,
                    },
                );
            }};
        }

        sys_lib!("tci.h");
        sys_lib!("stdio.h");
        sys_lib!("stdlib.h");
        sys_lib!("string.h");
        sys_lib!("stddef.h");
        sys_lib!("stdint.h");
        sys_lib!("stdarg.h");

        m
    };
}

pub const NO_SYMBOL: u32 = !0;

pub struct FileDbSlim {
    pub buckets: BucketListFactory,
    pub file_names: HashMap<&'static str, u32>,
    pub size: usize,
    pub garbage_size: usize,
    pub files: Vec<Option<File<'static>>>,
    pub empty_slots: Vec<u32>,
}

impl Drop for FileDbSlim {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

impl FileDbSlim {
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buckets: BucketListFactory::with_capacity(capacity),
            size: 0,
            garbage_size: 0,
            file_names: HashMap::new(),
            files: Vec::new(),
            empty_slots: Vec::new(),
        }
    }

    pub fn new() -> Self {
        Self {
            buckets: BucketListFactory::new(),
            file_names: HashMap::new(),
            size: 0,
            garbage_size: 0,
            files: Vec::new(),
            empty_slots: Vec::new(),
        }
    }

    pub fn line_index(&self, loc: CodeLoc) -> Option<usize> {
        let file = loc.file - 1;
        let file = self.files.get(file as usize)?.as_ref()?;
        return file.line_index(loc.start as usize);
    }

    pub fn file_db(&self) -> FileDb {
        let mut db = FileDb::with_capacity_slim(self.size, false);
        for file in &self.files {
            if let Some(file) = file {
                db.add(file._name, file._source).unwrap();
            } else {
                db.files.push(None);
                db._size += mem::size_of::<Option<File>>();
            }
        }

        for lib in SYS_LIBS.values() {
            db.add(lib.lib_file, unsafe { str::from_utf8_unchecked(lib.lib) })
                .unwrap();
        }

        return db;
    }

    pub fn add(&mut self, file_name: &str, source: &str) -> u32 {
        if self.garbage_size > self.size * 4 {
            *self = self.copy_gc();
        }

        return self.add_internal(file_name, source);
    }

    pub fn copy_gc(&self) -> Self {
        let mut new = Self::with_capacity(self.size);
        for (idx, file) in self.files.iter().enumerate() {
            if let Some(file) = file {
                new.add_internal(file._name, file._source);
            } else {
                new.empty_slots.push(idx as u32);
                new.files.push(None);
            }
        }
        return new;
    }

    pub fn remove_str(&mut self, file: &str) -> bool {
        let file = if let Some(file) = self.file_names.remove(file) {
            file
        } else {
            return false;
        };
        let file_slot = &mut self.files[file as usize];
        let file_data = file_slot.take().unwrap();

        self.garbage_size += file_data.size();
        self.size -= file_data.size();
        self.empty_slots.push(file);
        return true;
    }

    pub fn remove_id(&mut self, file: u32) -> bool {
        let file_slot = self.files.get_mut(file as usize);
        let file_slot = match file_slot {
            Some(f) => f,
            None => return false,
        };

        let file_data = match file_slot.take() {
            Some(f) => f,
            None => return false,
        };

        self.garbage_size += file_data.size();
        self.size -= file_data.size();
        self.file_names.remove(file_data._name);
        self.empty_slots.push(file);
        return true;
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again. Replaces the original if the file already exists in the database.
    pub fn add_internal(&mut self, file_name: &str, source: &str) -> u32 {
        let file_name_string = if file_name.as_bytes()[0] == b'/' {
            file_name.to_string()
        } else {
            let mut string = String::new();
            string.push('/');
            string.push_str(file_name);
            string
        };

        let file_name: &str = &file_name_string;

        let file = File::new(&*self.buckets, file_name, source);
        self.size += file.size();

        if let Some(file_idx) = self.file_names.get(file_name) {
            let file_slot = &mut self.files[*file_idx as usize];
            let file_slot = file_slot.as_mut().unwrap();
            self.garbage_size += file_slot.size();
            self.size -= file_slot.size();
            *file_slot = file;
            return *file_idx;
        }

        let file_idx = if let Some(file_idx) = self.empty_slots.pop() {
            file_idx
        } else {
            let file_idx = self.files.len() as u32;
            self.files.push(None);
            file_idx
        };

        self.size += file.size();
        self.files[file_idx as usize] = Some(file);
        self.file_names.insert(file._name, file_idx);
        return file_idx;
    }
}

pub struct FileDb {
    pub buckets: BucketListFactory,
    pub _size: usize,
    pub file_names: HashMap<&'static str, u32>,
    pub files: Vec<Option<File<'static>>>,
    pub translate: HashMap<&'static str, u32>,
    pub names: Vec<CodeLoc>,
    pub fs_read_access: bool,
}

impl Drop for FileDb {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

impl FileDb {
    pub fn with_capacity_slim(capacity: usize, fs_read_access: bool) -> Self {
        Self {
            buckets: BucketListFactory::with_capacity(capacity),
            _size: 0,
            files: Vec::new(),
            file_names: HashMap::new(),
            translate: HashMap::new(),
            names: Vec::new(),
            fs_read_access,
        }
    }

    pub fn with_capacity(capacity: usize, fs_read_access: bool) -> Self {
        let mut new_self = Self::with_capacity_slim(capacity, fs_read_access);

        for lib in SYS_LIBS.values() {
            new_self
                .add(lib.lib_file, unsafe { str::from_utf8_unchecked(lib.lib) })
                .unwrap();
        }

        new_self
    }

    /// Create a new files database.
    #[inline]
    pub fn new(fs_read_access: bool) -> Self {
        let mut new_self = Self {
            buckets: BucketListFactory::new(),
            _size: 0,
            files: Vec::new(),
            file_names: HashMap::new(),
            translate: HashMap::new(),
            names: Vec::new(),
            fs_read_access,
        };

        for lib in SYS_LIBS.values() {
            new_self
                .add(lib.lib_file, unsafe { str::from_utf8_unchecked(lib.lib) })
                .unwrap();
        }

        new_self
    }

    pub fn iter(&self) -> impl Iterator<Item = u32> {
        return self.vec().into_iter();
    }

    pub fn vec(&self) -> Vec<u32> {
        let iter = self.files.iter().enumerate();
        let filter_map = |(idx, value): (usize, &Option<File>)| value.as_ref().map(|_| idx as u32);

        return iter.filter_map(filter_map).collect();
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again. Errors if the file already exists in the database.
    pub fn add(&mut self, file_name: &str, source: &str) -> Result<u32, io::Error> {
        if let Some(id) = self.file_names.get(file_name) {
            return Err(io::ErrorKind::AlreadyExists.into());
        }

        let file_id = self.files.len() as u32;
        let file = File::new(&*self.buckets, file_name, &source);
        self._size += file.size() + mem::size_of::<Option<File>>();
        self.files.push(Some(file));
        self.file_names.insert(file._name, file_id);

        Ok(file_id)
    }

    pub fn add_from_include(&mut self, include: &str, file: u32) -> Result<u32, io::Error> {
        if Path::new(include).is_relative() {
            let base_path = parent_if_file(self.files[file as usize].unwrap()._name);
            let real_path = Path::new(base_path).join(include);
            let path_str = real_path.to_str().unwrap();

            if let Some(id) = self.file_names.get(&path_str) {
                return Ok(*id);
            }

            if !self.fs_read_access {
                return Err(io::ErrorKind::PermissionDenied.into());
            }

            let source = read_to_string(&path_str)?;
            return self.add(&path_str, &source);
        }

        if let Some(id) = self.file_names.get(include) {
            return Ok(*id);
        }

        if !self.fs_read_access {
            return Err(io::ErrorKind::PermissionDenied.into());
        }

        let source = read_to_string(include)?;
        return self.add(include, &source);
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again. Returns existing file handle if file already exists in
    /// the database
    pub fn add_from_fs(&mut self, file_name: &str) -> Result<u32, io::Error> {
        if Path::new(file_name).is_relative() {
            let real_path = std::fs::canonicalize(file_name)?;
            let path_str = real_path.to_str().unwrap();

            if let Some(id) = self.file_names.get(&path_str) {
                return Ok(*id);
            }

            if !self.fs_read_access {
                return Err(io::ErrorKind::PermissionDenied.into());
            }

            let source = read_to_string(&path_str)?;
            return self.add(&path_str, &source);
        }

        if let Some(id) = self.file_names.get(file_name) {
            return Ok(*id);
        }

        if !self.fs_read_access {
            return Err(io::ErrorKind::PermissionDenied.into());
        }

        let source = read_to_string(&file_name)?;
        return self.add(file_name, &source);
    }

    pub fn symbol_to_str(&self, symbol: u32) -> &str {
        let cloc = self.names[symbol as usize];
        return self.cloc_to_str(cloc);
    }

    pub fn cloc_to_str(&self, cloc: CodeLoc) -> &str {
        let range: ops::Range<usize> = cloc.into();
        let text = self.files[cloc.file as usize].unwrap()._source;
        return unsafe { str::from_utf8_unchecked(&text.as_bytes()[range]) };
    }

    #[inline]
    pub fn translate_add(&mut self, range: ops::Range<usize>, file: u32) -> u32 {
        let cloc = l(range.start as u32, range.end as u32, file); // TODO check for overflow
        return self.translate_add_cloc(cloc);
    }

    #[inline]
    pub fn translate_add_cloc(&mut self, cloc: CodeLoc) -> u32 {
        let range: ops::Range<usize> = cloc.into();
        let text = self.files[cloc.file as usize].unwrap()._source;
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
        Some(self.files.get(file_id as usize)?.as_ref()?._name)
    }

    fn source(&self, file_id: u32) -> Option<&'a str> {
        Some(self.files.get(file_id as usize)?.as_ref()?._source)
    }

    fn line_index(&self, file_id: u32, byte_index: usize) -> Option<usize> {
        let file = self.files.get(file_id as usize)?.as_ref()?;
        return file.line_index(byte_index);
    }

    fn line_range(&self, file_id: u32, line_index: usize) -> Option<core::ops::Range<usize>> {
        let file = self.files.get(file_id as usize)?.as_ref()?;
        return file.line_range(line_index);
    }
}

#[derive(Debug, Clone, Copy, Serialize)]
pub struct FileDbRef<'a> {
    pub files: &'a [Option<File<'a>>],
    pub symbols: &'a [&'a str],
}

impl<'a> FileDbRef<'a> {
    /// Create a new files database.
    pub fn new(frame: &impl Allocator<'a>, db: &FileDb) -> Self {
        let mut file_sources = Vec::new();

        for file in db.files.iter() {
            let file = if let Some(file) = file {
                let file = File::new_alloc(frame, *file);
                Some(file)
            } else {
                None
            };

            file_sources.push(file);
        }

        let mut symbols = Vec::new();
        for symbol in db.names.iter() {
            let range: ops::Range<usize> = (*symbol).into();
            let file = file_sources[symbol.file as usize].as_ref().unwrap();
            let bytes = &file._source.as_bytes()[range];
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
        Some(self.files.get(file_id as usize)?.as_ref()?._name)
    }

    fn source(&self, file_id: u32) -> Option<&'a str> {
        Some(self.files.get(file_id as usize)?.as_ref()?._source)
    }

    fn line_index(&self, file_id: u32, byte_index: usize) -> Option<usize> {
        let file = self.files.get(file_id as usize)?.as_ref()?;
        return file.line_index(byte_index);
    }

    fn line_range(&self, file_id: u32, line_index: usize) -> Option<core::ops::Range<usize>> {
        let file = self.files.get(file_id as usize)?.as_ref()?;
        return file.line_range(line_index);
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
            panic!("got relative file path {}", path);
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
