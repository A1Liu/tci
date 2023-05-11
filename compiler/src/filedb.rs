use crate::api::*;
use codespan_reporting::files::{line_starts, Error as SpanErr, Files};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum FileType {
    User,
    System,
}

#[derive(Debug, Clone)]
pub struct File {
    pub id: u32,
    pub ty: FileType,
    pub name: String,
    pub source: String,
    pub line_starts: Vec<usize>,
}

struct FileStatic {
    pub name: &'static str,
    pub source: &'static str,
}

macro_rules! sys {
    (header $file:literal) => {
        FileStatic {
            name: $file,
            source: include_str!(concat!("../header/", $file)),
        }
    };
    (lib $file:literal) => {
        FileStatic {
            name: $file,
            source: include_str!(concat!("../libc/", $file)),
        }
    };
}

const SYS_HEADERS: &[FileStatic] = &[
    sys!(header "stdio.h"),
    sys!(header "stdbool.h"),
    sys!(header "ctype.h"),
];

const SYS_LIB: &[FileStatic] = &[
    sys!(lib "stdlib.c"),
    sys!(lib "string.c"),
    sys!(lib "strings.c"),
];

pub struct FileDb {
    pub names: HashMap<(FileType, String), u32>,
    pub files: Vec<File>,
}

impl FileDb {
    #[inline]
    pub fn new() -> Self {
        let mut new_self = Self {
            files: Vec::new(),
            names: HashMap::new(),
        };

        let ty = FileType::System;
        for file in SYS_HEADERS {
            let name = file.name.to_string();
            let source = file.source.to_string();
            let id = new_self.files.len() as u32;

            new_self.names.insert((ty, name.clone()), id);
            new_self.files.push(File {
                id,
                ty,
                name,
                line_starts: line_starts(&source).collect(),
                source,
            });
        }

        new_self
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again. Errors if the file already exists in the database.
    pub fn add_file(&mut self, name: String, source: String) -> Result<u32, &'static str> {
        let tup = (FileType::User, name);
        if let Some(id) = self.names.get(&tup) {
            return Err("already exists");
        }

        let name = tup.1;
        let id = self.files.len() as u32;
        let file = File {
            id,
            ty: FileType::User,
            name,
            line_starts: line_starts(&source).collect(),
            source,
        };
        self.names.insert((FileType::User, file.name.clone()), id);
        self.files.push(file);

        return Ok(id);
    }

    pub fn resolve_include(&self, include: &str, file: u32) -> Result<&File, &'static str> {
        if !include.starts_with("/") {
            let or_else = || -> &'static str { "not found" };
            let mut path =
                parent_if_file(&*self.files.get(file as usize).ok_or_else(or_else)?.name)
                    .to_string();
            if !path.ends_with("/") && path != "" {
                path.push_str("/");
            }
            path.push_str(include);

            if let Some(id) = self.names.get(&(FileType::User, path)) {
                return Ok(&self.files[*id as usize]);
            }

            return Err("not found");
        }

        if let Some(id) = self.names.get(&(FileType::User, include.to_string())) {
            return Ok(&self.files[*id as usize]);
        }

        return Err("not found");
    }

    pub fn resolve_system_include(&self, include: &str, file: u32) -> Result<&File, &'static str> {
        if let Some(id) = self.names.get(&(FileType::System, include.to_string())) {
            return Ok(&self.files[*id as usize]);
        }

        return Err("not found");
    }

    pub fn name(&self, file_id: u32) -> Option<&str> {
        Some(&*self.files.get(file_id as usize)?.name)
    }

    pub fn source(&self, file_id: u32) -> Option<&str> {
        Some(&*self.files.get(file_id as usize)?.source)
    }
}

/// Return the starting byte index of the line with the specified line index.
/// Convenience method that already generates errors if necessary.
///
/// Copied from codespan_reporting
fn get_line_start(len: usize, line_starts: &[usize], line_index: usize) -> Result<usize, SpanErr> {
    use std::cmp::Ordering;

    match line_index.cmp(&line_starts.len()) {
        Ordering::Less => Ok(line_starts
            .get(line_index)
            .cloned()
            .expect("failed despite previous check")),
        Ordering::Equal => Ok(len),
        Ordering::Greater => Err(SpanErr::LineTooLarge {
            given: line_index,
            max: line_starts.len() - 1,
        }),
    }
}

impl<'a> Files<'a> for FileDb {
    type FileId = u32;

    type Name = &'a str;

    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, SpanErr> {
        let f = self.files.get(id as usize).ok_or(SpanErr::FileMissing)?;
        return Ok(&f.name);
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, SpanErr> {
        let f = self.files.get(id as usize).ok_or(SpanErr::FileMissing)?;
        return Ok(&f.source);
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, SpanErr> {
        let f = self.files.get(id as usize).ok_or(SpanErr::FileMissing)?;

        return Ok(f
            .line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1));
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, SpanErr> {
        let f = self.files.get(id as usize).ok_or(SpanErr::FileMissing)?;

        let line_start = get_line_start(f.source.len(), &f.line_starts, line_index)?;
        let next_line_start = get_line_start(f.source.len(), &f.line_starts, line_index + 1)?;

        return Ok(line_start..next_line_start);
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy, Serialize, Deserialize)]
#[repr(transparent)]
pub struct Symbol(u32);

#[allow(non_upper_case_globals)]
impl Symbol {
    // TODO: Make this a struct instead of an enum; Rust really really really doesn't
    // like enums that at runtime can have a different value.
    pub const Include: Symbol = Symbol(0);

    pub const Defined: Symbol = Symbol(1);
    pub const Main: Symbol = Symbol(2);

    pub const BuiltinPush: Symbol = Symbol(3);
    pub const BuiltinOp: Symbol = Symbol(4);

    pub const VaCurrent: Symbol = Symbol(5);
    pub const BuiltinVaStart: Symbol = Symbol(6);

    pub const NullSymbol: Symbol = Symbol(!0);
}

impl Into<u64> for Symbol {
    fn into(self) -> u64 {
        return self.0 as u64;
    }
}

impl From<u64> for Symbol {
    fn from(value: u64) -> Self {
        Self(value as u32)
    }
}

// TODO: This scatters the string values all over the heap. We can do better,
// by doing one of the following:
//
// - Use the hash_brown::raw::RawTable directly to control hashing and equality behavior
// - Use thread_local! to make the byte store accessible during the deref check
// - Use Rc and RefCell like some existing implementations out there
//
// All of this is very annoying though.
pub struct SymbolTable {
    to_symbol: HashMap<String, Symbol>,
    to_name: Vec<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut new_self = Self {
            to_symbol: HashMap::new(),
            to_name: Vec::new(),
        };

        new_self.add_str("include");
        new_self.add_str("defined");

        new_self.add_str("main");

        new_self.add_str("__tci_builtin_push");
        new_self.add_str("__tci_builtin_op");

        new_self.add_str("__tci_va_current");
        new_self.add_str("__builtin_va_start");

        new_self
    }

    pub fn add_str(&mut self, s: &str) -> Symbol {
        if let Some(id) = self.to_symbol.get(s) {
            return *id;
        }

        let id = Symbol(self.to_name.len() as u32);

        self.to_symbol.insert(s.to_string(), id);
        self.to_name.push(s.to_string());
        return id;
    }

    pub fn from_str(&self, s: &str) -> Option<Symbol> {
        self.to_symbol.get(s).map(|id| *id)
    }

    pub fn to_str(&self, id: Symbol) -> Option<&str> {
        return self.to_name.get(id.0 as usize).map(|a| &**a);
    }
}

pub fn parent_if_file<'a>(path: &'a str) -> &'a str {
    let bytes = path.as_bytes();
    let mut idx = bytes.len() - 1;
    while bytes[idx] != b'/' {
        if idx == 0 {
            return ""; // idk man this works
        }
        idx -= 1;
    }

    unsafe { core::str::from_utf8_unchecked(&bytes[..(idx + 1)]) }
}
