use crate::api::*;

#[derive(Debug, Clone)]
pub struct File {
    pub id: u32,
    pub name: String,
    pub source: String,
}

pub struct FileDb {
    pub names: HashMap<String, u32>,
    pub files: Vec<File>,
}

impl FileDb {
    #[inline]
    pub fn new() -> Self {
        let new_self = Self {
            files: Vec::new(),
            names: HashMap::new(),
        };

        new_self
    }

    /// Add a file to the database, returning the handle that can be used to
    /// refer to it again. Errors if the file already exists in the database.
    pub fn add_file(&mut self, name: String, source: String) -> Result<u32, &'static str> {
        if let Some(id) = self.names.get(&*name) {
            return Err("already exists");
        }

        let id = self.files.len() as u32;
        let file = File { id, name, source };
        self.names.insert(file.name.clone(), id);
        self.files.push(file);

        return Ok(id);
    }

    pub fn resolve_include(&self, include: &str, file: u32) -> Result<u32, &'static str> {
        if !include.starts_with("/") {
            let or_else = || -> &'static str { "not found" };
            let mut path =
                parent_if_file(&*self.files.get(file as usize).ok_or_else(or_else)?.name)
                    .to_string();
            if !path.ends_with("/") && path != "" {
                path.push_str("/");
            }
            path.push_str(include);

            if let Some(id) = self.names.get(&*path) {
                return Ok(*id);
            }

            return Err("not found");
        }

        if let Some(id) = self.names.get(include) {
            return Ok(*id);
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

#[derive(Hash, Eq, PartialEq, Clone, Copy)]
#[non_exhaustive]
#[repr(u32)]
pub enum Symbol {
    Include = 0,

    Defined,
    Main,

    BuiltinPush,
    BuiltinOp,

    VaCurrent,
    BuiltinVaStart,

    NullSymbol = !0,
}

impl Symbol {
    fn from_u32(id: u32) -> Self {
        return unsafe { core::mem::transmute(id) };
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

        let id: u32 = self.to_name.len() as u32;

        // I can't figure out right now what exactly is making the Symbol enum not convert from integers.
        // This works for now.
        //              - Albert Liu, April 25, 2023 Tue 20:07
        let id: Symbol = unsafe { std::mem::transmute(id) };

        self.to_symbol.insert(s.to_string(), id);
        self.to_name.push(s.to_string());
        return id;
    }

    pub fn from_str(&self, s: &str) -> Option<Symbol> {
        self.to_symbol.get(s).map(|id| *id)
    }

    pub fn to_str(&self, id: Symbol) -> Option<&str> {
        return self.to_name.get(id as usize).map(|a| &**a);
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
