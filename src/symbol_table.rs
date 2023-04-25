use std::collections::HashMap;

#[repr(u32)]
#[derive(Hash, Eq, PartialEq, Clone, Copy)]
#[non_exhaustive]
pub enum Symbol {
    Main = 0,

    MacroDefined,

    BuiltinPush,
    BuiltinOp,

    VaCurrent,
    BuiltinVaStart,
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
    to_symbol: HashMap<String, u32>,
    to_name: Vec<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut new_self = Self {
            to_symbol: HashMap::new(),
            to_name: Vec::new(),
        };

        new_self.add_str("main");

        new_self.add_str("defined");

        new_self.add_str("__tci_builtin_push");
        new_self.add_str("__tci_builtin_op");

        new_self.add_str("__tci_va_current");
        new_self.add_str("__builtin_va_start");

        new_self
    }

    pub fn add_str(&mut self, s: &str) -> u32 {
        if let Some(id) = self.to_symbol.get(s) {
            return *id;
        }

        let id = self.to_name.len() as u32;
        self.to_symbol.insert(s.to_string(), id);
        self.to_name.push(s.to_string());
        return id;
    }

    pub fn from_str(&self, s: &str) -> Option<u32> {
        self.to_symbol.get(s).map(|id| *id)
    }

    pub fn to_str(&self, id: u32) -> Option<&str> {
        return self.to_name.get(id as usize).map(|a| &**a);
    }
}
