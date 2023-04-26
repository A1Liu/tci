use std::collections::HashMap;

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

    pub fn to_str(&self, id: u32) -> Option<&str> {
        return self.to_name.get(id as usize).map(|a| &**a);
    }
}
