use crate::new_tc_ast::*;
use crate::util::*;
use std::collections::HashMap;

pub struct GlobalEnv {
    pub typedefs: HashMap<u32, TCTypedef>,
    pub func_types: HashMap<u32, TCFuncType>,
    pub values: HashMap<u32, TCGlobalVar>,
    pub builtins_enabled: Option<u32>,
}

pub struct FuncEnv {
    pub ops: Vec<TCOpcode>,
    pub gotos: Vec<u32>,
    pub next_label: u32,

    // const fields
    pub return_type: TCType,
    pub rtype_loc: CodeLoc,
    pub decl_idx: u32,
}

pub struct LocalTypeEnv {
    pub symbols: HashMap<u32, TCVar>,
    pub cases: Option<HashMap<TCExpr, u32>>,
    pub parent: *const LocalTypeEnv,
    pub decl_idx: i16,
}

impl LocalTypeEnv {
    pub fn new(params: i16) -> Self {
        Self {
            symbols: HashMap::new(),
            cases: None,
            parent: core::ptr::null(),
            decl_idx: -params,
        }
    }

    pub fn child(&self, is_switch: bool) -> Self {
        let cases = if is_switch {
            Some(HashMap::new())
        } else {
            None
        };

        if self.symbols.is_empty() {
            // for the case of chained if-else
            Self {
                symbols: HashMap::new(),
                cases,
                parent: self.parent,
                decl_idx: self.decl_idx,
            }
        } else {
            Self {
                symbols: HashMap::new(),
                cases,
                decl_idx: self.decl_idx,
                parent: self,
            }
        }
    }

    pub fn var(&self, id: u32) -> Option<&TCVar> {
        if let Some(var_type) = self.symbols.get(&id) {
            return Some(var_type);
        }

        if self.parent.is_null() {
            return None;
        }

        return unsafe { &*self.parent }.var(id);
    }

    pub fn add_var(&mut self, ident: u32, tc_value: TCVar) -> Result<(), Error> {
        let tc_loc = tc_value.loc;
        if let Some(var_type) = self.symbols.insert(ident, tc_value) {
            return Err(error!(
                "name redefined in scope",
                var_type.loc, "first declaration defined here", tc_loc, "redecaration defined here"
            ));
        }

        return Ok(());
    }

    pub fn add_static(&mut self, ident: u32, decl_type: TCType, loc: CodeLoc) -> Result<(), Error> {
        let tc_var = TCVar {
            is_static: true,
            decl_type,
            var_offset: self.decl_idx,
            loc,
        };

        self.decl_idx += 1;

        return self.add_var(ident, tc_var);
    }

    pub fn add_local(&mut self, ident: u32, decl_type: TCType, loc: CodeLoc) -> Result<(), Error> {
        let tc_var = TCVar {
            is_static: false,
            decl_type,
            var_offset: self.decl_idx,
            loc,
        };

        self.decl_idx += 1;

        return self.add_var(ident, tc_var);
    }
}
