use crate::ast::*;
use crate::interpreter::*;
use crate::type_checker::*;
use crate::util::*;
use std::collections::HashMap;

pub struct ASMFunc<'a> {
    pub func_type: TCFuncType<'a>,
    pub func_header: Option<u32>,
}

pub struct Assembler<'a> {
    pub opcodes: Vec<TaggedOpcode>,
    pub functions: HashMap<u32, ASMFunc<'a>>, // ranges here point into opcodes buffer
}

impl<'a> Assembler<'a> {
    pub fn new() -> Self {
        Self {
            opcodes: Vec::new(),
            functions: HashMap::new(),
        }
    }

    pub fn add_file(&mut self, types: TypeEnv<'a>) -> Result<(), Error> {
        for (ident, func) in types.functions.into_iter() {
            self.add_function(ident, func)?;
        }

        return Ok(());
    }

    pub fn add_function(&mut self, ident: u32, func: TCFunc<'a>) -> Result<(), Error> {
        return Err(error!("unimplemented"));
    }

    // pub fn assemble(self) -> Program {
    //     todo!()
    // }
}
