use crate::ast::*;
use crate::interpreter::*;
use crate::util::*;
use std::collections::HashMap;

pub struct Assembler {
    opcodes: Vec<TaggedOpcode>,
    functions: HashMap<u32, Range>, // ranges here point into opcodes buffer
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            opcodes: Vec::new(),
            functions: HashMap::new(),
        }
    }
}
