use crate::buckets::*;
use crate::interpreter::*;
use crate::new_tc_ast::*;
use crate::runtime::*;
use crate::util::*;

#[derive(Debug)]
pub struct ASMFunc {
    pub func_type: TCFuncType,
    pub decl_loc: CodeLoc,
    pub func_header: Option<(u32, CodeLoc)>, // first u32 points into opcodes buffer
}

pub struct Assembler {
    pub buckets: BucketListFactory,
    pub opcodes: Vec<TaggedOpcode>,
    pub func_linkage: HashMap<LinkName, ASMFunc>,
    pub data: VarBuffer,
    pub symbols: Vec<RuntimeVar>,
}

impl Drop for Assembler {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            buckets: BucketListFactory::new(),
            opcodes: Vec::new(),
            data: VarBuffer::new(),
            func_linkage: HashMap::new(),
            symbols: Vec::new(),
        }
    }

    pub fn add_file(&mut self, file: u32, mut tu: TranslationUnit) -> Result<(), Error> {
        // Add function return sizes

        let mut link_names = HashMap::new();
        let mut defns = Vec::new();

        for (ident, tc_func) in std::mem::replace(&mut tu.functions, HashMap::new()) {
            let link_name = if tc_func.is_static {
                LinkName::new_static(ident, file)
            } else {
                LinkName::new(ident)
            };

            link_names.insert(ident, link_name);

            let prev = match self.func_linkage.entry(link_name) {
                Entry::Occupied(o) => o,
                Entry::Vacant(v) => {
                    v.insert(ASMFunc {
                        func_type: tc_func.func_type.clone_into_alloc(&*self.buckets),
                        decl_loc: tc_func.decl_loc,
                        func_header: None,
                    });

                    if let Some(defn) = tc_func.defn {
                        defns.push((link_name, defn));
                    }

                    continue;
                }
            };

            if prev.get().func_type != tc_func.func_type {
                let error = func_decl_mismatch(prev.get().decl_loc, tc_func.decl_loc);
                return Err(error);
            }

            if let Some(defn) = tc_func.defn {
                defns.push((link_name, defn));
            }
        }

        for (link_name, defn) in defns {
            let header = &mut self.func_linkage.get_mut(&link_name).unwrap().func_header;
            if let Some((_, defn_loc)) = header.as_ref() {
                return Err(func_redef(*defn_loc, defn.loc));
            }

            let opcode = self.opcodes.len() as u32;

            self.opcodes.push(TaggedOpcode {
                op: Opcode::Func(link_name),
                loc: defn.loc,
            });

            self.add_function(&link_names, &defn);
            let header = &mut self.func_linkage.get_mut(&link_name).unwrap().func_header;
            *header = Some((opcode, defn.loc));
        }

        return Ok(());
    }

    pub fn add_function(&mut self, link_names: &HashMap<u32, LinkName>, defn: &TCFuncDefn) {
        if defn.ops.len() < 2 {
            unreachable!()
        }

        if let TCOpcodeKind::ScopeBegin(hash_ref, scope_end) = defn.ops[0].kind {
            let keys: Vec<_> = hash_ref.iter().collect();
        }
    }
}

pub fn func_decl_mismatch(original: CodeLoc, new: CodeLoc) -> Error {
    return error!(
        "function declaration type doesn't match previous declaration",
        original, "original declaration here", new, "second declaration here"
    );
}

pub fn func_redef(original: CodeLoc, redef: CodeLoc) -> Error {
    return error!(
        "redefinition of function",
        original, "original definition here", redef, "second definition here"
    );
}
