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
    pub opcodes: Vec<TaggedOpcode>,
    pub func_linkage: HashMap<LinkName, ASMFunc>,
    pub data: VarBuffer,
    pub symbols: Vec<RuntimeVar>,
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            opcodes: Vec::new(),
            data: VarBuffer::new(),
            func_linkage: HashMap::new(),
            symbols: Vec::new(),
        }
    }

    pub fn add_file(&mut self, file: u32, tu: TranslationUnit) -> Result<(), Error> {
        // Add function return sizes
        let mut defns = Vec::new();
        for (ident, tc_func) in tu.functions.into_iter() {
            let link_name = if tc_func.is_static {
                LinkName::new_static(ident, file)
            } else {
                LinkName::new(ident)
            };

            let mut prev = match self.func_linkage.entry(link_name) {
                Entry::Occupied(o) => o,
                Entry::Vacant(v) => {
                    v.insert(ASMFunc {
                        func_type: tc_func.func_type,
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

        // Add function definitions
        for (link_name, defn) in defns.into_iter() {}

        return Ok(());
    }
}

pub fn func_decl_mismatch(original: CodeLoc, new: CodeLoc) -> Error {
    return error!(
        "function declaration type doesn't match previous declaration",
        original, "original declaration here", new, "second declaration here"
    );
}
