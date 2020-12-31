use crate::buckets::*;
use crate::filedb::*;
use crate::new_ast::*;
use crate::new_tc_ast::*;
use crate::new_tc_structs::*;
use crate::util::*;

// pub fn check_declarator(decl_specs: &[DeclarationSpecifier], decl: &Declarator) -> (TCType, n32) {}

pub fn check_tree(tree: &[GlobalStatement]) {
    let typedefs: HashMap<u32, TCTypedef> = HashMap::new();
    let tu = TranslationUnit::new();
}
