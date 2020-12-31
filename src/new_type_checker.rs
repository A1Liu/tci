use crate::buckets::*;
use crate::filedb::*;
use crate::new_ast::*;
use crate::new_tc_ast::*;
use crate::new_tc_structs::*;
use crate::util::*;

// pub fn check_declarator(decl_specs: &[DeclarationSpecifier], decl: &Declarator) -> (TCType, n32){}

pub fn check_tree(tree: &[GlobalStatement]) -> Result<TranslationUnit, Error> {
    let mut globals = GlobalEnv::new();

    for decl in tree {
        match decl.kind {
            GlobalStatementKind::Declaration(decl) => {
                check_declaration(&globals, decl)?;
            }
            GlobalStatementKind::FunctionDefinition(func) => {
                check_function_defn(&globals, func)?;
            }
            GlobalStatementKind::Pragma(pragma) => {
                if pragma == "tci enable_builtins" {
                    globals.builtins_enabled = true;
                }
            }
        }
    }

    return Ok(globals.tu);
}

pub fn check_declaration(globals: &GlobalEnv, declaration: Declaration) -> Result<(), Error> {
    return Ok(());
}

pub fn check_function_defn(globals: &GlobalEnv, defn: FunctionDefinition) -> Result<(), Error> {
    return Ok(());
}
