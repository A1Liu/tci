/*!
Passes over the AST to validate and transform it.
 */
use crate::api::*;

pub mod ast_structure;
pub mod block_cuts;
pub mod declaration_scopes;
pub mod declaration_types;
pub mod expr_types;
pub mod types;

pub fn validate(
    ast: &mut AstNodeVec,
    ty_db: &TyDb,
    symbols: &SymbolTable,
) -> Result<(), Vec<Error>> {
    ast_structure::validate_structure(ast)?;

    let scopes = declaration_scopes::validate_scopes(ast, symbols)?;
    declaration_types::validate_declarations(ast, ty_db)?;
    expr_types::validate_exprs(ast)?;

    return Ok(());
}
