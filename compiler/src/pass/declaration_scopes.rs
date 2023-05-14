/*!
Ensures correctness of declarations relative to lexical scope.

This includes:
- Validate that declarations in the same scope do not interfere with each other
  - This includes variables but also structs, typedefs, etc.
- Validate that identifier expressions have an associated declaration
 */

use crate::api::*;

// validate declarators relative to their scopes
//          -> produce scopes
// validate identifiers
//          -> produce types for the identifiers
//          -> track which identifiers are pointer-referenced, and when each declaration is last used
// produce global symbols?
pub fn validate_scopes(ast: &mut AstNodeVec, scopes: &Scopes) -> Result<(), Vec<Error>> {
    let errors: Vec<_> = scopes
        .scopes
        .par_iter()
        .flat_map(|(scope, info)| {
            info.duplicates
                .par_iter()
                .map(|&(id, symbol)| error!(Todo, "duplicate identifier", ast.start[id as usize]))
        })
        .collect();

    if errors.len() > 0 {
        return Err(errors);
    }

    return Ok(());
}
