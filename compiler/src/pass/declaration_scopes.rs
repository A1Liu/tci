/*!
Ensures correctness of declarations relative to lexical scope.

This includes:
- Combine declarations into scopes
- Validate that declarations in the same scope do not interfere with each other
  - This includes variables but also structs, typedefs, etc.
- Validate that identifier expressions have an associated declaration
- Resolve typedef references and struct references

It may also eventually have some other responsibilities, like:
- Checking variable lifetimes
- Checking and tracking variable access patterns
- Assigning declaration slots/register
 */

use crate::api::*;

// validate declarators relative to their scopes
//          -> produce scopes
// validate identifiers
//          -> produce types for the identifiers
//          -> track which identifiers are pointer-referenced, and when each declaration is last used
// produce global symbols?
fn validate_scopes(ast: &mut ByKindAst) -> Result<(), Error> {
    // collapse declarators into their scopes
    // Probably decide on slots, and do lifetime analysis?
    // NOTE: the statements in a function are a child of a block node,
    // and then that block node is the child of the function definition node

    return Ok(());
}
