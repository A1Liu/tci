/*!
Validate expressions.

- Validate expressions and their children
- Validate return statements
 */

use crate::api::*;

// TODO: take in blocks, and run in series for each block
pub fn validate_exprs(ast: &mut AstNodeVec) -> Result<(), Vec<Error>> {
    let kind = &ast.kind;

    let v: Vec<_> = ast
        .ty_id
        .par_iter_mut()
        .enumerate()
        .filter_map(|(id, ty_slot)| {
            if let AstNodeKind::Expr(e) = kind[id] {
                return Some((id, e, ty_slot));
            }

            return None;
        })
        .map(|(id, e, ty_slot)| {
            *ty_slot = match e {
                AstExpr::IntLit(_) => TyId::S32,
                _ => panic!("OOOF"),
            };
        })
        .collect();

    return Ok(());
}
