/*!
Validate expressions.

- Validate expressions and their children
- Validate return statements
 */

use super::cut_blocks::BBlockCuts;
use crate::api::*;

// TODO: take in blocks, and run in series for each block, traversing using postorder semantics
pub fn validate_exprs(ast: &mut AstNodeVec, blocks: &BBlockCuts) -> Result<(), Vec<Error>> {
    let kind = &ast.kind;

    let block_ranges = ast::split_by_ranges(
        &mut ast.ty_id,
        blocks.blocks.iter().map(|b| b.range.clone()).collect(),
    );

    block_ranges
        .into_par_iter()
        .for_each(|(start_index, type_slots)| {
            let kinds = &ast.kind[start_index..(start_index + type_slots.len())];

            let expr_kinds = kinds
                .iter()
                .enumerate()
                // We only care about typing expressions, but the block might contain other stuff as well
                .filter_map(|(index, &kind)| match kind {
                    AstNodeKind::Expr(e) => Some((index, e)),
                    _ => None,
                });

            for (index, expr) in expr_kinds {
                type_slots[index] = match expr {
                    AstExpr::IntLit(_) => TyId::S32,
                    _ => panic!("OOOF"),
                };
            }
        });

    return Ok(());
}
