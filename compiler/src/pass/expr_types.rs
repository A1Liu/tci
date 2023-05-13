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

    let ranges = ast::split_by_ranges(
        &mut ast.ty_id,
        blocks.blocks.iter().map(|b| b.range.clone()).collect(),
    );

    ranges
        .into_par_iter()
        .for_each(|(start_index, type_slots)| {
            let kinds = &ast.kind[start_index..(start_index + type_slots.len())];
            for (index, &kind) in kinds.iter().enumerate() {
                let e = match kind {
                    AstNodeKind::Expr(e) => e,
                    _ => continue,
                };

                type_slots[index] = match e {
                    AstExpr::IntLit(_) => TyId::S32,
                    _ => panic!("OOOF"),
                };
            }
        });

    // let v: Vec<_> = ast
    //     .ty_id
    //     .par_iter_mut()
    //     .enumerate()
    //     .filter_map(|(id, ty_slot)| {
    //         if let AstNodeKind::Expr(e) = kind[id] {
    //             return Some((id, e, ty_slot));
    //         }

    //         return None;
    //     })
    //     .map(|(id, e, ty_slot)| {
    //         *ty_slot = match e {
    //             AstExpr::IntLit(_) => TyId::S32,
    //             _ => panic!("OOOF"),
    //         };
    //     })
    //     .collect();

    return Ok(());
}
