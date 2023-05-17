/*!
Validate expressions.

- Validate expressions and their children
- Validate return statements
 */

use super::cut_blocks::BBlockCuts;
use crate::api::*;

// TODO: take in blocks, and run in series for each block, traversing using postorder semantics
pub fn validate_exprs(
    ast: &mut AstNodeVec,
    scopes: &Scopes,
    blocks: &BBlockCuts,
) -> Result<(), Vec<Error>> {
    let kind = &ast.kind;

    let block_ranges = ast::split_by_ranges(
        &mut ast.ty_id,
        blocks.blocks.iter().map(|b| b.range.clone()).collect(),
    );

    let errors: Vec<_> = block_ranges
        .into_par_iter()
        .flat_map(|(start_index, type_slots)| {
            let range = start_index..(start_index + type_slots.len());
            let kinds = &ast.kind[range.clone()];
            let starts = &ast.start[range.clone()];
            let data = &ast.data[range.clone()];

            let mut errors = Vec::new();

            let expr_kinds = kinds
                .iter()
                .enumerate()
                // We only care about typing expressions, but the block might contain other stuff as well
                .filter_map(|(index, &kind)| match kind {
                    AstNodeKind::Expr(e) => Some((index, e)),
                    _ => None,
                });

            for (index, expr) in expr_kinds {
                let node_index = index + start_index;
                type_slots[index] = match expr {
                    AstExpr::IntLit(_) => TyId::S32,
                    AstExpr::Ident(i) => {
                        let sym = i.read(data[index]);
                        let scope = scopes.scope_for_id(node_index as u32);

                        match scopes.search_for_symbol(scope, sym) {
                            None => {
                                errors.push(error!(Todo, "identifier not found", starts[index]));
                                continue;
                            }

                            Some(id) => {
                                // found symbol
                            }
                        }

                        TyId::S32
                    }

                    b => panic!("OOOF {:?}", b),
                };
            }

            return errors.into_par_iter();
        })
        .collect();

    return Ok(());
}

struct ExprStackEntry {
    id: u32,
    ty_id: TyId,
    l_value: Option<LValue>,
}

enum LValue {
    Var,
    Ptr,
}
