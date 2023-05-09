use crate::api::*;

pub fn validate_exprs(ast: &mut AstNodeVec) -> Result<(), Error> {
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
