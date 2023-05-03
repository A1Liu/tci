use core::ops::Range;

use crate::api::*;

pub struct ByKindAst<'a> {
    pub ast: &'a mut AstNodeVec,
    pub by_kind: HashMap<AstNodeKind, Range<usize>>,
    pub by_kind_in_order: Vec<(AstNodeKind, Range<usize>)>,
}

pub fn sort_by_kind(ast: &mut AstNodeVec) -> ByKindAst {
    let mut indices = Vec::with_capacity(ast.len());
    for _ in 0..ast.len() {
        indices.push(u32::MAX);
    }

    // TODO: Sort by kind,post_order

    for (index, &order) in ast.post_order.iter().enumerate() {
        indices[order as usize] = index as u32;
    }

    // Rebuild parent indices
    for parent in &mut ast.parent {
        *parent = indices[*parent as usize];
    }

    let mut by_kind = HashMap::new();
    let mut by_kind_in_order = Vec::new();

    let mut prev = *ast.index(0).kind;
    let mut begin: usize = 0;
    let mut index: usize = 0;
    while index < ast.len() {
        let node = ast.index(index);
        let kind = *node.kind;
        if kind == prev {
            index += 1;
            continue;
        }

        if let Some(_) = by_kind.insert(prev, begin..index) {
            panic!("kind is somehow not sorted");
        }

        // This *should* be safe, because we're really just trying to do a series of split_at_mut
        // but for the SOA vec. This doesn't work because the lifetime on the output of
        // AstNodeSliceMut.split_at_mut is the mut borrow of AstNodeSliceMut, instead of taking
        // the AstNodeSliceMut by ownership and inheriting its lifetime. Thus, the result of
        // split_at_mut uses the local borrow's lifetime instead of the lifetime of the original
        // slice, and you can't really do more than one split at mut.
        // Seems this will be fixed soon though.
        by_kind_in_order.push((prev, begin..index));

        begin = index;
        prev = kind;
        index += 1;
    }

    return ByKindAst {
        ast,
        by_kind,
        by_kind_in_order,
    };
}

pub fn sort_to_postorder(ast: &mut AstNodeVec) {
    let mut indices = Vec::with_capacity(ast.len());

    for &order in &ast.post_order {
        indices.push(order);
    }

    // TODO: Sort by post_order

    // Rebuild parent indices
    for parent in &mut ast.parent {
        *parent = indices[*parent as usize];
    }
}

// validate declarations -> produce declaration types
// Function declarations need to have proper derived declarator and etc
// Declaration specifiers need to make sense for the kind of declaration theyre on
pub fn validate_declaration_types(ast: &mut ByKindAst) -> Result<(), Error> {
    return Ok(());
}

// validate declarators relative to their scopes
//          -> produce scopes
// validate identifiers
//          -> produce types for the identifiers
//          -> track which identifiers are pointer-referenced, and when each declaration is last used
// produce global symbols?
pub fn validate_scopes(ast: &mut ByKindAst) -> Result<(), Error> {
    return Ok(());
}
