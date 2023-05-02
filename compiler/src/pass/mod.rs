use crate::api::*;

pub struct ByKindAst<'a> {
    pub by_kind: HashMap<AstNodeKind, usize>,
    pub by_kind_in_order: Vec<(AstNodeKind, ast::AstNodeSliceMut<'a>)>,
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

        let by_kind_in_order_index = by_kind_in_order.len();
        let prev_slice = ast.slice_mut(begin..index);

        if let Some(_) = by_kind.insert(prev, by_kind_in_order_index) {
            panic!("kind is somehow not sorted");
        }

        // This *should* be safe, because we're really just trying to do a series of split_at_mut
        // but for the SOA vec. This doesn't work because the lifetime on the output of
        // AstNodeSliceMut.split_at_mut is the mut borrow of AstNodeSliceMut, instead of taking
        // the AstNodeSliceMut by ownership and inheriting its lifetime. Thus, the result of
        // split_at_mut uses the local borrow's lifetime instead of the lifetime of the original
        // slice, and you can't really do more than one split at mut.
        // Seems this will be fixed soon though.
        by_kind_in_order.push((prev, unsafe { core::mem::transmute(prev_slice) }));

        begin = index;
        prev = kind;
        index += 1;
    }

    return ByKindAst {
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

pub fn validate_ast(ast: &mut AstNodeVec) -> Result<(), Error> {
    // sort

    let mut id_translate = return Ok(());
}
