use core::ops::Range;

use crate::api::*;

pub struct ByKindAst<'a> {
    pub ast: &'a mut AstNodeVec,
    pub by_kind: HashMap<AstNodeKind, Range<usize>>,
    pub by_kind_in_order: Vec<(AstNodeKind, Range<usize>)>,
}

impl<'a> ByKindAst<'a> {
    pub fn new(ast: &'a mut AstNodeVec) -> Self {
        Self::sort_by_kind(ast);

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

    // NOTE: Assumes that the input was originally sorted by post-order
    fn sort_by_kind(ast: &mut AstNodeVec) {
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
    }
}

impl<'a> Drop for ByKindAst<'a> {
    fn drop(&mut self) {
        sort_by_postorder(self.ast);
    }
}

pub fn sort_by_postorder(ast: &mut AstNodeVec) {
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
