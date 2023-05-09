/*!
Passes over the AST to validate and transform it.
 */

use crate::api::*;
use core::ops::Range;

pub mod old_declaration_scopes;
pub mod old_declaration_types;
pub mod old_expr_types;
pub mod types;

// TODO: how do we do node insertions and node deletions?

pub struct ByKindAst<'a> {
    pub nodes: &'a mut AstNodeVec,
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
            nodes: ast,
            by_kind,
            by_kind_in_order,
        };
    }

    // NOTE: Assumes that the input was originally sorted by post-order
    fn sort_by_kind(ast: &mut AstNodeVec) {
        // Sort by kind,height,post_order
        ast.as_mut_slice().sort_by(|a, b| {
            let kind_cmp = a.kind.cmp(b.kind);
            let order_cmp = a.post_order.cmp(b.post_order);

            match (kind_cmp, order_cmp) {
                (core::cmp::Ordering::Equal, x) => return x,
                (x, _) => return x,
            }
        });

        ast.rebuild_ids();
    }

    pub fn matching_range(
        &self,
        matcher: impl for<'b> Fn(&'b AstNodeKind) -> bool,
    ) -> core::ops::Range<usize> {
        let mut final_range = None;
        for (kind, range) in &self.by_kind_in_order {
            if !matcher(kind) {
                continue;
            }

            let prev = match final_range {
                Some(prev) => prev,
                None => {
                    final_range = Some(range.clone());
                    continue;
                }
            };

            if range.end == prev.start {
                final_range = Some(range.start..prev.end);
                continue;
            }

            if prev.end == range.start {
                final_range = Some(prev.start..range.end);
                continue;
            }

            panic!(
                "matcher matched against ranges that were not consecutive {:?} {:?}",
                range, prev
            );
        }

        return final_range.unwrap_or(0..0);
    }
}

impl<'a> Drop for ByKindAst<'a> {
    fn drop(&mut self) {
        sort_by_postorder(self.nodes);
    }
}

pub fn sort_by_postorder(ast: &mut AstNodeVec) {
    // Sort by post_order
    ast.as_mut_slice().sort_by_key(|r| *r.post_order);

    ast.rebuild_ids();
}
