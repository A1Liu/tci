/*!
Cuts the AST into basic blocks which do not have any branches.
 */

use crate::api::*;
use core::ops::Range;

pub struct BBlockCuts {
    pub funcs: Vec<BBFunc>,
    pub blocks: Vec<BBlock>,
    pub decl_blocks: Vec<BBDeclBlock>,
}

pub struct BBFunc {
    pub parent: u32,
    pub blocks_range: Range<usize>,
}

pub struct BBDeclBlock {
    pub parent: u32,
    pub range: Range<usize>,
}

pub struct BBlock {
    pub range: Range<usize>,
}

pub fn create_block_cuts(ast: &AstNodeVec) -> BBlockCuts {
    let funcs = Vec::new();
    let blocks = Vec::new();

    let item_ranges = ast.collect_to_parents_range(0..ast.len(), |node| {
        let mut current = node;

        loop {
            let parent = *current.parent;
            if parent == *current.id {
                // We're at the global index
                break;
            }

            current = ast.index(parent as usize);
        }

        return Some(*current.id);
    });

    let (func_decls, decl_blocks): (Vec<_>, Vec<_>) =
        item_ranges
            .into_par_iter()
            .partition_map(|(parent, range)| match ast.kind[parent as usize] {
                AstNodeKind::FunctionDefinition(_) => Either::Left((parent, range)),

                // TODO: multiple variables can be declared in a single in a single `AstDeclaration`
                AstNodeKind::Declaration(_) => Either::Right(BBDeclBlock { parent, range }),

                x => panic!("unexpected node type: {:?}", x),
            });

    // Collect the data for the func_decls

    return BBlockCuts {
        funcs,
        blocks,
        decl_blocks,
    };
}
