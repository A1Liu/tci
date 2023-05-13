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
    // TODO: add endpoint thingy (e.g. does the block end with a conditional jump, or an unconditional jump, or a return, or etc.)
}

pub fn create_basic_blocks(ast: &AstNodeVec) -> BBlockCuts {
    let item_ranges = ast.collect_to_parents_range(0..ast.len(), |node| {
        let mut current = node;

        if *current.parent == *current.id {
            // Ignore the root node of the global object
            return None;
        }

        let mut was_decl = true;

        loop {
            let current_is_decl = match *current.kind {
                AstNodeKind::Declarator(_) => true,
                AstNodeKind::Declaration(_) => true,
                AstNodeKind::FunctionDefinition(_) => true,
                AstNodeKind::Specifier(_) => true,
                AstNodeKind::DerivedDeclarator(_) => true,
                AstNodeKind::ParamDecl(_) => true,

                AstNodeKind::Statement(_) => false,
                AstNodeKind::Expr(_) => false,
            };

            let parent = *current.parent;
            if parent == *current.id {
                if was_decl {
                    return None;
                }

                // We're at the global index
                break;
            }

            was_decl = current_is_decl && was_decl;

            current = ast.index(parent as usize);
        }

        return Some(*current.id);
    });

    let (func_decls, decl_blocks): (Vec<_>, Vec<_>) =
        item_ranges
            .into_par_iter()
            .partition_map(|(parent, range)| {
                match ast.kind[parent as usize] {
                    // TODO: multiple variables can be declared in a single in a single `AstDeclaration`
                    AstNodeKind::Declaration(_) => {
                        return Either::Right(BBDeclBlock { parent, range });
                    }

                    AstNodeKind::FunctionDefinition(_) => {}

                    x => panic!("unexpected node type: {:?}", x),
                }

                /*
                Functions:
                Loop up the parent chain, finding the nearest control flow point;
                 Group by the node right beneath the control flow node, these are the
                 basic blocks (at least, some of them)

                 E.g.

                 If
                    -> Cond
                        -> Boolean(true)

                Group by the `Cond` node, not the `If` node
                Use parent nodes (e.g. the `If`) to create block graph
                    - Stuff like if statements just need their bits joined together
                    - Blocks need to be split up if there's a control flow construct
                      in the middle
                    - Probably want to use a BTree here to track ranges of each block
                      and do lookups against ranges
                    - Blocks have the type of their last node (I think this will work, because of post-order)
                */

                let blocks = ast.collect_to_parents_range(range, |node| {
                    /*
                    Loop up the parent chain, finding the nearest control flow point;
                     Group by the node right beneath the control flow node, these are the
                     basic blocks (at least, some of them)
                     */

                    for (node, parent) in ast.parent_chain(*node.id as usize).tuple_windows() {
                        match *parent.kind {
                            AstNodeKind::FunctionDefinition(_) => return Some(*node.id),

                            // TODO: handle control flow statements
                            _ => continue,
                        }
                    }

                    unreachable!();
                });

                // TODO: create block graph

                struct BBlockInternal {
                    id: usize,
                    range: Range<usize>,
                }

                struct FuncInternal {
                    id: u32,
                    blocks: Vec<BBlockInternal>,
                }

                let blocks: Vec<_> = blocks
                    .into_iter()
                    .enumerate()
                    .map(|(id, (_, range))| BBlockInternal { id, range })
                    .collect();

                return Either::Left(FuncInternal { id: parent, blocks });
            });

    let mut funcs = Vec::new();
    let mut blocks = Vec::new();

    for func in func_decls {
        let blocks_begin = blocks.len();

        for block in func.blocks {
            blocks.push(BBlock { range: block.range });
        }

        funcs.push(BBFunc {
            parent: func.id,
            blocks_range: blocks_begin..blocks.len(),
        })
    }

    // Collect the data for the func_decls

    return BBlockCuts {
        funcs,
        blocks,
        decl_blocks,
    };
}
