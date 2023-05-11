/*!
Ensures correctness of declarations relative to lexical scope.

This includes:
- Combine declarations into scopes
- Validate that declarations in the same scope do not interfere with each other
  - This includes variables but also structs, typedefs, etc.
- Validate that identifier expressions have an associated declaration
- Resolve typedef references and struct references

It may also eventually have some other responsibilities, like:
- Checking variable lifetimes
- Checking and tracking variable access patterns
- Assigning declaration slots/register
 */

use rayon::iter::Either;

use crate::api::*;

pub struct Scopes<'a> {
    pub symbols: &'a SymbolTable,
    pub scope_tree: HashMap<u32, HashMap<Symbol, DeclInfo>>,
}

#[derive(Clone, Copy)]
pub struct DeclInfo {
    pub id: u32,
    pub symbol: Symbol,
}

// validate declarators relative to their scopes
//          -> produce scopes
// validate identifiers
//          -> produce types for the identifiers
//          -> track which identifiers are pointer-referenced, and when each declaration is last used
// produce global symbols?
pub fn validate_scopes<'a>(
    ast: &mut AstNodeVec,
    symbols: &'a SymbolTable,
) -> Result<Scopes<'a>, Error> {
    let scope_tree = ast.collect_to_parents(0..ast.len(), |node| {
        let kind = match node.kind {
            AstNodeKind::Declarator(d) => d,
            _ => return None,
        };

        let symbol = node.read_data(kind);
        let mut index = *node.parent;
        loop {
            match ast.kind[index as usize] {
                AstNodeKind::FunctionDefinition(_)
                | AstNodeKind::Declaration(_)
                | AstNodeKind::ParamDecl(_) => break,

                AstNodeKind::DerivedDeclarator(_) => index = ast.parent[index as usize],
                x => panic!("{:?}", x),
            }
        }

        let parent = ast.parent[index as usize];
        let scope_id = if parent == index { !0 } else { parent };
        let info = DeclInfo {
            id: *node.id,
            symbol,
        };

        return Some((scope_id, info));
    });

    let (scope_tree, errors): (HashMap<_, _>, Vec<_>) =
        scope_tree
            .into_par_iter()
            .partition_map(|(scope_id, decls)| {
                let mut decls_map = HashMap::new();
                let (_, errors): ((), Vec<_>) = decls.into_iter().partition_map(|decl| {
                    match decls_map.insert(decl.symbol, decl) {
                        None => Either::Left(()),
                        Some(prev_decl) => Either::Right(error!(
                            Todo,
                            "duplicate identifier", ast.start[decl.id as usize]
                        )),
                    }
                });

                match errors.len() {
                    0 => return Either::Left((scope_id, decls_map)),
                    _ => return Either::Right(errors),
                }
            });

    for e in errors.into_iter().flatten() {
        return Err(e);
    }

    // collapse declarators into their scopes
    // Probably decide on slots, and do lifetime analysis?
    // NOTE: the statements in a function are a child of a block node,
    // and then that block node is the child of the function definition node

    return Ok(Scopes {
        symbols,
        scope_tree,
    });
}
