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
fn validate_scopes<'a>(
    ast: &mut AstNodeVec,
    symbols: &'a SymbolTable,
) -> Result<Scopes<'a>, Vec<Error>> {
    let scopes = ast.collect_to_parents(0..ast.len(), |node| {
        let kind = match node.kind {
            AstNodeKind::Declarator(d) => d,
            _ => return None,
        };

        let info = node.read_data(kind);
        let mut index = *node.parent;
        loop {
            match ast.kind[index as usize] {
                AstNodeKind::Declaration(_) | AstNodeKind::ParamDecl(_) => break,
                _ => index = ast.parent[index as usize],
            }
        }

        return Some((
            ast.parent[index as usize],
            DeclInfo {
                id: *node.id,
                symbol: info.symbol(),
            },
        ));
    });

    let (scopes, error): (HashMap<_, _>, HashMap<_, _>) =
        scopes.into_par_iter().partition_map(|(scope_id, decls)| {
            let len = decls.len();

            let mut decls_map = HashMap::new();
            let mut errors = Vec::new();
            for decl in decls {
                if let Some(prev_decl) = decls_map.insert(decl.symbol, decl) {
                    errors.push(error!(
                        Todo,
                        "duplicate identifier", ast.start[decl.id as usize]
                    ));
                }
            }

            if errors.len() != 0 {
                return Either::Right((scope_id, errors));
            }

            return Either::Left((scope_id, decls_map));
        });

    if error.len() != 0 {
        return Err(error
            .into_iter()
            .flat_map(|(_, errors)| errors.into_iter())
            .collect());
    }

    // collapse declarators into their scopes
    // Probably decide on slots, and do lifetime analysis?
    // NOTE: the statements in a function are a child of a block node,
    // and then that block node is the child of the function definition node

    return Ok(Scopes {
        symbols,
        scope_tree: scopes,
    });
}
