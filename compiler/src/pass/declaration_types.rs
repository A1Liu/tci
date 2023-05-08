/*!
Checks things about individual declarations.

This includes:
- Validate declaration specifiers - check the declaration specifiers and type specifier on each declaration
- Validate derived declarators - check that type qualifiers make sense for each declarator
- Add types to declarators - Fill the `data` field of `AstDeclarator` with the `TyId`
- Move `AstDeclarator` up the tree - Make the parent of `AstDeclarator` its actual declaration
- Build `TyDb` - Add references to type definitions, function definitions, etc; these references
  won't necessarily be resolved yet, but they will at least exist.
- TODO: validate declaration types - E.g. function definitions need to be functions
- TODO: Validate declaration AST locations - Since the parser is incredibly lenient, we also need to ensure
     that e.g. structs don't have function definitions.

 Some side effects and invariants after this pass completes successfully runs:
 - All `AstDerivedDeclarator` nodes are likely completely useless once this pass runs.
 - `AstDeclarator` nodes now have their `type_id` available via `AstNodeRef::read_data`
 */

use crate::api::*;
use rayon::iter::Either;

#[derive(Default)]
struct SpecifierTracker {
    type_specifier: Option<Result<TyId, Error>>,
    has_int: bool,
    has_sign: bool,
}

#[derive(Clone)]
struct Param {
    post_order: u32,
    ty_id: TyId,
}

#[derive(Default, Clone)]
struct Params {
    count: u32,
    collected_params: Vec<Param>,
}

pub fn validate_declarations(ast: &mut ByKindAst, ty_db: &TyDb) -> Result<(), Error> {
    // NOTE: Going to early-return on the first error for now; ideally
    // we can return multiple errors instead though

    let specifier_range = ast.matching_range(|kind| matches!(kind, AstNodeKind::Specifier(_)));
    let specifiers = ast
        .nodes
        .collect_to_parents(specifier_range, |node| match node.kind {
            AstNodeKind::Specifier(k) => Some((*k, *node.id)),
            _ => None,
        });

    // let mut trackers = HashMap::<u32, SpecifierTracker>::new();

    // Build summary of all specifiers for each node with a specifier
    let mapper = |(parent_index, specifiers): (u32, Vec<(AstSpecifier, u32)>)| {
        use AstSpecifier::*;

        let mut tracker = SpecifierTracker::default();

        for (kind, node_index) in specifiers {
            let spec = match &tracker.type_specifier {
                Some(Err(e)) => return (parent_index, tracker),
                Some(Ok(s)) => Some(*s),
                None => None,
            };

            let node = ast.nodes.index(node_index as usize);

            // ensure the combined specifiers are valid for the declaration
            tracker.type_specifier = match (kind, spec.clone()) {
                (Void, Some(_)) => Some(Err(dup_type(*node.start))),
                (Void, None) => Some(Ok(TyId::Void)),

                (Char, Some(TyId::S32 | TyId::U32)) if tracker.has_int => {
                    Some(Err(dup_type(*node.start)))
                }
                (Char, Some(TyId::S32)) => Some(Ok(TyId::S8)),
                (Char, Some(TyId::U32)) => Some(Ok(TyId::U8)),
                (Char, Some(_)) => Some(Err(dup_type(*node.start))),
                (Char, None) => Some(Ok(TyId::S8)),

                (Short, Some(TyId::S32) | None) => Some(Ok(TyId::S16)),
                (Short, Some(TyId::U32)) => Some(Ok(TyId::U16)),
                (Short, Some(_)) => Some(Err(dup_type(*node.start))),

                (Long, Some(TyId::S32) | None) => Some(Ok(TyId::S64)),
                (Long, Some(TyId::U32)) => Some(Ok(TyId::U64)),
                (Long, Some(_)) => Some(Err(dup_type(*node.start))),

                (
                    Int,
                    t @ (Some(TyId::U32 | TyId::S64 | TyId::U64 | TyId::S16 | TyId::U16) | None),
                ) => {
                    if tracker.has_int {
                        Some(Err(dup_type(*node.start)))
                    } else {
                        tracker.has_int = true;
                        Some(Ok(t.unwrap_or(TyId::S32)))
                    }
                }
                (Int, Some(_)) => Some(Err(dup_type(*node.start))),

                // If we've already said "signed", then we shouldn't allow another sign-edness marker
                // NOTE: if we've said "unsigned", that'll be represented in the TyId, so we don't need
                // to use the `has_sign` field for that purpose.
                (Unsigned | Signed, _) if tracker.has_sign => Some(Err(dup_type(*node.start))),

                // Translate signed types to unsigned
                (Unsigned, None | Some(TyId::S32)) => Some(Ok(TyId::U32)),
                (Unsigned, Some(TyId::S8)) => Some(Ok(TyId::U8)),
                (Unsigned, Some(TyId::S16)) => Some(Ok(TyId::U16)),
                (Unsigned, Some(TyId::S64)) => Some(Ok(TyId::U64)),
                (Unsigned, Some(_)) => Some(Err(dup_type(*node.start))),

                (Signed, t @ (Some(TyId::S8 | TyId::S16 | TyId::S32 | TyId::S64) | None)) => {
                    tracker.has_sign = true;
                    Some(Ok(t.unwrap_or(TyId::S32)))
                }
                (Signed, Some(_)) => Some(Err(dup_type(*node.start))),

                (Float, Some(_)) => Some(Err(dup_type(*node.start))),
                (Float, None) => Some(Ok(TyId::F32)),

                (Double, Some(_)) => Some(Err(dup_type(*node.start))),
                (Double, None) => Some(Ok(TyId::F64)),

                (Ident | Struct(_), _) => Some(Err(error!(
                    NotImplemented,
                    "identifiers and structs not implemented yet", *node.start
                ))),

                _ => Some(Err(error!(
                    NotImplemented,
                    "unsupported declaration specifier", *node.start
                ))),
            };
        }

        return (parent_index, tracker);
    };

    let trackers: HashMap<_, _> = specifiers.into_par_iter().map(mapper).collect();

    let mut param_counters: HashMap<u32, Params> = HashMap::new();
    for (node_id, specifiers) in trackers {
        // ensure the specifiers' parent is a declaration of some kind
        let mut node = ast.nodes.index_mut(node_id as usize);

        let spec = match specifiers.type_specifier {
            Some(s) => s?,
            None => throw!(todo, "no type provided", *node.start),
        };

        // TODO: ensure the combined declaration specifiers are valid for each kind of declaration
        // add them to their parent's data field
        match *node.kind {
            AstNodeKind::Declaration(decl) => {
                node.write_data(&decl, ast::DeclSpecifiers::new().with_ty_id(spec));
            }
            AstNodeKind::FunctionDefinition(func) => {
                node.write_data(&func, ast::FuncDefSpecifiers::new().with_ty_id(spec));
            }
            AstNodeKind::ParamDecl(p) => {
                node.write_data(&p, ast::DeclSpecifiers::new().with_ty_id(spec));

                param_counters.entry(*node.parent).or_default().count += 1;
            }

            _ => throw!(Tci, "specifier attached to non-declaration", *node.start),
        };
    }

    let mut range: Vec<_> = ast
        .matching_range(|k| matches!(k, AstNodeKind::Declarator(_)))
        .into_par_iter()
        .map(|index| {
            let parent_index = ast.nodes.parent[index];
            let mut cur_index = parent_index;

            let (quals, ty_id) = loop {
                let node = ast.nodes.index(cur_index as usize);

                match node.kind {
                    // TODO: qualifiers
                    AstNodeKind::DerivedDeclarator(d) => {}

                    AstNodeKind::Declaration(d) => {
                        let ty = node.read_data(d);
                        break (ty.quals(), ty.ty_id());
                    }
                    AstNodeKind::ParamDecl(p) => {
                        let ty = node.read_data(p);
                        break (ty.quals(), ty.ty_id());
                    }
                    AstNodeKind::FunctionDefinition(f) => {
                        let ty = node.read_data(f);
                        break (ty.quals(), ty.ty_id());
                    }

                    _ => panic!("invariant broken: didn't find a declaration for this declarator"),
                }

                cur_index = *node.parent;
            };

            let ty_id = ty_db.add_type(ty_id, quals);

            DeclaratorData {
                index,
                ty_id,
                node_index: parent_index,
            }
        })
        .collect();

    let mut loop_count = 0;
    while loop_count < 20 && range.len() > 0 {
        loop_count += 1;

        let (done, ongoing): (Vec<_>, Vec<_>) = range
            .into_par_iter()
            .with_min_len(128)
            .partition_map(|index| type_for_declarator(index, ast, ty_db, &param_counters));

        let mut errors = Vec::new();
        for res in done {
            let data = match res {
                Ok(o) => o,
                Err(e) => {
                    errors.push(e);
                    continue;
                }
            };

            let mut node = ast.nodes.index_mut(data.index);
            node.write_data(&AstDeclarator::Ident, data.ty_id);
            *node.parent = data.node_index;

            let parent = ast.nodes.index(data.node_index as usize);
            if let AstNodeKind::ParamDecl(_) = parent.kind {
                param_counters
                    .get_mut(parent.parent)
                    .unwrap()
                    .collected_params
                    .push(Param {
                        post_order: *parent.post_order,
                        ty_id: data.ty_id,
                    });
            }
        }

        if errors.len() > 0 {
            return Err(errors.pop().unwrap());
        }

        range = ongoing;
    }

    if loop_count >= 20 {
        panic!("didn't finish");
    }

    return Ok(());
}

struct DeclaratorData {
    index: usize,
    ty_id: TyId,
    node_index: u32,
}

fn type_for_declarator(
    decl_data: DeclaratorData,
    ast: &ByKindAst,
    ty_db: &TyDb,
    param_counters: &HashMap<u32, Params>,
) -> Either<Result<DeclaratorData, Error>, DeclaratorData> {
    let node = ast.nodes.index(decl_data.index);
    let mut ty_id = decl_data.ty_id;

    // Use the list we created to add types to the type db
    let mut node_index = *node.parent;
    while let AstNodeKind::DerivedDeclarator(kind) = ast.nodes.kind[node_index as usize] {
        let node = ast.nodes.index(node_index as usize);
        match kind {
            AstDerivedDeclarator::Pointer => {
                ty_id = ty_db.add_ptr(ty_id, TyQuals::new());
            }

            // TODO: abstract declarators
            AstDerivedDeclarator::Function => {
                let dummy = Params::default();

                // if the derived declarator has no paramers, the param_counters object won't have any information on it
                let Params {
                    count,
                    collected_params,
                } = param_counters.get(&node_index).unwrap_or(&dummy);

                // We haven't finished collecting all the parameter types for this function;
                // we save our place in the tree so that there's not any extra types created,
                // and wait for the next loop to hopefully get more information.
                if collected_params.len() < (*count as usize) {
                    return Either::Right(DeclaratorData {
                        index: decl_data.index,
                        ty_id,
                        node_index,
                    });
                }

                let mut params = collected_params.clone();

                // Sort the collected parameters by their source order
                params.sort_by_key(|p| p.post_order);

                let params: Vec<_> = params.into_iter().map(|p| p.ty_id).collect();
                ty_id = ty_db.add_func(ty_id, &params);
            }

            _ => {
                return Either::Left(Err(error!(
                    NotImplemented,
                    "most derived declarators", *node.start
                )))
            }
        }

        node_index = ast.nodes.parent[node_index as usize];
    }

    // 7. Validate that types make sense for function definitions

    return Either::Left(Ok(DeclaratorData {
        index: decl_data.index,
        ty_id,
        node_index,
    }));
}

fn dup_type(start: u32) -> Error {
    return error!(todo, "two or more types for a single declaration", start);
}
