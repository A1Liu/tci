/*!
Checks things about individual declarations.

This includes:
- Validate declaration specifiers - check the declaration specifiers and type specifier on each declaration
- Validate derived declarators - check that type qualifiers make sense for each declarator
- Add types to declarators - Fill the `data` field of `AstDeclarator` with the `TyId`
- Build `TyDb` - Add references to type definitions, function definitions, etc; these references
  won't necessarily be resolved yet, but they will at least exist.
- TODO: validate declaration types - E.g. function definitions need to be functions
- TODO: Validate declaration AST locations - Since the parser is incredibly lenient, we also need to ensure
     that e.g. structs don't have function definitions.

 Some side effects and invariants after this pass completes successfully runs:
 - All `AstDerivedDeclarator` nodes are likely completely useless once this pass runs.
 - `AstDeclarator` nodes now have their `type_id` available via `AstNodeRef::read_data`

 In the future, it may be useful to move `AstDeclarator` nodes up the tree,
 and remove the in-between `AstDerivedDeclarator` nodes. However, right now,
 structural AST modifications are a bit too much.
 */

use crate::api::*;

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

pub fn validate_declarations(ast: &mut AstNodeVec, ty_db: &TyDb) -> Result<(), Vec<Error>> {
    // NOTE: Going to early-return on the first error for now; ideally
    // we can return multiple errors instead though

    let specifiers = ast.collect_to_parents_range(0..ast.len(), |node| match node.kind {
        AstNodeKind::Specifier(k) => Some(*node.parent),
        _ => None,
    });

    // Build summary of all specifiers for each node with a specifier

    let (trackers, errors): (HashMap<_, _>, Vec<_>) =
        specifiers
            .into_par_iter()
            .partition_map(|(parent_index, specifiers)| {
                let mut tracker = SpecifierTracker::default();

                for node_index in specifiers {
                    let node = ast.index(node_index as usize);
                    let kind = match *node.kind {
                        AstNodeKind::Specifier(s) => s,
                        _ => panic!("wtf"),
                    };

                    if let Err(e) = tracker.consume_specifier(kind, node) {
                        return Either::Right(e);
                    }
                }

                match tracker.type_specifier {
                    Some(s) => Either::Left((parent_index, s)),
                    None => Either::Right(error!(
                        Todo,
                        "no type provided", ast.start[parent_index as usize]
                    )),
                }
            });

    if errors.len() > 0 {
        return Err(errors);
    }

    let mut param_counters: HashMap<u32, Params> = HashMap::new();
    let mut errors = Vec::new();
    for (&node_id, &ty_id) in &trackers {
        // ensure the specifiers' parent is a declaration of some kind
        let node = ast.index_mut(node_id as usize);

        // TODO: ensure the combined declaration specifiers are valid for each kind of declaration
        // add them to their parent's data field
        match *node.kind {
            AstNodeKind::Declaration(_) | AstNodeKind::FunctionDefinition(_) => {}
            AstNodeKind::ParamDecl(_) => param_counters.entry(*node.parent).or_default().count += 1,
            _ => errors.push(error!(
                Tci,
                "specifier attached to non-declaration", *node.start
            )),
        };

        *node.ty_id = ty_id;
    }

    if errors.len() > 0 {
        return Err(errors);
    }

    let info = AstInfo {
        kind: &ast.kind,
        parent: &ast.parent,
        post_order: &ast.post_order,
        start: &ast.start,
        data: &ast.data,
    };

    let (mut done, mut ongoing): (Vec<_>, Vec<_>) = ast
        .ty_id
        .par_iter_mut()
        .enumerate()
        .filter(|(id, ty_slot)| matches!(info.kind[*id], AstNodeKind::Declarator(_)))
        .map(|(index, ty_slot)| {
            let parent_index = info.parent[index];
            let mut cur_index = parent_index as usize;

            let ty_id = loop {
                match info.kind[cur_index] {
                    // TODO: qualifiers
                    AstNodeKind::DerivedDeclarator(d) => {}

                    AstNodeKind::Declaration(d) => {
                        break trackers[&(cur_index as u32)];
                    }
                    AstNodeKind::ParamDecl(p) => {
                        break trackers[&(cur_index as u32)];
                    }
                    AstNodeKind::FunctionDefinition(f) => {
                        break trackers[&(cur_index as u32)];
                    }

                    _ => panic!("invariant broken: didn't find a declaration for this declarator"),
                }

                cur_index = info.parent[cur_index] as usize;
            };

            DeclaratorData {
                index,
                ty_slot,
                ty_id,
                node_index: parent_index as usize,
            }
        })
        .partition_map(|index| type_for_declarator(index, info, ty_db, &param_counters));

    let mut loop_count = 0;
    loop {
        let (done_ok, done_err): (Vec<_>, Vec<_>) =
            done.into_iter().partition_map(|res| match res {
                Ok(o) => Either::Left(o),
                Err(e) => Either::Right(e),
            });

        if done_err.len() > 0 {
            return Err(errors);
        }

        for data in done_ok {
            let decl = data.node_index;
            if let AstNodeKind::ParamDecl(_) = info.kind[decl] {
                let counter = param_counters.get_mut(&info.parent[decl]).unwrap();
                counter.collected_params.push(Param {
                    post_order: info.post_order[decl],
                    ty_id: data.ty_id,
                });
            }
        }

        loop_count += 1;

        (done, ongoing) = ongoing
            .into_par_iter()
            .with_min_len(128)
            .partition_map(|index| type_for_declarator(index, info, ty_db, &param_counters));

        if loop_count >= 20 || ongoing.len() == 0 {
            break;
        }
    }

    if ongoing.len() > 0 {
        panic!("didn't finish");
    }

    return Ok(());
}

#[derive(Clone, Copy)]
struct AstInfo<'a> {
    kind: &'a [AstNodeKind],
    parent: &'a [u32],
    post_order: &'a [u32],
    start: &'a [u32],
    data: &'a [u64],
}

struct DeclaratorData<'a> {
    ty_slot: &'a mut TyId,
    index: usize,
    ty_id: TyId,
    node_index: usize,
}

fn type_for_declarator<'a>(
    state: DeclaratorData<'a>,
    info: AstInfo,
    ty_db: &TyDb,
    param_counters: &HashMap<u32, Params>,
) -> Either<Result<DeclaratorData<'a>, Error>, DeclaratorData<'a>> {
    // Use the list we created to add types to the type db
    let mut node_index = state.node_index;
    let mut ty_id = state.ty_id;
    while let AstNodeKind::DerivedDeclarator(kind) = info.kind[node_index] {
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
                } = param_counters.get(&(node_index as u32)).unwrap_or(&dummy);

                // We haven't finished collecting all the parameter types for this function;
                // we save our place in the tree so that there's not any extra types created,
                // and wait for the next loop to hopefully get more information.
                if collected_params.len() < (*count as usize) {
                    return Either::Right(DeclaratorData {
                        index: state.index,
                        ty_slot: state.ty_slot,
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
                    "most derived declarators", info.start[node_index]
                )))
            }
        }

        node_index = info.parent[node_index] as usize;
    }

    // 7. Validate that types make sense for function definitions

    *state.ty_slot = ty_id;

    return Either::Left(Ok(DeclaratorData {
        index: state.index,
        ty_slot: state.ty_slot,
        ty_id,
        node_index,
    }));
}

fn dup_type(start: u32) -> Error {
    return error!(todo, "two or more types for a single declaration", start);
}

#[derive(Default)]
struct SpecifierTracker {
    type_specifier: Option<TyId>,
    has_int: bool,
    has_sign: bool,
}

impl SpecifierTracker {
    fn consume_specifier<'a>(
        &mut self,
        spec: AstSpecifier,
        node: ast::AstNodeRef<'a>,
    ) -> Result<(), Error> {
        use AstSpecifier::*;

        // ensure the combined specifiers are valid for the declaration
        self.type_specifier = match (spec, self.type_specifier) {
            (Void, Some(_)) => return Err(dup_type(*node.start)),
            (Void, None) => Some(TyId::Void),

            (Char, Some(TyId::S32 | TyId::U32)) if self.has_int => {
                return Err(dup_type(*node.start))
            }
            (Char, Some(TyId::S32)) => Some(TyId::S8),
            (Char, Some(TyId::U32)) => Some(TyId::U8),
            (Char, Some(_)) => return Err(dup_type(*node.start)),
            (Char, None) => Some(TyId::S8),

            (Short, Some(TyId::S32) | None) => Some(TyId::S16),
            (Short, Some(TyId::U32)) => Some(TyId::U16),
            (Short, Some(_)) => return Err(dup_type(*node.start)),

            (Long, Some(TyId::S32) | None) => Some(TyId::S64),
            (Long, Some(TyId::U32)) => Some(TyId::U64),
            (Long, Some(_)) => return Err(dup_type(*node.start)),

            (Int, t @ (Some(TyId::U32 | TyId::S64 | TyId::U64 | TyId::S16 | TyId::U16) | None)) => {
                if self.has_int {
                    return Err(dup_type(*node.start));
                } else {
                    self.has_int = true;
                    Some(t.unwrap_or(TyId::S32))
                }
            }
            (Int, Some(_)) => return Err(dup_type(*node.start)),

            // If we've already said "signed", then we shouldn't allow another sign-edness marker
            // NOTE: if we've said "unsigned", that'll be represented in the TyId, so we don't need
            // to use the `has_sign` field for that purpose.
            (Unsigned | Signed, _) if self.has_sign => return Err(dup_type(*node.start)),

            // Translate signed types to unsigned
            (Unsigned, None | Some(TyId::S32)) => Some(TyId::U32),
            (Unsigned, Some(TyId::S8)) => Some(TyId::U8),
            (Unsigned, Some(TyId::S16)) => Some(TyId::U16),
            (Unsigned, Some(TyId::S64)) => Some(TyId::U64),
            (Unsigned, Some(_)) => return Err(dup_type(*node.start)),

            (Signed, t @ (Some(TyId::S8 | TyId::S16 | TyId::S32 | TyId::S64) | None)) => {
                self.has_sign = true;
                Some(t.unwrap_or(TyId::S32))
            }
            (Signed, Some(_)) => return Err(dup_type(*node.start)),

            (Float, Some(_)) => return Err(dup_type(*node.start)),
            (Float, None) => Some(TyId::F32),

            (Double, Some(_)) => return Err(dup_type(*node.start)),
            (Double, None) => Some(TyId::F64),

            (Ident | Struct(_), _) => {
                return Err(error!(
                    NotImplemented,
                    "identifiers and structs not implemented yet", *node.start
                ))
            }

            _ => {
                return Err(error!(
                    NotImplemented,
                    "unsupported declaration specifier", *node.start
                ))
            }
        };

        Ok(())
    }
}
