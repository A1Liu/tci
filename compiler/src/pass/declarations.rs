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

// validate declarations -> produce declaration types
// Declaration specifiers need to make sense for the kind of declaration theyre on
pub fn validate_declarations(ast: &mut ByKindAst, ty_db: &TyDb) -> Result<(), Error> {
    // NOTE: Going to early-return on the first error for now; ideally
    // we can return multiple errors instead though

    let mut trackers = HashMap::<u32, SpecifierTracker>::new();
    let mut param_counters: HashMap<u32, Params> = HashMap::new();

    // Build summary of all specifiers for each node with a specifier
    for (kind, range) in &ast.by_kind_in_order {
        let kind = match kind {
            AstNodeKind::Specifier(k) => *k,
            _ => continue,
        };

        for node in ast.nodes.as_slice().index(range.clone()) {
            use AstSpecifier::*;

            let tracker = trackers
                .entry(*node.parent)
                .or_insert(SpecifierTracker::default());

            let spec = match tracker.type_specifier.take().transpose() {
                Ok(s) => s,
                Err(e) => {
                    tracker.type_specifier = Some(Err(e));
                    continue;
                }
            };

            fn dup_type(start: u32) -> Error {
                return error!(todo "two or more types for a single declaration" start);
            }

            // ensure the combined specifiers are valid for the declaration
            tracker.type_specifier = match (kind, spec) {
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

                (Ident | Struct(_), _) => {
                    throw!(NotImplemented "identifiers and structs not implemented yet" *node.start)
                }

                _ => throw!(NotImplemented "unsupported declaration specifier" *node.start),
            };
        }
    }

    for (node_id, specifiers) in trackers {
        // ensure the specifiers' parent is a declaration of some kind
        let mut node = ast.nodes.index_mut(node_id as usize);

        let spec = match specifiers.type_specifier {
            Some(s) => s?,
            None => throw!(todo "no type provided" *node.start),
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

            _ => throw!(Tci "specifier attached to non-declaration" *node.start),
        };
    }

    let mut range: Vec<_> = [AstDeclarator::Ident, AstDeclarator::Abstract]
        .into_iter()
        .filter_map(|k| ast.by_kind.get(&k.into()))
        .flat_map(|r| r.clone().into_iter())
        .collect();

    let mut loop_count = 0;
    while loop_count < 20 {
        loop_count += 1;

        let (left, right): (Vec<_>, Vec<_>) = range
            .into_par_iter()
            .partition_map(|index| type_for_declarator(index, ast, ty_db, &param_counters));

        let mut errors = Vec::new();
        for res in left {
            let data = match res {
                Ok(o) => o,
                Err(e) => {
                    errors.push(e);
                    continue;
                }
            };

            let mut node = ast.nodes.index_mut(data.index);
            node.write_data(&AstDeclarator::Ident, data.ty_id);
            *node.parent = data.parent_index;

            let parent = ast.nodes.index(data.parent_index as usize);
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

        if right.len() == 0 {
            break;
        }

        range = Vec::with_capacity(right.len());
        for data in right {
            range.push(data.index);

            // let mut node = ast.nodes.index_mut(data.index);
            // node.write_data(&AstDeclarator::Ident, data.ty_id);
            // *node.parent = data.parent_index;
        }
    }

    if loop_count >= 20 {
        panic!("didn't finish");
    }

    return Ok(());
}

struct DeclaratorData {
    index: usize,
    ty_id: TyId,
    parent_index: u32,
}

fn type_for_declarator(
    index: usize,
    ast: &ByKindAst,
    ty_db: &TyDb,
    param_counters: &HashMap<u32, Params>,
) -> Either<Result<DeclaratorData, Error>, DeclaratorData> {
    let node = ast.nodes.index(index);

    let mut derived = Vec::new();
    let (parent_index, mut ty_id) = {
        let mut cur_index = *node.parent;

        // Build a list of derived declarators + the final specifier & qualifier
        let (quals, ty_id) = loop {
            let node = ast.nodes.index(cur_index as usize);

            match node.kind {
                // TODO: qualifiers
                AstNodeKind::DerivedDeclarator(d) => derived.push((*d, cur_index)),

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

        // cur_index is now pointing to the parent
        (cur_index, ty_db.add_type(ty_id, quals))
    };

    // Use the list we created to add types to the type db
    for (kind, node_index) in derived {
        let node = ast.nodes.index(node_index as usize);
        match kind {
            AstDerivedDeclarator::Pointer => {
                ty_id = ty_db.add_ptr(ty_id, TyQuals::new());
            }

            // TODO: abstract declarators
            AstDerivedDeclarator::Function => {
                let dummy = Params::default();
                let Params {
                    count,
                    collected_params,
                } = match param_counters.get(&node_index) {
                    Some(s) => s,

                    // if the derived declarator has no paramers, the param_counters object won't have any information on it
                    None => &dummy,
                };

                if collected_params.len() < (*count as usize) {
                    return Either::Right(DeclaratorData {
                        index,
                        ty_id,
                        parent_index: node_index,
                    });
                }

                let mut params = collected_params.clone();

                // Sort the collected parameters by their source order
                params.sort_by_key(|p| p.post_order);

                let params: Vec<_> = params.into_iter().map(|p| p.ty_id).collect();
                ty_id = ty_db.add_func(ty_id, &params);
            }

            _ => {
                return Either::Left(Err(
                    error!(NotImplemented "most derived declarators" *node.start),
                ))
            }
        }
    }

    // 7. Validate that types make sense for function definitions

    return Either::Left(Ok(DeclaratorData {
        index,
        ty_id,
        parent_index,
    }));
}
