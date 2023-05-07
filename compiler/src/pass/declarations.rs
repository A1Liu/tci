use crate::api::*;

#[derive(Default)]
struct SpecifierTracker {
    type_specifier: Option<Result<TyId, Error>>,
    has_int: bool,
    has_sign: bool,
}

// validate declarations -> produce declaration types
// Declaration specifiers need to make sense for the kind of declaration theyre on
pub fn validate_declarations(ast: &mut ByKindAst) -> Result<(), Error> {
    // NOTE: Going to early-return on the first error for now; ideally
    // we can return multiple errors instead though

    let mut trackers = HashMap::<u32, SpecifierTracker>::new();

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
                node.write_data(&decl, ast::DeclSpecifiers::new().with_ty_id(spec))
            }
            AstNodeKind::FunctionDefinition(func) => {
                node.write_data(&func, ast::FuncDefSpecifiers::new().with_ty_id(spec))
            }

            _ => throw!(Tci "specifier attached to non-declaration" *node.start),
        };
    }

    let mut ty_db = TyDb::new();

    // TODO: abstract declarators
    let mut ranges = Vec::new();
    for &k in &[AstDeclarator::Ident, AstDeclarator::Abstract] {
        if let Some(s) = ast.by_kind.get(&k.into()) {
            ranges.push(s.clone());
        }
    }

    let range = ranges.into_iter().flat_map(|f| f);

    for index in range {
        let node = ast.nodes.index(index);

        let mut derived = Vec::new();
        let (parent_idx, mut ty_id) = {
            let mut cur_index = *node.parent;

            // Build a list of derived declarators + the final specifier & qualifier
            let (quals, ty_id) = loop {
                let node = ast.nodes.index(cur_index as usize);

                match node.kind {
                    // TODO: qualifiers
                    AstNodeKind::DerivedDeclarator(d) => derived.push((*d, node)),

                    // TODO: Add ParamDecl
                    AstNodeKind::Declaration(d) => {
                        let ty = node.read_data(d);
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

        // partition_map

        // Use the list we created to add types to the type db
        for (kind, node) in derived {
            match kind {
                AstDerivedDeclarator::Pointer => {
                    ty_id = ty_db.add_ptr(ty_id, TyQuals::new());
                }

                AstDerivedDeclarator::Function => {
                    // ????
                }

                _ => throw!(NotImplemented "most derived declarators" *node.start),
            }
        }

        // 7. Validate that types make sense for function definitions

        ast.nodes
            .index_mut(index)
            .write_data(&AstDeclarator::Ident, ty_id);
        ast.nodes.parent[index] = parent_idx;
    }

    return Ok(());
}
