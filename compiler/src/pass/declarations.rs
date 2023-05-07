use crate::api::*;

// validate declarations -> produce declaration types
// Declaration specifiers need to make sense for the kind of declaration theyre on
pub fn validate_declaration_nodes(ast: &mut ByKindAst) -> Result<(), Error> {
    // NOTE: Going to early-return on the first error for now; ideally
    // we can return multiple errors instead though

    #[derive(Default)]
    struct SpecifierTracker {
        type_specifier: Option<Result<TyId, Error>>,
        has_int: bool,
        has_sign: bool,
    }

    let mut trackers = HashMap::<u32, SpecifierTracker>::new();

    // Build summary of all specifiers for each node with a specifier
    for (kind, range) in &ast.by_kind_in_order {
        let kind = match kind {
            AstNodeKind::Specifier(k) => *k,
            _ => continue,
        };

        use AstSpecifier::*;

        for node in ast.nodes.as_slice().index(range.clone()) {
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

                (Char, Some(_)) => Some(Err(dup_type(*node.start))),
                (Char, None) => Some(Ok(TyId::S8)),

                (Short, Some(TyId::S32) | None) => Some(Ok(TyId::S16)),
                (Short, Some(TyId::U32)) => Some(Ok(TyId::U16)),
                (Short, Some(_)) => Some(Err(dup_type(*node.start))),

                (Long, Some(TyId::S32) | None) => Some(Ok(TyId::S64)),
                (Long, Some(TyId::U32)) => Some(Ok(TyId::U64)),
                (Long, Some(_)) => Some(Err(dup_type(*node.start))),

                (Int, Some(t @ (TyId::U32 | TyId::S64 | TyId::U64 | TyId::S16 | TyId::U16))) => {
                    if tracker.has_int {
                        Some(Err(dup_type(*node.start)))
                    } else {
                        tracker.has_int = true;
                        Some(Ok(t))
                    }
                }
                (Int, Some(_)) => Some(Err(dup_type(*node.start))),
                (Int, None) => {
                    tracker.has_int = true;
                    Some(Ok(TyId::S32))
                }

                (Unsigned | Signed, _) if tracker.has_sign => Some(Err(dup_type(*node.start))),

                (Unsigned, None | Some(TyId::S32)) => Some(Ok(TyId::U32)),
                (Unsigned, Some(TyId::S16)) => Some(Ok(TyId::U16)),
                (Unsigned, Some(TyId::S64)) => Some(Ok(TyId::U64)),
                (Unsigned, Some(TyId::S8)) => Some(Ok(TyId::U8)),
                (Unsigned, Some(_)) => Some(Err(dup_type(*node.start))),

                (Signed, None) => {
                    tracker.has_sign = true;
                    Some(Ok(TyId::S32))
                }
                (Signed, Some(t @ (TyId::S16 | TyId::S64 | TyId::S8))) => {
                    tracker.has_sign = true;
                    Some(Ok(t))
                }
                (Signed, Some(_)) => Some(Err(dup_type(*node.start))),

                (Float, Some(_)) => Some(Err(dup_type(*node.start))),
                (Float, None) => Some(Ok(TyId::F32)),

                (Double, Some(_)) => Some(Err(dup_type(*node.start))),
                (Double, None) => Some(Ok(TyId::F64)),

                // AstSpecifier::Ident | AstSpecifier::Struct(_) => unimplemented!(),
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

        // ensure the combined declaration specifiers are valid for each kind of declaration
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
    let kind = AstDeclarator::Ident;
    let range = ast.by_kind[&kind.into()].clone();
    for index in range {
        let node = ast.nodes.index(index);

        let mut derived = Vec::new();
        let mut cur_index = *node.parent;

        // Build a list of derived declarators + the final specifier & qualifier
        let (quals, mut ty_id) = loop {
            let node = ast.nodes.index(cur_index as usize);

            match node.kind {
                // TODO: qualifiers
                AstNodeKind::DerivedDeclarator(d) => derived.push((*d, node)),

                AstNodeKind::Declaration(d) => {
                    let ty = node.read_data(d);
                    break (ty.quals(), ty.ty_id());
                }
                AstNodeKind::FunctionDefinition(f) => {
                    let ty = node.read_data(f);
                    break (ty.quals(), ty.ty_id());
                }

                _ => panic!("wtf"),
            }

            cur_index = *node.parent;
        };

        ty_id = ty_db.add_type(ty_id, quals);

        // Use the list we created to add types to the type db
        for (kind, node) in derived {
            match kind {
                AstDerivedDeclarator::Pointer => {
                    ty_id = ty_db.add_ptr(ty_id, quals);
                }

                AstDerivedDeclarator::Function => {
                    // ????
                }

                _ => throw!(NotImplemented "most derived declarators" *node.start),
            }
        }

        // 7. Validate that types make sense for function definitions

        ast.nodes.index_mut(index).write_data(&kind, ty_id);
    }

    return Ok(());
}
