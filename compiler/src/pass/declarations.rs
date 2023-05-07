use crate::api::*;
use ast::TypeSpecifier;

// validate declarations -> produce declaration types
// Declaration specifiers need to make sense for the kind of declaration theyre on
pub fn validate_declaration_nodes(ast: &mut ByKindAst) -> Result<(), Error> {
    // NOTE: Going to early-return on the first error for now; ideally
    // we can return multiple errors instead though

    #[derive(Default)]
    struct SpecifierTracker {
        type_specifier: Option<Result<ast::TypeSpecifier, Error>>,
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

        use ast::TypeSpecifier as Ty;
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
                (Void, None) => Some(Ok(Ty::Void)),

                (Char, Some(_)) => Some(Err(dup_type(*node.start))),
                (Char, None) => Some(Ok(Ty::Char)),

                (Short, Some(Ty::Int) | None) => Some(Ok(Ty::Short)),
                (Short, Some(Ty::UInt)) => Some(Ok(Ty::UShort)),
                (Short, Some(_)) => Some(Err(dup_type(*node.start))),

                (Long, Some(Ty::Int) | None) => Some(Ok(Ty::Long)),
                (Long, Some(Ty::UInt)) => Some(Ok(Ty::ULong)),
                (Long, Some(_)) => Some(Err(dup_type(*node.start))),

                (Int, Some(t @ (Ty::UInt | Ty::Long | Ty::ULong | Ty::Short | Ty::UShort))) => {
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
                    Some(Ok(Ty::Int))
                }

                (Unsigned | Signed, _) if tracker.has_sign => Some(Err(dup_type(*node.start))),

                (Unsigned, None | Some(Ty::Int)) => Some(Ok(Ty::UInt)),
                (Unsigned, Some(Ty::Short)) => Some(Ok(Ty::UShort)),
                (Unsigned, Some(Ty::Long)) => Some(Ok(Ty::ULong)),
                (Unsigned, Some(Ty::Char)) => Some(Ok(Ty::UChar)),
                (Unsigned, Some(_)) => Some(Err(dup_type(*node.start))),

                (Signed, None) => {
                    tracker.has_sign = true;
                    Some(Ok(Ty::Int))
                }
                (Signed, Some(t @ (Ty::Short | Ty::Long | Ty::Char))) => {
                    tracker.has_sign = true;
                    Some(Ok(t))
                }
                (Signed, Some(_)) => Some(Err(dup_type(*node.start))),

                (Float, Some(_)) => Some(Err(dup_type(*node.start))),
                (Float, None) => Some(Ok(Ty::Float)),

                (Double, Some(_)) => Some(Err(dup_type(*node.start))),
                (Double, None) => Some(Ok(Ty::Double)),

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
                node.write_data(&decl, ast::DeclSpecifiers::new().with_specifier(spec))
            }
            AstNodeKind::FunctionDefinition(func) => {
                node.write_data(&func, ast::FuncDefSpecifiers::new().with_specifier(spec))
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
        let (quals, spec) = loop {
            let node = ast.nodes.index(cur_index as usize);

            match node.kind {
                // TODO: qualifiers
                AstNodeKind::DerivedDeclarator(d) => derived.push((*d, node)),

                AstNodeKind::Declaration(d) => {
                    let ty = node.read_data(d);
                    break (ty.quals(), ty.specifier());
                }
                AstNodeKind::FunctionDefinition(f) => {
                    let ty = node.read_data(f);
                    break (ty.quals(), ty.specifier());
                }

                _ => panic!("wtf"),
            }

            cur_index = *node.parent;
        };

        let mut ty_id = match spec {
            TypeSpecifier::Void => TyId::Void,

            TypeSpecifier::Char => TyId::S8,
            TypeSpecifier::Short => TyId::S16,
            TypeSpecifier::Int => TyId::S32,
            TypeSpecifier::Long => TyId::S64,

            TypeSpecifier::UChar => TyId::U8,
            TypeSpecifier::UShort => TyId::U16,
            TypeSpecifier::UInt => TyId::U32,
            TypeSpecifier::ULong => TyId::U64,

            TypeSpecifier::Float => TyId::F32,
            TypeSpecifier::Double => TyId::F64,

            _ => {
                throw!(NotImplemented "structs, unions, idents" ast.nodes.start[cur_index as usize])
            }
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
