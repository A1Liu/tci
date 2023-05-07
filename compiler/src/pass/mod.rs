use crate::api::*;
use core::ops::Range;

pub mod types;

// TODO: how do we do node insertions and node deletions?

pub struct ByKindAst<'a> {
    pub nodes: &'a mut AstNodeVec,
    pub by_kind: HashMap<AstNodeKind, Range<usize>>,
    pub by_kind_in_order: Vec<(AstNodeKind, Range<usize>)>,
}

impl<'a> ByKindAst<'a> {
    pub fn new(ast: &'a mut AstNodeVec) -> Self {
        Self::sort_by_kind(ast);

        let mut by_kind = HashMap::new();
        let mut by_kind_in_order = Vec::new();

        let mut prev = *ast.index(0).kind;
        let mut begin: usize = 0;
        let mut index: usize = 0;
        while index < ast.len() {
            let node = ast.index(index);
            let kind = *node.kind;
            if kind == prev {
                index += 1;
                continue;
            }

            if let Some(_) = by_kind.insert(prev, begin..index) {
                panic!("kind is somehow not sorted");
            }

            by_kind_in_order.push((prev, begin..index));

            begin = index;
            prev = kind;
            index += 1;
        }

        return ByKindAst {
            nodes: ast,
            by_kind,
            by_kind_in_order,
        };
    }

    // NOTE: Assumes that the input was originally sorted by post-order
    fn sort_by_kind(ast: &mut AstNodeVec) {
        let mut indices = Vec::with_capacity(ast.len());
        for _ in 0..ast.len() {
            indices.push(u32::MAX);
        }

        // Sort by kind,height,post_order
        ast.as_mut_slice().sort_by(|a, b| {
            let kind_cmp = a.kind.cmp(b.kind);
            let order_cmp = a.post_order.cmp(b.post_order);

            match (kind_cmp, order_cmp) {
                (core::cmp::Ordering::Equal, x) => return x,
                (x, _) => return x,
            }
        });

        for (index, &order) in ast.post_order.iter().enumerate() {
            indices[order as usize] = index as u32;
        }

        // Rebuild parent indices
        for parent in &mut ast.parent {
            *parent = indices[*parent as usize];
        }
    }
}

impl<'a> Drop for ByKindAst<'a> {
    fn drop(&mut self) {
        sort_by_postorder(self.nodes);
    }
}

pub fn sort_by_postorder(ast: &mut AstNodeVec) {
    let mut indices = Vec::with_capacity(ast.len());

    for &order in &ast.post_order {
        indices.push(order);
    }

    // Sort by post_order
    ast.as_mut_slice().sort_by_key(|r| *r.post_order);

    // Rebuild parent indices
    for parent in &mut ast.parent {
        *parent = indices[*parent as usize];
    }
}

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
        let node = ast.nodes.index_mut(node_id as usize);

        let spec = match specifiers.type_specifier {
            Some(s) => s?,
            None => throw!(todo "no type provided" *node.start),
        };

        // ensure the combined declaration specifiers are valid for each kind of declaration
        // add them to their parent's data field
        match node.kind {
            AstNodeKind::Declaration(decl) => {
                ast::AstDeclaration::write(
                    node.data,
                    ast::DeclSpecifiers::new().with_specifier(spec),
                );
            }
            AstNodeKind::FunctionDefinition(_) => {
                ast::AstDeclaration::write(
                    node.data,
                    ast::DeclSpecifiers::new().with_specifier(spec),
                );
            }

            _ => throw!(Tci "specifier attached to non-declaration" *node.start),
        };
    }

    // 4. Loop over all derived declarators, and combine them into their declarator

    // 5. Loop over all declarators, and fold them into parents
    // 6. Combine type from declaration and derived declarators to produce types for each declarator
    // 7. Validate that types make sense for function definitions

    return Ok(());
}

// validate declarators relative to their scopes
//          -> produce scopes
// validate identifiers
//          -> produce types for the identifiers
//          -> track which identifiers are pointer-referenced, and when each declaration is last used
// produce global symbols?
pub fn validate_scopes(ast: &mut ByKindAst) -> Result<(), Error> {
    return Ok(());
}
