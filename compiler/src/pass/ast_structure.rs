/*!
Validate AST structure.

- Ensure that function definitions are always global
- Ensure that struct field declarators aren't abstract
- TODO: Function definitions with non-function declarators, function definitions with abstract declarators
 */
use crate::api::*;

pub fn validate_structure(ast: &AstNodeVec) -> Result<(), Vec<Error>> {
    let (errors, _): (Vec<_>, ()) = (0..ast.len()).into_par_iter().partition_map(|index| {
        let kind = ast.kind[index];
        let parent_index = ast.parent[index] as usize;
        let parent_kind = ast.kind[parent_index];
        let is_global = index == parent_index;
        let start = &ast.start[index];

        let err = match (kind, parent_kind) {
            (AstNodeKind::FunctionDefinition(_), _) if !is_global => error!(
                Todo,
                "function definitions always need to be global", *start
            ),

            (AstNodeKind::Declarator(AstDeclarator::Abstract), _) => 'error: {
                for node in ast.parent_chain(index) {
                    break 'error match node.kind {
                        AstNodeKind::DerivedDeclarator(
                            AstDerivedDeclarator::Function | AstDerivedDeclarator::FunctionElipsis,
                        ) => continue,
                        AstNodeKind::Specifier(AstSpecifier::Struct(_)) => error!(
                            Todo,
                            "Abstract declarator was used as part of struct field", *start
                        ),

                        _ => continue,
                    };
                }

                return Either::Right(());
            }

            _ => return Either::Right(()),
        };

        return Either::Left(err);
    });

    if errors.len() != 0 {
        return Err(errors);
    }

    return Ok(());
}
