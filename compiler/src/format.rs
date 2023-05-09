use crate::api::*;

/// Prints the tree in a text format, so that it's a lil easier to read.
/// Output right now looks like this:
///
/// ```text
/// FunctionDefinition(AstFunctionDefinition)                                  
/// └ Specifier(Int)                                                               
/// └ Declarator(Ident)                                                            
/// | └ DerivedDeclarator(Function)                                                
/// | | └ Declaration(AstDeclaration)                                              
/// | | | └ Specifier(Int)                                                         
/// | | | └ Declarator(Ident)                                                      
/// | | └ Declaration(AstDeclaration)                                              
/// | | | └ Specifier(Char)                                                        
/// | | | └ Declarator(Ident)                                                      
/// | | | | └ DerivedDeclarator(Pointer)                                           
/// | | | | └ DerivedDeclarator(Pointer)                                           
/// └ Statement(Block)                                                             
/// | └ Statement(Ret)                                                             
/// | | └ Expr(StringLit)
/// ```
pub fn display_tree(ast: &[AstNode], ty_db: Option<&TyDb>) -> String {
    use std::fmt::Write;

    let mut children = Vec::<Vec<usize>>::with_capacity(ast.len());
    children.resize_with(ast.len(), || Vec::new());

    let mut roots = Vec::new();

    for node in ast.into_iter() {
        if node.post_order != node.parent {
            children[node.parent as usize].push(node.post_order as usize);
        } else {
            roots.push(node.post_order);
        }
    }

    let mut parent_stack = Vec::with_capacity(roots.len());
    for root in roots.iter().rev() {
        parent_stack.push((0u32, *root as usize));
    }

    let mut out = String::new();
    while let Some((depth, node_id)) = parent_stack.pop() {
        if depth > 0 {
            for _ in 0..(depth - 1) {
                out += "| ";
            }

            out += "└ ";
        }

        let node = &ast[node_id];
        out += &format!("{:?}", node.kind);

        match node.kind {
            AstNodeKind::Declarator(d) => {
                out += " ";

                let info = node.read_data(&d);
                if let Some(ty_db) = ty_db {
                    ty_db.write(&mut out, info.ty_id());
                } else {
                    write!(out, "{:?}", info.ty_id()).unwrap();
                }

                out += "\n";
            }
            _ => out.push('\n'),
        }

        for id in children[node_id].iter().rev() {
            parent_stack.push((depth + 1, *id));
        }
    }

    return out;
}
