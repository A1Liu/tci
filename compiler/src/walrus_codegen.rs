use walrus::*;

use crate::api::*;

pub fn codegen(ast: &AstNodeVec) -> Vec<u8> {
    let config = ModuleConfig::new();
    let mut module = Module::with_config(config);

    let ranges = ast.collect_to_parents_range(0..ast.len(), |node| {
        let mut current = node;

        loop {
            let parent = *current.parent;
            let is_global = parent == *current.id;

            match (*node.kind, is_global) {
                // global declarations aren't part of generated runtime code
                (AstNodeKind::Declaration(_), true) => return None,

                (_, true) => break,

                _ => {}
            }

            current = ast.index(parent as usize);
        }

        return Some(*current.id);
    });

    eprintln!("err: {:?}", ranges);

    for (parent, range) in ranges {
        let mut func = FunctionBuilder::new(&mut module.types, &[], &[ValType::I32]);

        let mut builder = func.func_body();

        for id in range {
            let node = ast.index(id);

            match *node.kind {
                AstNodeKind::Specifier(_) => continue,

                // TODO
                AstNodeKind::Declarator(_) => continue,
                AstNodeKind::DerivedDeclarator(_) => continue,
                AstNodeKind::Declaration(_) => continue,
                AstNodeKind::ParamDecl(_) => continue,
                AstNodeKind::FunctionDefinition(_) => continue,

                AstNodeKind::Expr(e) => match e {
                    AstExpr::IntLit(i) => builder.i32_const(node.read_data(&i) as u32 as i32),
                    x => unimplemented!("Expr not implemented: {:?}", x),
                },

                AstNodeKind::Statement(s) => match s {
                    AstStatement::Block => continue,

                    AstStatement::Ret => builder.return_(),
                    AstStatement::Expr => builder.drop(),

                    x => unimplemented!("Statement not implemented: {:?}", x),
                },
                // x => unimplemented!("Node not implemented: {:?}", x),
            };
        }

        func.finish(vec![], &mut module.funcs);
    }

    return module.emit_wasm();
}
