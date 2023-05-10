use walrus::ValType;

use crate::api::*;

pub fn codegen(ast: &AstNodeVec) -> Vec<u8> {
    let config = walrus::ModuleConfig::new();
    let mut module = walrus::Module::with_config(config);

    let func = walrus::FunctionBuilder::new(&mut module.types, &[], &[ValType::I32])
        .finish(vec![], &mut module.funcs);

    return module.emit_wasm();
}
