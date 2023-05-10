use walrus::*;

use crate::api::*;

pub fn codegen(ast: &AstNodeVec) -> Vec<u8> {
    let config = ModuleConfig::new();
    let mut module = Module::with_config(config);

    let func = FunctionBuilder::new(&mut module.types, &[], &[ValType::I32])
        .finish(vec![], &mut module.funcs);

    return module.emit_wasm();
}
