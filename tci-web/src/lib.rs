use compiler::api::*;
use serde::Serialize;
use wasm_bindgen::prelude::*;

#[global_allocator]
static GLOBAL: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[derive(Serialize)]
pub struct PipelineOutput {
    source: String,
    lexer: Vec<TokenKind>,
    macro_expansion: Vec<TokenKind>,
    parsed_ast: Vec<compiler::SimpleAstNode>,
}

#[wasm_bindgen]
pub fn compile(source: String) -> Result<String, String> {
    let mut source_string = source.to_string();
    if !source_string.ends_with("\n") {
        source_string.push('\n');
    }

    let mut files = FileDb::new();
    let file_id = files.add_file("main.c".to_string(), source_string)?;
    let file = &files.files[file_id as usize];

    let lexer_res = lex(&files, file).expect("Expected lex to succeed");

    let macro_expansion_res = expand_macros(lexer_res.tokens.as_slice());

    let parsed_ast = parse(&macro_expansion_res).map_err(|_e| "parsing failed")?;
    let mut simple_ast = Vec::with_capacity(parsed_ast.len());
    for node in parsed_ast.as_slice() {
        simple_ast.push(compiler::SimpleAstNode {
            kind: *node.kind,
            parent: *node.parent,
            post_order: *node.post_order,
            height: *node.height,
        });
    }

    let out = PipelineOutput {
        source,
        lexer: lexer_res.tokens.kind,
        macro_expansion: macro_expansion_res.kind,
        parsed_ast: simple_ast,
    };

    let out = serde_json::to_string(&out).map_err(|e| e.to_string())?;

    return Ok(out);
}
