use compiler::api::*;
use serde::Serialize;
use wasm_bindgen::prelude::*;

#[global_allocator]
static GLOBAL: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[derive(Serialize)]
pub struct PipelineOutput {
    lexer: Option<Vec<TokenKind>>,
    macro_expansion: Option<Vec<TokenKind>>,
    parsed_ast: Option<Vec<compiler::SimpleAstNode>>,
    error: Option<String>,
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

    let mut output = PipelineOutput {
        lexer: None,
        macro_expansion: None,
        parsed_ast: None,
        error: None,
    };

    'done: {
        let lexer_res = match lex(&files, file) {
            Ok(l) => l,
            Err(e) => {
                output.error = Some(format!("lex error: {:?}", e));
                break 'done;
            }
        };

        output.lexer = Some(lexer_res.tokens.kind.clone());

        let macro_expansion_res = expand_macros(lexer_res.tokens.as_slice());

        let parsed_ast = match parse(&macro_expansion_res) {
            Ok(l) => l,
            Err(e) => {
                output.error = Some(format!("parse error: {:?}", e));
                break 'done;
            }
        };

        let mut simple_ast = Vec::with_capacity(parsed_ast.len());
        for node in parsed_ast.as_slice() {
            simple_ast.push(compiler::SimpleAstNode {
                kind: *node.kind,
                parent: *node.parent,
                post_order: *node.post_order,
                height: *node.height,
            });
        }
        output.parsed_ast = Some(simple_ast);
    }

    let out = serde_json::to_string(&output).map_err(|e| e.to_string())?;

    return Ok(out);
}
