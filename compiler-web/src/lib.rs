use compiler::{api::*, run_compiler_for_testing, single_file_db, StageOutput};
use serde::Serialize;
use wasm_bindgen::prelude::*;

#[global_allocator]
static GLOBAL: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[derive(Serialize)]
pub struct PipelineOutput {
    lexer: Option<Vec<TokenKind>>,
    macro_expansion: Option<Vec<TokenKind>>,
    parsed_ast: Option<Vec<AstNode>>,
    errors: Option<Vec<String>>,
}

#[wasm_bindgen]
pub fn compile(source: String) -> Result<String, String> {
    let mut output = PipelineOutput {
        lexer: None,
        macro_expansion: None,
        parsed_ast: None,
        errors: None,
    };

    'done: {
        let (files, file_id) = single_file_db(source);
        let data = run_compiler_for_testing(&files, file_id);

        macro_rules! stage_transfer {
            ($i:ident) => {
                match data.$i {
                    StageOutput::Ok(l) => output.$i = Some(l),
                    StageOutput::Ignore => {}
                    StageOutput::Err(e) => {
                        let error = format!(concat!(stringify!($i), " error: {:?}"), e);
                        output.errors.get_or_insert(Vec::new()).push(error);

                        break 'done;
                    }
                }
            };
        }

        stage_transfer!(lexer);
        stage_transfer!(macro_expansion);
        stage_transfer!(parsed_ast);
    }

    let out = serde_json::to_string(&output).map_err(|e| e.to_string())?;

    return Ok(out);
}
