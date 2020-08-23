use crate::ast::*;
use crate::interpreter::*;
use crate::lexer::*;
use crate::type_checker::*;
use crate::util::*;
use std::collections::HashMap;

pub struct ASMFunc<'a> {
    pub func_type: TCFuncType<'a>,
    pub func_header: Option<(u32, CodeLoc)>, // points into opcodes buffer
}

pub struct Assembler<'a> {
    pub opcodes: Vec<TaggedOpcode>,
    pub functions: HashMap<u32, ASMFunc<'a>>, // keys are identifier symbols
}

impl<'a> Assembler<'a> {
    pub fn new() -> Self {
        let opcodes = vec![
            Opcode::Func(FuncDesc {
                file: 0,
                name: _MAIN_SYMBOL,
            }),
            Opcode::StackAlloc(4),
            Opcode::StackAlloc(4), // int argc
            Opcode::StackAlloc(8), // int argv
            Opcode::Call(MAIN_SYMBOL),
        ]
        .into_iter()
        .map(|op| TaggedOpcode { op, range: r(0, 0) })
        .collect();

        Self {
            opcodes,
            functions: HashMap::new(),
        }
    }

    pub fn add_file(&mut self, types: TypeEnv<'a>) -> Result<(), Error> {
        // TODO Add types here

        // Add functions
        for (ident, func) in types.functions.into_iter() {
            self.add_function(ident, func)?;
        }

        return Ok(());
    }

    pub fn add_function(&mut self, ident: u32, func: TCFunc<'a>) -> Result<(), Error> {
        let (func_type, func_header) = match self.functions.get(&ident) {
            Some(asm_func) => {
                if asm_func.func_type != func.func_type {
                    let error = func_decl_mismatch(asm_func.func_type.loc, func.func_type.loc);
                    return Err(error);
                }
                (asm_func.func_type, asm_func.func_header)
            }
            None => (func.func_type, None),
        };

        let asm_func = ASMFunc {
            func_type,
            func_header: None,
        };

        let defn = match func.defn {
            Some(defn) => defn,
            None => {
                self.functions.insert(ident, asm_func);
                return Ok(());
            }
        };

        if let Some((func_header, defn_loc)) = func_header {
            return Err(func_redef(defn_loc, defn.loc));
        }

        let func_header = self.opcodes.len() as u32;
        let param_count = func_type.params.len() as u32;

        for stmt in defn.stmts {
            let mut ops = self.translate_statement(param_count, stmt);
            self.opcodes.append(&mut ops);
        }

        return Err(error!("unimplemented"));
    }

    pub fn translate_statement(&self, param_count: u32, stmt: &TCStmt) -> Vec<TaggedOpcode> {
        let ops = Vec::new();

        match &stmt.kind {
            TCStmtKind::RetVal(val) => {}
        }

        return ops;
    }

    pub fn translate_expr(&self, expr: &TCExpr) -> Vec<TaggedOpcode> {
        let mut ops = Vec::new();
        let mut tagged = TaggedOpcode {
            op: Opcode::StackDealloc,
            range: expr.range,
        };

        match &expr.kind {
            TCExprKind::IntLiteral(val) => {
                tagged.op = Opcode::MakeTempInt32(*val);
                ops.push(tagged);
            }
            TCExprKind::AddI32(l, r) => {
                ops.append(&mut self.translate_expr(l));
                ops.append(&mut self.translate_expr(r));
                tagged.op = Opcode::AddU32;
                ops.push(tagged);
            }
            TCExprKind::AddU64(l, r) => {
                ops.append(&mut self.translate_expr(l));
                ops.append(&mut self.translate_expr(r));
                tagged.op = Opcode::AddU64;
                ops.push(tagged);
            }
            TCExprKind::SConv8To32(expr) => {
                ops.append(&mut self.translate_expr(expr));
                tagged.op = Opcode::PushZero { bytes: 7 };
                ops.push(tagged);
                tagged.op = Opcode::Swap { top: 7, bottom: 1 };
                ops.push(tagged);
            }
            TCExprKind::ZConv8To32(expr) => {}
            TCExprKind::ZConv32To64(expr) => {}
        }

        return ops;
    }

    // pub fn assemble(self) -> Program {
    //     todo!()
    // }
}
