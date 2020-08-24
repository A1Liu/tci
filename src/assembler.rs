use crate::ast::*;
use crate::buckets::*;
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
            Opcode::MakeTempInt32(0),
            Opcode::SetLocal {
                var: 0,
                offset: 0,
                bytes: 4,
            },
            Opcode::Call(MAIN_SYMBOL),
            Opcode::GetGlobal {
                var: 0,
                offset: 0,
                bytes: 4,
            },
            Opcode::Ecall(ECALL_EXIT_WITH_CODE),
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

        let mut asm_func = ASMFunc {
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

        asm_func.func_header = Some((self.opcodes.len() as u32, defn.loc));
        let param_count = func_type.params.len() as u32;

        self.opcodes.push(TaggedOpcode {
            op: Opcode::Func(FuncDesc {
                file: defn.loc.file,
                name: ident,
            }),
            range: defn.loc.range,
        });

        for stmt in defn.stmts {
            let mut ops = self.translate_statement(param_count, stmt);
            self.opcodes.append(&mut ops);
        }

        self.functions.insert(ident, asm_func);
        return Ok(());
    }

    pub fn translate_statement(&self, param_count: u32, stmt: &TCStmt) -> Vec<TaggedOpcode> {
        let mut ops = Vec::new();
        let mut tagged = TaggedOpcode {
            op: Opcode::StackDealloc,
            range: stmt.range,
        };

        match &stmt.kind {
            TCStmtKind::RetVal(val) => {
                let ret_idx = param_count as i32 * -1;
                tagged.op = Opcode::SetLocal {
                    var: ret_idx,
                    offset: 0,
                    bytes: val.expr_type.size(),
                };
                ops.push(tagged);
            }
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
                tagged.op = Opcode::SExtend8To32;
                ops.push(tagged);
            }
            TCExprKind::ZConv8To32(expr) => {
                ops.append(&mut self.translate_expr(expr));
                tagged.op = Opcode::ZExtend8To32;
                ops.push(tagged);
            }
            TCExprKind::ZConv32To64(expr) => {
                ops.append(&mut self.translate_expr(expr));
                tagged.op = Opcode::ZExtend32To64;
                ops.push(tagged);
            }
        }

        return ops;
    }

    pub fn assemble<'b>(self, buckets: BucketListRef<'b>) -> Program<'b> {
        todo!()
    }
}
