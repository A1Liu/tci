use crate::ast::*;
use crate::buckets::*;
use crate::interpreter::*;
use crate::lexer::*;
use crate::type_checker::*;
use crate::util::*;
use crate::*;
use core::alloc;
use core::mem::{align_of, size_of};
use std::collections::HashMap;

pub struct ASMFunc<'a> {
    pub func_type: TCFuncType<'a>,
    pub func_header: Option<(u32, CodeLoc)>, // first u32 points into opcodes buffer
}

pub struct Assembler<'a> {
    pub opcodes: Vec<TaggedOpcode>,
    pub func_types: HashMap<u32, u32>,
    pub functions: HashMap<u32, ASMFunc<'a>>, // keys are identifier symbols
}

impl<'a> Assembler<'a> {
    pub fn new() -> Self {
        Self {
            opcodes: Vec::new(),
            functions: HashMap::new(),
            func_types: HashMap::new(),
        }
    }

    pub fn add_file(&mut self, types: TypeEnv<'a>) -> Result<(), Error> {
        // TODO Add types here

        // Add function return sizes
        for (ident, TCFunc { func_type, .. }) in types.functions.iter() {
            self.func_types.insert(*ident, func_type.return_type.size());
        }

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
            let mut ops = self.translate_statement(&func_type, param_count, stmt);
            self.opcodes.append(&mut ops);
        }

        self.functions.insert(ident, asm_func);
        return Ok(());
    }

    pub fn translate_statement(
        &self,
        func_type: &TCFuncType,
        param_count: u32,
        stmt: &TCStmt,
    ) -> Vec<TaggedOpcode> {
        let mut ops = Vec::new();
        let mut tagged = TaggedOpcode {
            op: Opcode::StackDealloc,
            range: stmt.range,
        };

        match &stmt.kind {
            TCStmtKind::RetVal(expr) => {
                ops.append(&mut self.translate_expr(expr));

                let ret_idx = param_count as i32 * -1 - 1;
                tagged.op = Opcode::SetLocal {
                    var: ret_idx,
                    offset: 0,
                    bytes: expr.expr_type.size(),
                };
                ops.push(tagged);
                tagged.op = Opcode::Ret;
                ops.push(tagged);
            }
            TCStmtKind::Ret => {
                tagged.op = Opcode::Ret;
                ops.push(tagged);
            }

            TCStmtKind::Expr(expr) => {
                ops.append(&mut self.translate_expr(expr));
                tagged.op = Opcode::Pop {
                    bytes: expr.expr_type.size(),
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
            TCExprKind::SubI32(l, r) => {
                ops.append(&mut self.translate_expr(l));
                ops.append(&mut self.translate_expr(r));
                tagged.op = Opcode::SubI32;
                ops.push(tagged);
            }

            TCExprKind::SConv8To32(expr) => {
                ops.append(&mut self.translate_expr(expr));
                tagged.op = Opcode::SExtend8To32;
                ops.push(tagged);
            }
            TCExprKind::SConv32To64(expr) => {
                ops.append(&mut self.translate_expr(expr));
                tagged.op = Opcode::SExtend32To64;
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

            TCExprKind::Call { func, params } => {
                let rtype_size = *self.func_types.get(&func).unwrap();
                tagged.op = Opcode::StackAlloc(rtype_size);
                ops.push(tagged);

                for param in *params {
                    ops.append(&mut self.translate_expr(param));
                }

                tagged.op = Opcode::Call(*func);
                ops.push(tagged);

                tagged.op = Opcode::StackDealloc;
                for param in *params {
                    ops.push(tagged);
                }

                if rtype_size == 0 {
                    tagged.op = Opcode::StackDealloc;
                    ops.push(tagged);
                } else {
                    tagged.op = Opcode::StackAddToTemp;
                    ops.push(tagged);
                }
            }
        }

        return ops;
    }

    pub fn assemble<'b>(
        mut self,
        env: &Environment,
        symbols: &Symbols,
    ) -> Result<Program<'b>, Error> {
        let no_main = || error!("missing definition main function");
        let main_func = self.functions.get(&MAIN_SYMBOL).ok_or_else(no_main)?;
        let (main_idx, _main_loc) = main_func.func_header.ok_or_else(no_main)?;

        let mut current_func = FuncDesc { file: !0, name: !0 };
        for op in self.opcodes.iter_mut() {
            match &mut op.op {
                Opcode::Func(func_desc) => {
                    current_func = *func_desc;
                }
                Opcode::Call(addr) => {
                    let function = self.functions.get(addr).unwrap();
                    if let Some(func_header) = function.func_header {
                        let (fptr, loc) = func_header;
                        *addr = fptr;
                    } else {
                        let func_loc = function.func_type.loc;
                        return Err(error!(
                            "couldn't find definition for function",
                            op.range,
                            current_func.file,
                            "called here",
                            func_loc.range,
                            func_loc.file,
                            "declared here"
                        ));
                    }
                }
                _ => {}
            }
        }

        let file_size = env.files.size;
        let symbols_size = align_usize(symbols.names.iter().map(|i| i.len()).sum(), 8)
            + align_usize(symbols.names.len() * 16, align_of::<TaggedOpcode>());
        let opcode_size = size_of::<TaggedOpcode>();
        let opcodes_size = align_usize(self.opcodes.len() * opcode_size, align_of::<Program>());

        let total_size = file_size + symbols_size + opcodes_size;
        let buckets = BucketList::with_capacity(0);
        let layout = alloc::Layout::from_size_align(total_size, 1).expect("why did this fail?");
        let mut frame = buckets.alloc_frame(layout);

        let files = env.files.clone_into_frame(&mut frame);
        let strings: &[&str] = frame.add_array(Vec::new()); // TODO
        let symbols = symbols.names.iter().map(|i| &*frame.add_str(*i)).collect();
        let symbols: &[&str] = frame.add_array(symbols);
        let ops = frame.add_array(self.opcodes);

        let program = Program {
            files,
            strings,
            symbols,
            ops,
            main_idx,
        };

        return Ok(program);
    }
}
