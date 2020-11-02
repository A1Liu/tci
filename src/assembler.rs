use crate::ast::*;
use crate::buckets::*;
use crate::filedb::*;
use crate::interpreter::*;
use crate::runtime::*;
use crate::type_checker::*;
use crate::util::*;
use core::alloc;
use core::mem;
use core::mem::{align_of, size_of};
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct ASMFunc<'a> {
    pub func_type: TCFuncType<'a>,
    pub func_header: Option<(u32, CodeLoc)>, // first u32 points into opcodes buffer
}

pub static LIB_FUNCS: LazyStatic<HashSet<u32>> = lazy_static!(lib_funcs, HashSet<u32>, {
    let mut m = HashSet::new();
    m.insert(INIT_SYMS.translate["printf"]);
    m.insert(INIT_SYMS.translate["exit"]);
    m.insert(INIT_SYMS.translate["malloc"]);
    m.insert(INIT_SYMS.translate["free"]);
    m
});

pub fn init_main_no_args(main_sym: u32) -> Vec<Opcode> {
    return vec![
        Opcode::StackAlloc {
            bytes: 4,
            symbol: META_NO_SYMBOL,
        },
        Opcode::Call(main_sym),
        Opcode::GetLocal {
            var: 0,
            offset: 0,
            bytes: 4,
        },
        Opcode::Ecall(ECALL_EXIT),
    ];
}

pub struct Assembler<'a> {
    pub opcodes: Vec<TaggedOpcode>,
    pub func_types: HashMap<u32, u32>,
    pub data: VarBuffer,
    pub functions: HashMap<u32, ASMFunc<'a>>, // keys are identifier symbols
    pub types: HashMap<u32, RuntimeStruct<'a>>,
    pub symbols: Vec<RuntimeVar>,
    pub struct_member_count: usize,
}

impl<'a> Assembler<'a> {
    pub fn new() -> Self {
        Self {
            opcodes: Vec::new(),
            data: VarBuffer::new(),
            functions: HashMap::new(),
            func_types: HashMap::new(),
            types: HashMap::new(),
            symbols: Vec::new(),
            struct_member_count: 0,
        }
    }

    pub fn add_file(&mut self, typed_ast: TypedFuncs<'a>) -> Result<(), Error> {
        for (struct_id, struct_type) in typed_ast.types.structs.iter() {
            let struct_type = match &struct_type.defn {
                Some(struct_type) => struct_type,
                None => {
                    if !self.types.contains_key(struct_id) {
                        self.types.insert(
                            *struct_id,
                            RuntimeStruct {
                                members: None,
                                loc: struct_type.decl_loc,
                                sa: TC_UNKNOWN_SA,
                            },
                        );
                    }

                    continue;
                }
            };

            if let Some(RuntimeStruct { members, loc, sa }) = self.types.get_mut(struct_id) {
                if let Some(members) = members {
                    if members != &struct_type.members {
                        return Err(error!(
                            "multiple, conflicting definitions of same type",
                            *loc, "first one found here", struct_type.loc, "second one found here"
                        ));
                    }
                } else {
                    *members = Some(struct_type.members);
                    *loc = struct_type.loc;
                    *sa = struct_type.sa;
                    self.struct_member_count += struct_type.members.len();
                }

                continue;
            }

            self.types.insert(
                *struct_id,
                RuntimeStruct {
                    members: Some(struct_type.members),
                    loc: struct_type.loc,
                    sa: struct_type.sa,
                },
            );
            self.struct_member_count += struct_type.members.len();
        }

        // Add function return sizes
        for (ident, TCFunc { func_type, .. }) in typed_ast.functions.iter() {
            self.func_types.insert(*ident, func_type.return_type.size());
        }

        // Add functions
        for (ident, func) in typed_ast.functions.into_iter() {
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

        if let Some((_func_header, defn_loc)) = func_header {
            return Err(func_redef(defn_loc, defn.loc));
        }

        asm_func.func_header = Some((self.opcodes.len() as u32, defn.loc));
        let param_count = func_type.params.len() as u32;

        self.opcodes.push(TaggedOpcode {
            op: Opcode::Func(ident),
            loc: defn.loc,
        });

        let mut ops = self.translate_block(param_count, defn.stmts, defn.loc, 0, 0);
        self.opcodes.append(&mut ops);

        self.opcodes.push(TaggedOpcode {
            op: Opcode::Ret,
            loc: defn.loc,
        });

        self.functions.insert(ident, asm_func);
        return Ok(());
    }

    pub fn translate_block(
        &mut self,
        param_count: u32,
        block: &[TCStmt],
        block_loc: CodeLoc,
        cb_idx: u32, // docs for this later in the match statement: stands for continue-break
        mut ld_count: u32, // loop declaration count
    ) -> Vec<TaggedOpcode> {
        let mut ops = Vec::new();
        let mut decl_count = 0;

        macro_rules! cb {
            () => {{
                cb_idx + ops.len() as u32
            }};
        }

        for stmt in block {
            let mut tagged = TaggedOpcode {
                op: Opcode::StackDealloc,
                loc: stmt.loc,
            };

            match &stmt.kind {
                TCStmtKind::RetVal(expr) => {
                    ops.append(&mut self.translate_expr(expr));

                    let ret_idx = (param_count as i16 * -1) - 1;
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

                TCStmtKind::Decl { symbol, init } => {
                    decl_count += 1;
                    ld_count += 1;
                    let bytes = init.expr_type.size();
                    tagged.op = Opcode::StackAlloc {
                        bytes,
                        symbol: self.symbols.len() as u32,
                    };
                    self.symbols.push(RuntimeVar {
                        symbol: *symbol,
                        decl_type: init.expr_type,
                        loc: stmt.loc,
                    });
                    ops.push(tagged);
                    ops.append(&mut self.translate_expr(init));
                    tagged.op = Opcode::PopIntoTopVar { bytes, offset: 0 };
                    ops.push(tagged);
                }

                TCStmtKind::Branch {
                    cond,
                    if_body: if_,
                    else_body: else_,
                } => {
                    let cond_bytes = cond.expr_type.size();
                    ops.append(&mut self.translate_expr(cond));

                    // cb! + 1 because of conditional jump instruction
                    let mut if_ops =
                        self.translate_block(param_count, if_.stmts, if_.loc, cb!() + 1, ld_count);
                    let ifbr_len = if_ops.len() as u32 + 2;

                    tagged.op = match cond_bytes {
                        1 => Opcode::JumpIfZero8(ifbr_len),
                        2 => Opcode::JumpIfZero16(ifbr_len),
                        4 => Opcode::JumpIfZero32(ifbr_len),
                        8 => Opcode::JumpIfZero64(ifbr_len),
                        _ => unreachable!(),
                    };
                    ops.push(tagged);
                    ops.append(&mut if_ops);
                    mem::drop(if_ops);

                    // cb! + 1 because of jump instruction
                    let mut else_ops = self.translate_block(
                        param_count,
                        else_.stmts,
                        else_.loc,
                        cb!() + 1,
                        ld_count,
                    );
                    let elsebr_len = else_ops.len() as u32 + 1;

                    tagged.op = Opcode::Jump(elsebr_len);
                    ops.push(tagged);
                    ops.append(&mut else_ops);
                }

                TCStmtKind::Block(block) => {
                    let mut block =
                        self.translate_block(param_count, block.stmts, block.loc, cb!(), ld_count);
                    ops.append(&mut block);
                }

                TCStmtKind::Loop(block) => {
                    // cb_idx is the way we track loop structures and handle break and continue

                    tagged.op = Opcode::Jump(2);
                    ops.push(tagged);

                    let mut block = self.translate_block(param_count, block.stmts, block.loc, 0, 0);
                    tagged.op = Opcode::Jump(block.len() as u32 + 2); // break out of loop
                    ops.push(tagged);
                    tagged.op = Opcode::Jump(0u32.wrapping_sub(block.len() as u32));
                    ops.append(&mut block);
                    ops.push(tagged);
                }

                TCStmtKind::Break => {
                    tagged.op = Opcode::StackDealloc;
                    for _ in 0..ld_count {
                        ops.push(tagged);
                    }

                    tagged.op = Opcode::Jump(0u32.wrapping_sub(cb!() + 1));
                    ops.push(tagged);
                }
                TCStmtKind::Continue => {
                    tagged.op = Opcode::StackDealloc;
                    for _ in 0..ld_count {
                        ops.push(tagged);
                    }

                    tagged.op = Opcode::Jump(0u32.wrapping_sub(cb!()));
                    ops.push(tagged);
                }
            }
        }

        let dealloc = TaggedOpcode {
            op: Opcode::StackDealloc,
            loc: block_loc,
        };

        for _ in 0..decl_count {
            ops.push(dealloc);
        }

        return ops;
    }

    pub fn translate_expr(&mut self, expr: &TCExpr) -> Vec<TaggedOpcode> {
        let mut ops = Vec::new();
        let mut tagged = TaggedOpcode {
            op: Opcode::StackDealloc,
            loc: expr.loc,
        };

        match &expr.kind {
            TCExprKind::Uninit => {
                tagged.op = Opcode::PushUndef {
                    bytes: expr.expr_type.size(),
                };
                ops.push(tagged);
            }
            TCExprKind::IntLiteral(val) => {
                tagged.op = Opcode::MakeTempInt32(*val);
                ops.push(tagged);
            }
            TCExprKind::StringLiteral(val) => {
                let var = self.data.add_var(val.len() as u32 + 1, META_NO_SYMBOL); // TODO overflow here
                let slice = self.data.get_full_var_range_mut(var);
                let end = slice.len() - 1;
                slice[..end].copy_from_slice(val.as_bytes());
                slice[end] = 0;
                tagged.op = Opcode::MakeTempBinaryPtr { var, offset: 0 };
                ops.push(tagged);
            }
            TCExprKind::LocalIdent { var_offset } => {
                tagged.op = Opcode::GetLocal {
                    var: *var_offset,
                    offset: 0,
                    bytes: expr.expr_type.size(),
                };
                ops.push(tagged);
            }

            TCExprKind::List(exprs) => {
                for (idx, expr) in exprs.iter().enumerate() {
                    ops.append(&mut self.translate_expr(expr));
                    if idx + 1 < exprs.len() {
                        tagged.op = Opcode::Pop {
                            bytes: expr.expr_type.size(),
                        };
                        ops.push(tagged);
                    }
                }
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

            TCExprKind::LtI32(l, r) => {
                ops.append(&mut self.translate_expr(l));
                ops.append(&mut self.translate_expr(r));
                tagged.op = Opcode::CompI32;
                ops.push(tagged);
            }
            TCExprKind::EqI32(l, r) => {
                ops.append(&mut self.translate_expr(l));
                ops.append(&mut self.translate_expr(r));
                tagged.op = Opcode::CompEqI32;
                ops.push(tagged);
            }

            TCExprKind::NeqI32(l, r) => {
                ops.append(&mut self.translate_expr(l));
                ops.append(&mut self.translate_expr(r));
                tagged.op = Opcode::CompNeqI32;
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

            TCExprKind::Assign { target, value } => {
                ops.append(&mut self.translate_expr(value));
                ops.append(&mut self.translate_assign(target));
            }

            TCExprKind::Member { base, offset } => {
                let base_bytes = base.expr_type.size();
                ops.append(&mut self.translate_expr(base));
                let want_bytes = expr.expr_type.size();
                let top_bytes = base_bytes - want_bytes - offset;
                tagged.op = Opcode::Pop { bytes: top_bytes };
                ops.push(tagged);
                tagged.op = Opcode::PopKeep {
                    drop: *offset,
                    keep: want_bytes,
                };
                ops.push(tagged);
            }
            TCExprKind::PtrMember { base, offset } => {
                let bytes = expr.expr_type.size();
                ops.append(&mut self.translate_expr(base));
                tagged.op = Opcode::Get {
                    offset: *offset,
                    bytes,
                };
                ops.push(tagged);
            }

            TCExprKind::Deref(ptr) => {
                ops.append(&mut self.translate_expr(ptr));
                tagged.op = Opcode::Get {
                    offset: 0,
                    bytes: expr.expr_type.size(),
                };
                ops.push(tagged);
            }
            TCExprKind::Ref(lvalue) => match lvalue.kind {
                TCAssignKind::LocalIdent { var_offset } => {
                    tagged.op = Opcode::MakeTempLocalStackPtr {
                        var: var_offset,
                        offset: 0,
                    };
                    ops.push(tagged);
                }
                TCAssignKind::Ptr(expr) => {
                    ops.append(&mut self.translate_expr(expr));
                }
            },

            TCExprKind::Call {
                func,
                params,
                varargs,
            } => {
                let rtype_size = *self.func_types.get(&func).unwrap();
                tagged.op = Opcode::StackAlloc {
                    bytes: rtype_size,
                    symbol: META_NO_SYMBOL,
                };
                ops.push(tagged);

                for param in *params {
                    let bytes = param.expr_type.size();
                    tagged.op = Opcode::StackAlloc {
                        bytes,
                        symbol: META_NO_SYMBOL,
                    };
                    ops.push(tagged);
                    ops.append(&mut self.translate_expr(param));
                    tagged.op = Opcode::PopIntoTopVar { offset: 0, bytes };
                    ops.push(tagged);
                }

                if *varargs {
                    tagged.op = Opcode::StackAlloc {
                        bytes: 4,
                        symbol: META_NO_SYMBOL,
                    };
                    ops.push(tagged);
                    tagged.op = Opcode::MakeTempInt32(params.len() as i32); // check overflow here
                    ops.push(tagged);
                    tagged.op = Opcode::PopIntoTopVar {
                        offset: 0,
                        bytes: 4,
                    };
                    ops.push(tagged);
                }

                tagged.op = Opcode::Call(*func);
                ops.push(tagged);

                tagged.op = Opcode::StackDealloc;
                for _ in 0..params.len() {
                    ops.push(tagged);
                }

                if *varargs {
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

    pub fn translate_assign(&mut self, assign: &TCAssignTarget) -> Vec<TaggedOpcode> {
        let mut ops = Vec::new();
        let mut tagged = TaggedOpcode {
            op: Opcode::StackDealloc,
            loc: assign.target_loc,
        };

        match assign.kind {
            TCAssignKind::Ptr(expr) => {
                let bytes = assign.target_type.size();
                tagged.op = Opcode::PushDup { bytes };
                ops.push(tagged);
                ops.append(&mut self.translate_expr(expr));
                tagged.op = Opcode::Set { offset: 0, bytes };
                ops.push(tagged);
            }
            TCAssignKind::LocalIdent { var_offset } => {
                let (bytes, offset) = (assign.target_type.size(), assign.offset);
                tagged.op = Opcode::PushDup { bytes };
                ops.push(tagged);
                tagged.op = Opcode::SetLocal {
                    var: var_offset,
                    offset,
                    bytes,
                };
                ops.push(tagged);
            }
        }

        return ops;
    }

    pub fn assemble<'b>(mut self, env: &FileDb) -> Result<Program<'b>, Error> {
        let no_main = || error!("missing main function definition");
        let main_sym = INIT_SYMS.translate["main"];
        let main_func = self.functions.get(&main_sym).ok_or_else(no_main)?;
        let (main_idx, main_loc) = main_func.func_header.ok_or_else(no_main)?;
        let mut opcodes: Vec<TaggedOpcode> = init_main_no_args(main_sym)
            .iter()
            .map(|&op| TaggedOpcode { op, loc: main_loc })
            .collect();
        let runtime_length = opcodes.len() as u32; // No overflow here because len is predefined
        opcodes.append(&mut self.opcodes);

        for (op_idx, op) in opcodes.iter_mut().enumerate() {
            let op_idx = op_idx as u32;
            match &mut op.op {
                Opcode::Call(addr) => {
                    let function = self.functions.get(addr).unwrap();
                    if let Some(func_header) = function.func_header {
                        let (fptr, _loc) = func_header;
                        *addr = fptr + runtime_length;
                    } else if LIB_FUNCS.contains(addr) {
                        op.op = Opcode::LibCall(*addr);
                    } else {
                        let func_loc = function.func_type.loc;
                        return Err(error!(
                            "couldn't find definition for function",
                            op.loc, "called here", func_loc, "declared here"
                        ));
                    }
                }
                Opcode::Jump(target) => {
                    *target = op_idx.wrapping_add(*target);
                }
                Opcode::JumpIfZero8(target) => {
                    *target = op_idx.wrapping_add(*target);
                }
                Opcode::JumpIfZero16(target) => {
                    *target = op_idx.wrapping_add(*target);
                }
                Opcode::JumpIfZero32(target) => {
                    *target = op_idx.wrapping_add(*target);
                }
                Opcode::JumpIfZero64(target) => {
                    *target = op_idx.wrapping_add(*target);
                }
                Opcode::JumpIfNotZero8(target) => {
                    *target = op_idx.wrapping_add(*target);
                }
                Opcode::JumpIfNotZero16(target) => {
                    *target = op_idx.wrapping_add(*target);
                }
                Opcode::JumpIfNotZero32(target) => {
                    *target = op_idx.wrapping_add(*target);
                }
                Opcode::JumpIfNotZero64(target) => {
                    *target = op_idx.wrapping_add(*target);
                }
                _ => {}
            }
        }
        let file_size = align_usize(env.size(), align_of::<HashRefSlot<u32, RuntimeStruct>>());

        let type_struct_slots = self.types.len() * 2;
        let type_structs_size = align_usize(
            type_struct_slots * size_of::<HashRefSlot<u32, RuntimeStruct>>(),
            align_of::<TCStructMember>(),
        );
        let type_members_size = align_usize(
            self.struct_member_count * size_of::<TCStructMember>(),
            align_of::<RuntimeVar>(),
        );
        let types_size = type_members_size + type_structs_size;

        let symbols_size = align_usize(
            self.symbols.len() * size_of::<RuntimeVar>(),
            align_of::<TaggedOpcode>(),
        );

        let opcodes_size = opcodes.len() * size_of::<TaggedOpcode>();

        let var_data_size = align_usize(self.data.data.len(), align_of::<Var>());
        let vars_size = self.data.vars.len() * size_of::<Var>();
        let data_size = var_data_size + vars_size;

        let total_size = file_size + types_size + symbols_size + opcodes_size + data_size;
        let buckets = BucketList::with_capacity(0);
        let layout = alloc::Layout::from_size_align(total_size, 8).unwrap();
        let mut frame = buckets.alloc_frame(layout);

        macro_rules! type_mapper {
            () => {
                |(id, value)| {
                    let runtime_struct = RuntimeStruct {
                        members: value.members.map(|members| &*frame.add_slice(members)),
                        loc: value.loc,
                        sa: value.sa,
                    };

                    (*id, runtime_struct)
                }
            };
        }

        let files = FileDbRef::new_from_frame(&mut frame, env);
        let types: Vec<(u32, RuntimeStruct)> = self.types.iter().map(type_mapper!()).collect();
        let types = HashRef::new(&mut frame, type_struct_slots, types.into_iter());
        let symbols = frame.add_array(self.symbols);
        let ops = frame.add_array(opcodes);
        let data = self.data.write_to_ref(frame);

        let program = Program {
            buckets,
            files,
            types,
            symbols,
            data,
            ops,
        };

        return Ok(program);
    }
}
