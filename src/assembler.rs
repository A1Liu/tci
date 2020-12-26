use crate::buckets::*;
use crate::filedb::*;
use crate::interpreter::*;
use crate::runtime::*;
use crate::tc_ast::*;
use crate::type_checker::*;
use crate::util::*;
use core::mem::{align_of, size_of};
use core::{alloc, mem};
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct ASMFunc {
    pub func_type: TCFuncType,
    pub func_header: Option<(u32, CodeLoc)>, // first u32 points into opcodes buffer
}

lazy_static! {
    pub static ref LIB_FUNCS: HashSet<u32> = {
        let mut m = HashSet::new();
        m.insert(INIT_SYMS.translate["printf"]);
        m
    };
}

pub fn init_main_no_args(main_sym: u32) -> Vec<Opcode> {
    return vec![
        Opcode::StackAlloc {
            bytes: 4,
            symbol: META_NO_SYMBOL,
        },
        Opcode::StackAlloc {
            bytes: 8,
            symbol: META_NO_SYMBOL,
        },
        Opcode::MakeTempStackFpPtr { var: 0, offset: 0 },
        Opcode::PopIntoTopVar {
            offset: 0,
            bytes: 8,
        },
        Opcode::Call(main_sym),
        Opcode::StackDealloc,
        Opcode::GetLocal {
            var: 0,
            offset: 0,
            bytes: 4,
        },
        Opcode::MakeTempU32(ECALL_EXIT),
        Opcode::EcallDyn,
    ];
}

#[derive(Debug, Clone)]
pub struct ASMRuntimeStruct {
    pub members: Option<Vec<TCStructMember>>,
    pub loc: CodeLoc,
    pub sa: SizeAlign,
}

pub struct Assembler {
    pub opcodes: Vec<TaggedOpcode>,
    pub func_types: HashMap<u32, u32>,
    pub data: VarBuffer,
    pub functions: HashMap<u32, ASMFunc>, // keys are identifier symbols
    pub types: HashMap<u32, ASMRuntimeStruct>,
    pub symbols: Vec<RuntimeVar>,
    pub struct_member_count: usize,
}

impl Assembler {
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

    pub fn add_file(&mut self, typed_ast: TypedFuncs) -> Result<(), Error> {
        for (struct_id, struct_type) in typed_ast.types.structs.into_iter() {
            let struct_type = match struct_type.defn {
                Some(struct_type) => struct_type,
                None => {
                    if !self.types.contains_key(&struct_id) {
                        self.types.insert(
                            struct_id,
                            ASMRuntimeStruct {
                                members: None,
                                loc: struct_type.decl_loc,
                                sa: TC_UNKNOWN_SA,
                            },
                        );
                    }

                    continue;
                }
            };

            let member_count = struct_type.members.len();
            if let Some(ASMRuntimeStruct { members, loc, sa }) = self.types.get_mut(&struct_id) {
                if let Some(members) = members {
                    if members != &struct_type.members {
                        return Err(error!(
                            "multiple, conflicting definitions of same type",
                            *loc,
                            "first one found here",
                            struct_type.meta.loc,
                            "second one found here"
                        ));
                    }
                } else {
                    self.struct_member_count += member_count;
                    *members = Some(struct_type.members);
                    *loc = struct_type.meta.loc;
                    *sa = struct_type.meta.sa;
                }

                continue;
            }

            self.types.insert(
                struct_id,
                ASMRuntimeStruct {
                    members: Some(struct_type.members),
                    loc: struct_type.meta.loc,
                    sa: struct_type.meta.sa,
                },
            );
            self.struct_member_count += member_count;
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

    pub fn add_function(&mut self, ident: u32, func: TCFunc) -> Result<(), Error> {
        let asm_func = match self.functions.get_mut(&ident) {
            Some(asm_func) => {
                if asm_func.func_type != func.func_type {
                    let error = func_decl_mismatch(asm_func.func_type.loc, func.func_type.loc);
                    return Err(error);
                }
                asm_func
            }
            None => {
                self.functions.insert(
                    ident,
                    ASMFunc {
                        func_type: func.func_type,
                        func_header: None,
                    },
                );

                self.functions.get_mut(&ident).unwrap()
            }
        };

        let defn = match func.defn {
            Some(defn) => defn,
            None => return Ok(()),
        };

        if let Some((_func_header, defn_loc)) = &asm_func.func_header {
            return Err(func_redef(*defn_loc, defn.loc));
        }

        asm_func.func_header = Some((self.opcodes.len() as u32, defn.loc));
        let param_count = asm_func.func_type.params.len() as u32;

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

                    tagged.op = Opcode::GetLocal {
                        var: -1,
                        offset: 0,
                        bytes: 8,
                    };
                    ops.push(tagged);
                    tagged.op = Opcode::Set {
                        offset: 0,
                        bytes: expr.expr_type.repr_size(),
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
                        bytes: expr.expr_type.repr_size(),
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
                    let cond_bytes = cond.expr_type.repr_size();
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

    pub fn translate_bin_op(
        &self,
        op: BinOp,
        op_type: TCPrimType,
        ops: &mut Vec<TaggedOpcode>,
        loc: CodeLoc,
    ) {
        let mut tagged = TaggedOpcode {
            op: Opcode::StackDealloc,
            loc,
        };

        tagged.op = match (op, op_type) {
            (BinOp::Add, TCPrimType::U32) => Opcode::AddU32,
            (BinOp::Add, TCPrimType::I32) => Opcode::AddU32,
            (BinOp::Add, TCPrimType::U64) => Opcode::AddU64,
            (BinOp::Add, TCPrimType::I64) => Opcode::AddU64,
            (BinOp::Add, TCPrimType::Pointer { .. }) => Opcode::AddU64,

            (BinOp::Sub, TCPrimType::I32) => Opcode::SubI32,
            (BinOp::Sub, TCPrimType::I64) => Opcode::SubI64,
            (BinOp::Sub, TCPrimType::U64) => Opcode::SubU64,
            (BinOp::Sub, TCPrimType::Pointer { .. }) => Opcode::SubU64,

            (BinOp::Mul, TCPrimType::I32) => Opcode::MulI32,
            (BinOp::Mul, TCPrimType::I64) => Opcode::MulI64,
            (BinOp::Mul, TCPrimType::U64) => Opcode::MulU64,

            (BinOp::Div, TCPrimType::I32) => Opcode::DivI32,
            (BinOp::Div, TCPrimType::U64) => Opcode::DivU64,

            (BinOp::Eq, TCPrimType::I32) => Opcode::CompEq32,
            (BinOp::Eq, TCPrimType::I64) => Opcode::CompEq64,
            (BinOp::Eq, TCPrimType::Pointer { .. }) => Opcode::CompEq64,

            (BinOp::Neq, TCPrimType::I32) => Opcode::CompNeq32,
            (BinOp::Neq, TCPrimType::U64) => Opcode::CompNeq64,

            (BinOp::Gt, TCPrimType::I32) => {
                tagged.op = Opcode::Swap { top: 4, bottom: 4 };
                ops.push(tagged);
                Opcode::CompLtI32
            }
            (BinOp::Gt, TCPrimType::U64) => {
                tagged.op = Opcode::Swap { top: 8, bottom: 8 };
                ops.push(tagged);
                Opcode::CompLtU64
            }

            (BinOp::Lt, TCPrimType::I32) => Opcode::CompLtI32,
            (BinOp::Lt, TCPrimType::U64) => Opcode::CompLtU64,

            (BinOp::Leq, TCPrimType::I32) => Opcode::CompLeqI32,

            (BinOp::Geq, TCPrimType::I32) => {
                tagged.op = Opcode::Swap { top: 4, bottom: 4 };
                ops.push(tagged);
                Opcode::CompLeqI32
            }
            (BinOp::Geq, TCPrimType::U64) => {
                tagged.op = Opcode::Swap { top: 8, bottom: 8 };
                ops.push(tagged);
                Opcode::CompLeqU64
            }

            (BinOp::LShift, TCPrimType::I32) => Opcode::LShiftI32,

            (BinOp::RShift, TCPrimType::I32) => Opcode::RShiftI32,

            (BinOp::BitAnd, TCPrimType::I8) => Opcode::BitAndI8,
            (BinOp::BitAnd, TCPrimType::I32) => Opcode::BitAndI32,

            (BinOp::BitOr, TCPrimType::I8) => Opcode::BitOrI8,
            (BinOp::BitOr, TCPrimType::I32) => Opcode::BitOrI32,

            (BinOp::BitXor, TCPrimType::I32) => Opcode::BitXorI32,

            (BinOp::BoolAnd, TCPrimType::I8) => Opcode::BitAndI8,
            (BinOp::BoolOr, TCPrimType::I8) => Opcode::BitOrI8,

            (op, ptype) => unreachable!("op={:?} type={:?}", op, ptype),
        };
        ops.push(tagged);
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
                    bytes: expr.expr_type.repr_size(),
                };
                ops.push(tagged);
            }
            TCExprKind::I8Literal(val) => {
                tagged.op = Opcode::MakeTempI8(*val);
                ops.push(tagged);
            }
            TCExprKind::I32Literal(val) => {
                tagged.op = Opcode::MakeTempI32(*val);
                ops.push(tagged);
            }
            TCExprKind::I64Literal(val) => {
                tagged.op = Opcode::MakeTempI64(*val);
                ops.push(tagged);
            }
            TCExprKind::U64Literal(val) => {
                tagged.op = Opcode::MakeTempU64(*val);
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
                tagged.op = if expr.expr_type.is_array() {
                    Opcode::MakeTempStackFpPtr {
                        var: *var_offset,
                        offset: 0,
                    }
                } else {
                    Opcode::GetLocal {
                        var: *var_offset,
                        offset: 0,
                        bytes: expr.expr_type.repr_size(),
                    }
                };

                ops.push(tagged);
            }

            TCExprKind::Array(exprs) => {
                for expr in *exprs {
                    ops.append(&mut self.translate_expr(expr));
                }
            }
            TCExprKind::BraceList(_) => unreachable!(),
            TCExprKind::ParenList(exprs) => {
                for (idx, expr) in exprs.iter().enumerate() {
                    ops.append(&mut self.translate_expr(expr));
                    let bytes = expr.expr_type.repr_size();

                    if idx + 1 < exprs.len() {
                        tagged.op = Opcode::Pop { bytes };
                        ops.push(tagged);
                    }
                }
            }

            TCExprKind::BinOp {
                op,
                op_type,
                left,
                right,
            } => {
                ops.append(&mut self.translate_expr(left));
                ops.append(&mut self.translate_expr(right));
                self.translate_bin_op(*op, *op_type, &mut ops, tagged.loc);
            }

            TCExprKind::Conv { from, to, expr } => {
                ops.append(&mut self.translate_expr(expr));
                let opcode = match (from, to.size()) {
                    (TCPrimType::U8, 1) => None,
                    (TCPrimType::U8, 4) => Some(Opcode::ZExtend8To32),
                    (TCPrimType::U8, 8) => Some(Opcode::ZExtend8To64),
                    (TCPrimType::I8, 1) => None,
                    (TCPrimType::I8, 4) => Some(Opcode::SExtend8To32),
                    (TCPrimType::I8, 8) => Some(Opcode::SExtend8To64),

                    (TCPrimType::U32, 1) => Some(Opcode::Pop { bytes: 3 }),
                    (TCPrimType::U32, 4) => None,
                    (TCPrimType::U32, 8) => Some(Opcode::ZExtend32To64),
                    (TCPrimType::I32, 1) => Some(Opcode::Pop { bytes: 3 }),
                    (TCPrimType::I32, 4) => None,
                    (TCPrimType::I32, 8) => Some(Opcode::SExtend32To64),

                    (TCPrimType::U64, 1) => Some(Opcode::Pop { bytes: 7 }),
                    (TCPrimType::U64, 4) => Some(Opcode::Pop { bytes: 4 }),
                    (TCPrimType::U64, 8) => None,
                    (TCPrimType::I64, 1) => Some(Opcode::Pop { bytes: 7 }),
                    (TCPrimType::I64, 4) => Some(Opcode::Pop { bytes: 4 }),
                    (TCPrimType::I64, 8) => None,

                    (TCPrimType::Pointer { .. }, 1) => Some(Opcode::Pop { bytes: 7 }),
                    (TCPrimType::Pointer { .. }, 4) => Some(Opcode::Pop { bytes: 4 }),
                    (TCPrimType::Pointer { .. }, 8) => None,
                    (_, _) => unreachable!(),
                };

                if let Some(opcode) = opcode {
                    tagged.op = opcode;
                    ops.push(tagged);
                }
            }

            TCExprKind::PostIncrU32(target) => {
                ops.append(&mut self.translate_assign(target));
                tagged.op = Opcode::PushDup { bytes: 8 };
                ops.push(tagged);
                let bytes = 4;
                tagged.op = Opcode::Get { offset: 0, bytes };
                ops.push(tagged);
                tagged.op = Opcode::PushDup { bytes };
                ops.push(tagged);
                tagged.op = Opcode::MakeTempU32(1);
                ops.push(tagged);
                tagged.op = Opcode::AddU32;
                ops.push(tagged);
                let top = bytes * 2;
                tagged.op = Opcode::Swap { top, bottom: 8 };
                ops.push(tagged);
                tagged.op = Opcode::Set { offset: 0, bytes };
                ops.push(tagged);
            }

            TCExprKind::PostIncrU64(target) => {
                ops.append(&mut self.translate_assign(target));
                tagged.op = Opcode::PushDup { bytes: 8 };
                ops.push(tagged);
                let bytes = 8;
                tagged.op = Opcode::Get { offset: 0, bytes };
                ops.push(tagged);
                tagged.op = Opcode::PushDup { bytes };
                ops.push(tagged);
                tagged.op = Opcode::MakeTempU64(1);
                ops.push(tagged);
                tagged.op = Opcode::AddU64;
                ops.push(tagged);
                let top = bytes * 2;
                tagged.op = Opcode::Swap { top, bottom: 8 };
                ops.push(tagged);
                tagged.op = Opcode::Set { offset: 0, bytes };
                ops.push(tagged);
            }

            TCExprKind::Assign { target, value } => {
                ops.append(&mut self.translate_expr(value));
                let bytes = value.expr_type.repr_size();
                tagged.op = Opcode::PushDup { bytes };
                ops.push(tagged);
                ops.append(&mut self.translate_assign(target));
                tagged.op = Opcode::Set { offset: 0, bytes };
                ops.push(tagged);
            }

            TCExprKind::MutAssign {
                target,
                value,
                op,
                op_type,
            } => {
                ops.append(&mut self.translate_assign(target));
                tagged.op = Opcode::PushDup { bytes: 8 };
                ops.push(tagged);

                let bytes = target.target_type.size();
                tagged.op = Opcode::Get { offset: 0, bytes };
                ops.push(tagged);

                ops.append(&mut self.translate_expr(value));

                self.translate_bin_op(*op, *op_type, &mut ops, tagged.loc);

                tagged.op = Opcode::PushDup { bytes };
                ops.push(tagged);

                let top = bytes * 2;
                tagged.op = Opcode::Swap { top, bottom: 8 };
                ops.push(tagged);

                tagged.op = Opcode::Set { offset: 0, bytes };
                ops.push(tagged);
            }

            TCExprKind::Ternary {
                condition,
                if_true,
                if_false,
            } => {
                let cond_bytes = condition.expr_type.repr_size();
                ops.append(&mut self.translate_expr(condition));

                let mut if_ops = self.translate_expr(if_true);
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

                let mut else_ops = self.translate_expr(if_false);
                let elsebr_len = else_ops.len() as u32 + 1;

                tagged.op = Opcode::Jump(elsebr_len);
                ops.push(tagged);
                ops.append(&mut else_ops);
            }

            TCExprKind::Member { base, offset } => {
                let base_bytes = base.expr_type.repr_size();
                ops.append(&mut self.translate_expr(base));
                let want_bytes = expr.expr_type.repr_size();
                let top_bytes = base_bytes - want_bytes - offset;
                tagged.op = Opcode::Pop { bytes: top_bytes };
                ops.push(tagged);
                tagged.op = Opcode::PopKeep {
                    drop: *offset,
                    keep: want_bytes,
                };
                ops.push(tagged);
            }
            &TCExprKind::PtrMember { base, offset } => {
                let bytes = expr.expr_type.repr_size();
                ops.append(&mut self.translate_expr(base));
                if expr.expr_type.is_array() {
                    tagged.op = Opcode::MakeTempU64(offset as u64);
                    ops.push(tagged);
                    tagged.op = Opcode::AddU64;
                    ops.push(tagged);
                } else {
                    tagged.op = Opcode::Get {
                        offset: offset,
                        bytes,
                    };
                    ops.push(tagged);
                }
            }

            TCExprKind::Deref(ptr) => {
                ops.append(&mut self.translate_expr(ptr));
                tagged.op = Opcode::Get {
                    offset: 0,
                    bytes: expr.expr_type.repr_size(),
                };
                ops.push(tagged);
            }
            TCExprKind::Ref(lvalue) => match lvalue.kind {
                TCAssignTargetKind::LocalIdent { var_offset } => {
                    tagged.op = Opcode::MakeTempStackFpPtr {
                        var: var_offset,
                        offset: 0,
                    };
                    ops.push(tagged);
                }
                TCAssignTargetKind::Ptr(expr) => {
                    ops.append(&mut self.translate_expr(expr));
                }
            },

            TCExprKind::Call {
                func,
                params,
                named_count,
            } => {
                let rtype_size = expr.expr_type.repr_size();

                tagged.op = Opcode::StackAlloc {
                    bytes: rtype_size,
                    symbol: META_NO_SYMBOL,
                };
                ops.push(tagged);

                // Safety allocation to make sure varargs don't run off the stack
                tagged.op = Opcode::StackAlloc {
                    bytes: 0,
                    symbol: META_NO_SYMBOL,
                };
                ops.push(tagged);

                for (idx, param) in params.iter().rev().enumerate() {
                    let bytes = param.expr_type.repr_size();

                    tagged.op = Opcode::StackAlloc {
                        bytes,
                        symbol: META_NO_SYMBOL, // TODO this should be the parameter symbol
                    };
                    ops.push(tagged);
                    ops.append(&mut self.translate_expr(param));
                    tagged.op = Opcode::PopIntoTopVar { offset: 0, bytes };
                    ops.push(tagged);
                }

                // Create a pointer to the return location
                // PERFORMANCE technically this isn't necessary when the function
                // being called isn't a vararg function
                tagged.op = Opcode::StackAlloc {
                    bytes: 8,
                    symbol: META_NO_SYMBOL,
                };
                ops.push(tagged);
                tagged.op = Opcode::MakeTempStackSpPtr {
                    var: params.len() as i16 * -1 - 3,
                    offset: 0,
                };
                ops.push(tagged);
                tagged.op = Opcode::PopIntoTopVar {
                    offset: 0,
                    bytes: 8,
                };
                ops.push(tagged);

                tagged.op = Opcode::Call(*func);
                ops.push(tagged);

                tagged.op = Opcode::StackDealloc;
                for _ in 0..params.len() {
                    ops.push(tagged);
                }
                ops.push(tagged); // for the pointer to the return location
                ops.push(tagged); // for the safety allocation

                if rtype_size == 0 {
                    //
                    tagged.op = Opcode::StackDealloc;
                    ops.push(tagged);
                } else {
                    tagged.op = Opcode::StackAddToTemp;
                    ops.push(tagged);
                }
            }

            // TODO
            TCExprKind::Builtin(TCBuiltin::Ecall(ecall)) => {
                ops.append(&mut self.translate_expr(ecall));
                tagged.op = Opcode::EcallDyn;
                ops.push(tagged);
            }
            TCExprKind::Builtin(TCBuiltin::PushTempStack { ptr, size }) => {
                ops.append(&mut self.translate_expr(size));
                ops.append(&mut self.translate_expr(ptr));

                tagged.op = Opcode::PushDyn;
                ops.push(tagged);
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
            TCAssignTargetKind::Ptr(expr) => {
                ops.append(&mut self.translate_expr(expr));
                if assign.offset != 0 {
                    tagged.op = Opcode::MakeTempU64(assign.offset as u64);
                    ops.push(tagged);
                    tagged.op = Opcode::AddU64;
                    ops.push(tagged);
                }
            }
            TCAssignTargetKind::LocalIdent { var_offset } => {
                tagged.op = Opcode::MakeTempStackFpPtr {
                    var: var_offset,
                    offset: assign.offset,
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
                    let mapper = |members: &Vec<_>| &*frame.add_slice(members);
                    let runtime_struct = RuntimeStruct {
                        members: value.members.as_ref().map(mapper),
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
