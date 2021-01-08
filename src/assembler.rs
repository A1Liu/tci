use crate::buckets::*;
use crate::filedb::*;
use crate::interpreter::*;
use crate::runtime::*;
use crate::tc_ast::*;
use crate::util::*;

#[derive(Debug)]
pub struct ASMFunc {
    pub func_type: TCFuncType,
    pub decl_loc: CodeLoc,
    pub func_header: Option<(u32, CodeLoc)>, // first u32 points into opcodes buffer
}

pub struct Assembler {
    pub buckets: BucketListFactory,
    pub opcodes: Vec<TaggedOpcode>,
    pub func_linkage: HashMap<LinkName, u32>,
    pub functions: Vec<ASMFunc>,
    pub data: VarBuffer,
    pub symbols: Vec<RuntimeVar>,
}

#[derive(Debug, Clone, Copy)]
pub struct ASMEnv<'a> {
    var_offsets: &'a [i16],
    binary_offsets: &'a HashMap<u32, u32>,
}

impl Drop for Assembler {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

pub fn init_main_no_args(main_idx: u32) -> Vec<Opcode> {
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
        Opcode::Call(main_idx),
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

impl Assembler {
    pub fn new() -> Self {
        Self {
            buckets: BucketListFactory::new(),
            opcodes: Vec::new(),
            data: VarBuffer::new(),
            func_linkage: HashMap::new(),
            functions: Vec::new(),
            symbols: Vec::new(),
        }
    }

    pub fn add_file(&mut self, file: u32, mut tu: TranslationUnit) -> Result<(), Error> {
        // Add function return sizes

        let mut link_names = HashMap::new();
        let mut defns = Vec::new();

        for (ident, tc_func) in std::mem::replace(&mut tu.functions, HashMap::new()) {
            let link_name = if tc_func.is_static {
                LinkName::new_static(ident, file)
            } else {
                LinkName::new(ident)
            };

            link_names.insert(ident, link_name);

            let prev = match self.func_linkage.entry(link_name) {
                Entry::Occupied(o) => &self.functions[*o.get() as usize],
                Entry::Vacant(v) => {
                    v.insert(self.functions.len() as u32);
                    self.functions.push(ASMFunc {
                        func_type: tc_func.func_type.clone_into_alloc(&*self.buckets),
                        decl_loc: tc_func.decl_loc,
                        func_header: None,
                    });

                    if let Some(defn) = tc_func.defn {
                        defns.push((link_name, defn));
                    }

                    continue;
                }
            };

            if prev.func_type != tc_func.func_type {
                let error = func_decl_mismatch(prev.decl_loc, tc_func.decl_loc);
                return Err(error);
            }

            if let Some(defn) = tc_func.defn {
                defns.push((link_name, defn));
            }
        }

        for (link_name, defn) in defns {
            let header = self.functions[self.func_linkage[&link_name] as usize].func_header;
            if let Some((_, defn_loc)) = header.as_ref() {
                return Err(func_redef(*defn_loc, defn.loc));
            }

            let opcode = self.opcodes.len() as u32;

            self.opcodes.push(TaggedOpcode {
                op: Opcode::Func(link_name),
                loc: defn.loc,
            });

            self.add_function(&link_names, &defn);
            let header = &mut self.functions[self.func_linkage[&link_name] as usize].func_header;
            *header = Some((opcode, defn.loc));
        }

        return Ok(());
    }

    pub fn add_function(&mut self, link_names: &HashMap<u32, LinkName>, defn: &TCFuncDefn) {
        if defn.ops.len() < 2 {
            unreachable!()
        }

        let jumps: Vec<u32> = Vec::new();
        let mut var_offsets: Vec<i16> = (0..defn.sym_count).map(|_| i16::MAX).collect();
        let mut next_offset = 0;

        for idx in 0..defn.param_count {
            var_offsets[idx as usize] = -(idx as i16) - 2;
        }

        for t_op in defn.ops {
            let mut op = TaggedOpcode::new(Opcode::Ret, t_op.loc);

            match t_op.kind {
                TCOpcodeKind::ScopeBegin(vars, scope_end) => {
                    for (&var, &ty) in vars.iter() {
                        op.op = Opcode::StackAlloc {
                            bytes: ty.size().into(),
                            symbol: self.symbols.len() as u32,
                        };

                        self.opcodes.push(op);
                        var_offsets[var as usize] = next_offset;
                        next_offset += 1;

                        // TODO add runtime var support
                        // self.symbols.push(RuntimeVar {
                        //     symbol: *symbol,
                        //     decl_type: ty.clone_into_alloc(&*self.buckets),
                        //     loc: t_op.loc,
                        // });
                    }
                }
                TCOpcodeKind::ScopeEnd { count, begin } => {
                    op.op = Opcode::StackDealloc;

                    for _ in 0..count {
                        self.opcodes.push(op);
                    }

                    next_offset -= count as i16;
                }
                TCOpcodeKind::Ret => {
                    op.op = Opcode::Ret;
                    self.opcodes.push(op);
                }
                TCOpcodeKind::RetVal(val) => {
                    self.translate_expr(&var_offsets, &val);

                    op.op = Opcode::GetLocal {
                        var: -1,
                        offset: 0,
                        bytes: 8,
                    };
                    self.opcodes.push(op);
                    op.op = Opcode::Set {
                        offset: 0,
                        bytes: val.ty.repr_size(),
                    };
                    self.opcodes.push(op);
                    op.op = Opcode::Ret;
                    self.opcodes.push(op);
                }
                TCOpcodeKind::Expr(expr) => {
                    self.translate_expr(&var_offsets, &expr);

                    let bytes = expr.ty.repr_size();
                    op.op = Opcode::Pop { bytes };
                    self.opcodes.push(op);
                }
                _ => {}
            }
        }
    }

    pub fn translate_expr(&mut self, var_offsets: &[i16], expr: &TCExpr) {
        let mut tagged = TaggedOpcode::new(Opcode::Ret, expr.loc);

        match &expr.kind {
            TCExprKind::I8Literal(val) => {
                tagged.op = Opcode::MakeTempI8(*val);
                self.opcodes.push(tagged);
            }
            TCExprKind::I32Literal(val) => {
                tagged.op = Opcode::MakeTempI32(*val);
                self.opcodes.push(tagged);
            }
            TCExprKind::I64Literal(val) => {
                tagged.op = Opcode::MakeTempI64(*val);
                self.opcodes.push(tagged);
            }
            TCExprKind::U64Literal(val) => {
                tagged.op = Opcode::MakeTempU64(*val);
                self.opcodes.push(tagged);
            }
            TCExprKind::StringLiteral(val) => {
                // TODO check overflow here
                let var = self.data.add_var(val.len() as u32 + 1, META_NO_SYMBOL);
                let slice = self.data.get_full_var_range_mut(var);
                let end = slice.len() - 1;
                slice[..end].copy_from_slice(val.as_bytes());
                slice[end] = 0;
                tagged.op = Opcode::MakeTempBinaryPtr { var, offset: 0 };
                self.opcodes.push(tagged);
            }
            TCExprKind::LocalIdent { label } => {
                let var = var_offsets[*label as usize];
                tagged.op = if expr.ty.is_array() {
                    Opcode::MakeTempStackFpPtr { var, offset: 0 }
                } else {
                    Opcode::GetLocal {
                        var,
                        offset: 0,
                        bytes: expr.ty.repr_size(),
                    }
                };

                self.opcodes.push(tagged);
            }

            TCExprKind::ParenList(exprs) => {
                for (idx, expr) in exprs.iter().enumerate() {
                    self.translate_expr(var_offsets, expr);
                    let bytes = expr.ty.repr_size();

                    if idx + 1 < exprs.len() {
                        tagged.op = Opcode::Pop { bytes };
                        self.opcodes.push(tagged);
                    }
                }
            }

            TCExprKind::BinOp {
                op,
                op_type,
                left,
                right,
            } => {
                self.translate_expr(var_offsets, left);
                self.translate_expr(var_offsets, right);
                self.translate_bin_op(*op, *op_type, tagged.loc);
            }

            TCExprKind::Conv { from, to, expr } => {
                self.translate_expr(var_offsets, expr);
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
                    self.opcodes.push(tagged);
                }
            }

            TCExprKind::Assign { target, value } => {
                self.translate_expr(var_offsets, value);
                let bytes = value.ty.repr_size();
                tagged.op = Opcode::PushDup { bytes };
                self.opcodes.push(tagged);
                self.translate_assign(var_offsets, target);
                tagged.op = Opcode::Set { offset: 0, bytes };
                self.opcodes.push(tagged);
            }

            TCExprKind::Deref(ptr) => {
                self.translate_expr(var_offsets, ptr);
                tagged.op = Opcode::Get {
                    offset: 0,
                    bytes: expr.ty.size().into(),
                };
                self.opcodes.push(tagged);
            }
            TCExprKind::Ref(lvalue) => self.translate_assign(var_offsets, lvalue),

            TCExprKind::Call { func, params } => {
                self.translate_expr(var_offsets, func); // callee is sequenced before params

                let rtype_size = expr.ty.repr_size();
                tagged.op = Opcode::StackAlloc {
                    bytes: rtype_size,
                    symbol: META_NO_SYMBOL,
                };
                self.opcodes.push(tagged);

                // Safety allocation to make sure varargs don't run off the stack
                tagged.op = Opcode::StackAlloc {
                    bytes: 0,
                    symbol: META_NO_SYMBOL,
                };
                self.opcodes.push(tagged);

                for (idx, param) in params.iter().rev().enumerate() {
                    let bytes = param.ty.repr_size();

                    tagged.op = Opcode::StackAlloc {
                        bytes,
                        symbol: META_NO_SYMBOL, // TODO this should be the parameter symbol
                    };
                    self.opcodes.push(tagged);
                    self.translate_expr(var_offsets, param);
                    tagged.op = Opcode::PopIntoTopVar { offset: 0, bytes };
                    self.opcodes.push(tagged);
                }

                // Create a pointer to the return location
                tagged.op = Opcode::StackAlloc {
                    bytes: 8,
                    symbol: META_NO_SYMBOL,
                };
                self.opcodes.push(tagged);
                tagged.op = Opcode::MakeTempStackSpPtr {
                    var: params.len() as i16 * -1 - 3,
                    offset: 0,
                };
                self.opcodes.push(tagged);
                tagged.op = Opcode::PopIntoTopVar {
                    offset: 0,
                    bytes: 8,
                };
                self.opcodes.push(tagged);

                tagged.op = Opcode::CallDyn;
                self.opcodes.push(tagged);

                tagged.op = Opcode::StackDealloc;
                for _ in 0..params.len() {
                    self.opcodes.push(tagged);
                }
                self.opcodes.push(tagged); // for the pointer to the return location
                self.opcodes.push(tagged); // for the safety allocation

                if rtype_size == 0 {
                    //
                    tagged.op = Opcode::StackDealloc;
                    self.opcodes.push(tagged);
                } else {
                    tagged.op = Opcode::StackAddToTemp;
                    self.opcodes.push(tagged);
                }
            }
            x => unimplemented!("{:?}", x),
        }
    }

    pub fn translate_assign(&mut self, var_offsets: &[i16], assign: &TCAssignTarget) {
        let mut tagged = TaggedOpcode::new(Opcode::Ret, assign.loc);

        match assign.kind {
            TCAssignTargetKind::Ptr(expr) => {
                self.translate_expr(var_offsets, expr);
                if assign.offset != 0 {
                    tagged.op = Opcode::MakeTempU64(assign.offset as u64);
                    self.opcodes.push(tagged);
                    tagged.op = Opcode::AddU64;
                    self.opcodes.push(tagged);
                }
            }
            TCAssignTargetKind::LocalIdent { label } => {
                let var = var_offsets[label as usize];
                tagged.op = Opcode::MakeTempStackFpPtr {
                    var,
                    offset: assign.offset,
                };
                self.opcodes.push(tagged);
            }
            _ => unimplemented!(),
        }
    }

    pub fn translate_bin_op(&mut self, op: BinOp, op_type: TCPrimType, loc: CodeLoc) {
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
                self.opcodes.push(tagged);
                Opcode::CompLtI32
            }
            (BinOp::Gt, TCPrimType::U64) => {
                tagged.op = Opcode::Swap { top: 8, bottom: 8 };
                self.opcodes.push(tagged);
                Opcode::CompLtU64
            }

            (BinOp::Lt, TCPrimType::I32) => Opcode::CompLtI32,
            (BinOp::Lt, TCPrimType::U64) => Opcode::CompLtU64,

            (BinOp::Leq, TCPrimType::I32) => Opcode::CompLeqI32,

            (BinOp::Geq, TCPrimType::I32) => {
                tagged.op = Opcode::Swap { top: 4, bottom: 4 };
                self.opcodes.push(tagged);
                Opcode::CompLeqI32
            }
            (BinOp::Geq, TCPrimType::U64) => {
                tagged.op = Opcode::Swap { top: 8, bottom: 8 };
                self.opcodes.push(tagged);
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

        self.opcodes.push(tagged);
    }

    pub fn assemble(mut self, env: &FileDb) -> Result<Program, Error> {
        let no_main = || error!("missing main function definition");
        let main_sym = if let Some(sym) = env.translate.get("main") {
            *sym
        } else {
            return Err(no_main());
        };

        let main_link_name = LinkName {
            name: main_sym,
            file: n32::NULL,
        };

        let main_func_idx = self.func_linkage.get(&main_link_name).ok_or_else(no_main)?;

        let main_func = &self.functions[*main_func_idx as usize];
        let (main_idx, main_loc) = main_func.func_header.ok_or_else(no_main)?;
        let mut opcodes: Vec<TaggedOpcode> = init_main_no_args(*main_func_idx)
            .iter()
            .map(|&op| TaggedOpcode { op, loc: main_loc })
            .collect();
        let runtime_length = opcodes.len() as u32; // No overflow here because len is predefined
        opcodes.append(&mut self.opcodes);
        for (op_idx, op) in opcodes.iter_mut().enumerate() {
            let op_idx = op_idx as u32;
            match &mut op.op {
                Opcode::Call(addr) => {
                    let function = &self.functions[*addr as usize];
                    if let Some((fptr, _loc)) = function.func_header {
                        *addr = fptr + runtime_length;
                    } else {
                        let func_loc = function.decl_loc;
                        return Err(error!(
                            "couldn't find definition for function",
                            op.loc, "called here", func_loc, "declared here"
                        ));
                    }
                }
                _ => {}
            }
        }

        use core::mem::{align_of, replace, size_of};
        let file_size = env.size();

        let symbols_size = align_usize(
            self.symbols.len() * size_of::<RuntimeVar>(),
            align_of::<TaggedOpcode>(),
        );

        let opcodes_size = opcodes.len() * size_of::<TaggedOpcode>();

        let var_data_size = align_usize(self.data.data.len(), align_of::<Var>());
        let vars_size = self.data.vars.len() * size_of::<Var>();
        let data_size = var_data_size + vars_size;

        let total_size = file_size + symbols_size + opcodes_size + data_size;
        let buckets = BucketListFactory::with_capacity(total_size + 16);
        let mut frame = buckets.frame(total_size).unwrap();

        let files = FileDbRef::new_from_frame(&mut frame, env);
        let symbols = frame.add_array(replace(&mut self.symbols, Vec::new()));
        let ops = frame.add_array(opcodes);
        let data = self.data.write_to_ref(frame);

        let program = Program {
            buckets,
            files,
            symbols,
            data,
            ops,
        };

        return Ok(program);
    }
}

pub fn func_decl_mismatch(original: CodeLoc, new: CodeLoc) -> Error {
    return error!(
        "function declaration type doesn't match previous declaration",
        original, "original declaration here", new, "second declaration here"
    );
}

pub fn func_redef(original: CodeLoc, redef: CodeLoc) -> Error {
    return error!(
        "redefinition of function",
        original, "original definition here", redef, "second definition here"
    );
}
