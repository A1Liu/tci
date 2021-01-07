use crate::buckets::*;
use crate::interpreter::*;
use crate::new_tc_ast::*;
use crate::runtime::*;
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

impl Drop for Assembler {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
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
        for idx in 0..defn.param_count {
            var_offsets[idx as usize] = -(idx as i16) - 2;
        }

        for t_op in defn.ops {
            let mut op = TaggedOpcode::new(Opcode::Ret, t_op.loc);

            match t_op.kind {
                TCOpcodeKind::ScopeBegin(vars, scope_end) => {
                    for (var, ty) in vars.iter() {
                        op.op = Opcode::StackAlloc {
                            bytes: ty.size().into(),
                            symbol: self.symbols.len() as u32,
                        };

                        self.opcodes.push(op);

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
                        bytes: val.ty.size().into(),
                    };
                    self.opcodes.push(op);
                    op.op = Opcode::Ret;
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
                let var = self.data.add_var(val.len() as u32 + 1, META_NO_SYMBOL); // TODO overflow here
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
                        bytes: expr.ty.size().into(),
                    }
                };

                self.opcodes.push(tagged);
            }

            TCExprKind::ParenList(exprs) => {
                for (idx, expr) in exprs.iter().enumerate() {
                    self.translate_expr(var_offsets, expr);
                    let bytes = expr.ty.size().into();

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
                let bytes = value.ty.size().into();
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
                let rtype_size = expr.ty.size().into();

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
                    let bytes = param.ty.size().into();

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

                // TODO calling functions
                // let link_name = self.link_names[func];

                // let link_pos = if let Some(&link_pos) = self.func_linkage.get(&link_public) {
                //     link_pos
                // } else {
                //     self.func_linkage[&link_static]
                // };

                // tagged.op = Opcode::Call(link_pos);
                // self.opcodes.push(tagged);

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
            _ => unimplemented!(),
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

    pub fn translate_bin_op(&mut self, op: BinOp, op_type: TCPrimType, loc: CodeLoc) {}
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
