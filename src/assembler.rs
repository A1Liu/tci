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
    pub function_temps: Vec<u32>,
    pub data: VarBuffer,
    pub symbols: Vec<RuntimeVar>,
    pub labels: Vec<u32>,
}

#[derive(Debug, Clone, Copy)]
pub struct ASMEnv<'a> {
    var_offsets: &'a [i16],
    binary_offsets: &'a [u32],
    link_names: &'a HashMap<u32, LinkName>,
}

pub fn asm_env<'a>(
    var_offsets: &'a [i16],
    binary_offsets: &'a [u32],
    link_names: &'a HashMap<u32, LinkName>,
) -> ASMEnv<'a> {
    ASMEnv {
        var_offsets,
        binary_offsets,
        link_names,
    }
}

impl Drop for Assembler {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LabelData {
    pub new_label: u32,
    pub scope_idx: u32,
}

lazy_static! {
    pub static ref INIT_MAIN :Vec<Opcode> = vec![
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
        Opcode::Ret, // Placeholder for main idx
        Opcode::CallDyn,
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

pub const INIT_MAIN_PLACEHOLDER_IDX: usize = 4;

impl Assembler {
    pub fn new() -> Self {
        Self {
            buckets: BucketListFactory::new(),
            opcodes: Vec::new(),
            data: VarBuffer::new(),
            func_linkage: HashMap::new(),
            functions: Vec::new(),
            function_temps: Vec::new(),
            symbols: Vec::new(),
            labels: Vec::new(),
        }
    }

    pub fn add_file(&mut self, file: u32, mut tu: TranslationUnit) -> Result<(), Error> {
        // Add function return sizes

        let mut link_names = HashMap::new();
        let mut defns = Vec::new();
        let mut binary_offsets = Vec::with_capacity(tu.variables.len());
        unsafe { binary_offsets.set_len(tu.variables.len()) };

        for (_, &global_var) in tu.variables.iter() {
            if global_var.ty.is_function() {
                binary_offsets[global_var.var_idx as usize] = !0; // TODO idk man wtf do i do about this?
                continue;
            }

            let size = global_var.ty.size().into();
            let var = self.data.add_var(size, META_NO_SYMBOL); // TODO add symbol
            binary_offsets[global_var.var_idx as usize] = var;
        }

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

            self.add_function(&binary_offsets, &link_names, &defn);
            let header = &mut self.functions[self.func_linkage[&link_name] as usize].func_header;
            *header = Some((opcode, defn.loc));
        }

        return Ok(());
    }

    pub fn add_function(
        &mut self,
        binary_offsets: &[u32],
        link_names: &HashMap<u32, LinkName>,
        defn: &TCFuncDefn,
    ) {
        if defn.ops.len() < 2 {
            unreachable!()
        }

        let jumps: Vec<u32> = Vec::new();
        // maps symbols to variable offsets
        let mut var_offsets: Vec<i16> = (0..defn.sym_count).map(|_| i16::MAX).collect();
        let mut next_offset = 0; // where should the next variable be allocated (relative to fp)?
        let mut labels: Vec<LabelData> = Vec::with_capacity(defn.label_count as usize);
        unsafe { labels.set_len(defn.label_count as usize) };

        for idx in 0..defn.param_count {
            var_offsets[idx as usize] = -(idx as i16) - 2;
        }

        for t_op in defn.ops {
            match t_op.kind {
                TCOpcodeKind::Label { label, scope_idx } => {
                    let new_label = self.labels.len() as u32;
                    self.labels.push(!0);
                    let label_data = LabelData {
                        new_label,
                        scope_idx,
                    };

                    labels[label as usize] = label_data;
                }
                _ => {}
            }
        }

        if let TCOpcodeKind::ScopeBegin(vars, _) = defn.ops[0].kind {
            let mut op = TaggedOpcode::new(Opcode::Ret, defn.ops[0].loc);

            for (&var, &ty) in vars {
                if var_offsets[var as usize] < 0 {
                    continue;
                }

                op.op = Opcode::StackAlloc {
                    bytes: ty.size().into(),
                    symbol: META_NO_SYMBOL,
                };

                self.opcodes.push(op);
                var_offsets[var as usize] = next_offset;
                next_offset += 1;
            }
        } else {
            panic!("idk what happened man");
        }

        for t_op in &defn.ops[1..(defn.ops.len() - 1)] {
            let mut op = TaggedOpcode::new(Opcode::Ret, t_op.loc);

            match t_op.kind {
                TCOpcodeKind::ScopeBegin(vars, _) => {
                    for (&var, &ty) in vars {
                        op.op = Opcode::StackAlloc {
                            bytes: ty.size().into(),
                            symbol: META_NO_SYMBOL,
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
                    // TODO make sure stack deallocs happen
                    op.op = Opcode::Ret;
                    self.opcodes.push(op);
                }
                TCOpcodeKind::RetVal(val) => {
                    let env = asm_env(&var_offsets, binary_offsets, link_names);
                    self.translate_expr(env, &val);

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
                    let env = asm_env(&var_offsets, binary_offsets, link_names);
                    self.translate_expr(env, &expr);

                    let bytes = expr.ty.repr_size();
                    op.op = Opcode::Pop { bytes };
                    self.opcodes.push(op);
                }

                TCOpcodeKind::Label { label, .. } => {
                    let new_label = labels[label as usize].new_label;
                    self.labels[new_label as usize] = self.opcodes.len() as u32;
                }
                TCOpcodeKind::Goto { goto, scope_idx } => {
                    let label = labels[goto as usize];
                    self.solve_scope_difference(defn.ops, scope_idx, label.scope_idx, t_op.loc);
                    op.op = Opcode::Jump(label.new_label);
                    self.opcodes.push(op);
                }
                TCOpcodeKind::GotoIfZero {
                    cond,
                    cond_ty,
                    goto,
                    scope_idx,
                } => {
                    let env = asm_env(&var_offsets, binary_offsets, link_names);
                    self.translate_expr(env, &cond);

                    let new_label = labels[goto as usize].new_label;
                    op.op = match cond_ty.size() {
                        1 => Opcode::JumpIfZero8(new_label),
                        2 => Opcode::JumpIfZero16(new_label),
                        4 => Opcode::JumpIfZero32(new_label),
                        8 => Opcode::JumpIfZero64(new_label),
                        _ => unreachable!(),
                    };
                    self.opcodes.push(op);
                }
                TCOpcodeKind::GotoIfNotZero {
                    cond,
                    cond_ty,
                    goto,
                    scope_idx,
                } => {
                    let env = asm_env(&var_offsets, binary_offsets, link_names);
                    self.translate_expr(env, &cond);

                    let new_label = labels[goto as usize].new_label;
                    op.op = match cond_ty.size() {
                        1 => Opcode::JumpIfNotZero8(new_label),
                        2 => Opcode::JumpIfNotZero16(new_label),
                        4 => Opcode::JumpIfNotZero32(new_label),
                        8 => Opcode::JumpIfNotZero64(new_label),
                        _ => unreachable!(),
                    };
                    self.opcodes.push(op);
                }
                x => unimplemented!("{:?}", x),
            }
        }

        let last = defn.ops[defn.ops.len() - 1];
        if let TCOpcodeKind::ScopeEnd { count, begin } = last.kind {
            let mut op = TaggedOpcode::new(Opcode::StackDealloc, last.loc);
            op.op = Opcode::StackDealloc;

            for _ in 0..(count - defn.param_count) {
                self.opcodes.push(op);
            }

            next_offset -= count as i16;
        } else {
        }

        self.opcodes.push(TaggedOpcode::new(Opcode::Ret, defn.loc));
    }

    /// inserts stack deallocs and stack allocs to solve scope problems
    pub fn solve_scope_difference(
        &mut self,
        ops: &[TCOpcode],
        mut goto_scope: u32,
        mut label_scope: u32,
        loc: CodeLoc,
    ) {
        let mut goto_scopes = Vec::new();
        let mut label_scopes = Vec::new();

        while goto_scope != !0 {
            if let TCOpcodeKind::ScopeBegin(vars, parent) = ops[goto_scope as usize].kind {
                goto_scopes.push((goto_scope, vars));
                goto_scope = parent;
            } else {
                panic!("scope_idx pointed to wrong opcode")
            }
        }

        while label_scope != !0 {
            if let TCOpcodeKind::ScopeBegin(vars, parent) = ops[label_scope as usize].kind {
                label_scopes.push((label_scope, vars));
                label_scope = parent;
            } else {
                panic!("scope_idx pointed to wrong opcode")
            }
        }

        while let Some((g, l)) = goto_scopes.last().zip(label_scopes.last()) {
            let (goto, _) = g;
            let (label, _) = l;
            if goto != label {
                break;
            }

            goto_scopes.pop();
            label_scopes.pop();
        }

        let mut tagged = TaggedOpcode::new(Opcode::StackDealloc, loc);
        for (_, scope) in goto_scopes {
            for _ in 0..scope.len() {
                self.opcodes.push(tagged);
            }
        }

        for (_, scope) in label_scopes.into_iter().rev() {
            for (&var, &ty) in scope {
                tagged.op = Opcode::StackAlloc {
                    bytes: ty.size().into(),
                    symbol: META_NO_SYMBOL,
                };

                self.opcodes.push(tagged);
            }
        }
    }

    pub fn translate_expr(&mut self, env: ASMEnv, expr: &TCExpr) {
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
                let var = env.var_offsets[*label as usize];
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
            TCExprKind::FunctionIdent { ident } => {
                let link_name = env.link_names[ident];
                let id = self.func_linkage[&link_name];
                self.function_temps.push(self.opcodes.len() as u32);
                tagged.op = Opcode::MakeTempU32(id);
                self.opcodes.push(tagged);
            }

            TCExprKind::ParenList(exprs) => {
                for (idx, expr) in exprs.iter().enumerate() {
                    self.translate_expr(env, expr);
                    let bytes = expr.ty.repr_size();

                    if idx + 1 < exprs.len() {
                        tagged.op = Opcode::Pop { bytes };
                        self.opcodes.push(tagged);
                    }
                }
            }

            TCExprKind::PostIncr { incr_ty, value } => {
                use TCPrimType::*;
                self.translate_assign(env, value);
                tagged.op = Opcode::PushDup { bytes: 8 };
                self.opcodes.push(tagged);
                let bytes: u32 = incr_ty.size() as u32;
                tagged.op = Opcode::Get { offset: 0, bytes };
                self.opcodes.push(tagged);
                tagged.op = Opcode::PushDup { bytes };
                self.opcodes.push(tagged);
                match incr_ty {
                    I32 | U32 => {
                        tagged.op = Opcode::MakeTempU32(1);
                        self.opcodes.push(tagged);
                        tagged.op = Opcode::AddU32;
                        self.opcodes.push(tagged);
                    }
                    I64 | U64 => {
                        tagged.op = Opcode::MakeTempU64(1);
                        self.opcodes.push(tagged);
                        tagged.op = Opcode::AddU64;
                        self.opcodes.push(tagged);
                    }
                    Pointer { stride } => {
                        let stride: u32 = stride.into();
                        tagged.op = Opcode::MakeTempU64(stride as u64);
                        self.opcodes.push(tagged);
                        tagged.op = Opcode::AddU64;
                        self.opcodes.push(tagged);
                    }
                    x => unimplemented!("post incr for {:?}", x),
                }

                let top = bytes * 2;
                tagged.op = Opcode::Swap { top, bottom: 8 };
                self.opcodes.push(tagged);
                tagged.op = Opcode::Set { offset: 0, bytes };
                self.opcodes.push(tagged);
            }

            TCExprKind::BinOp {
                op,
                op_type,
                left,
                right,
            } => {
                self.translate_expr(env, left);
                self.translate_expr(env, right);
                self.translate_bin_op(*op, *op_type, tagged.loc);
            }

            TCExprKind::UnaryOp {
                op,
                op_type,
                operand,
            } => {
                self.translate_expr(env, operand);
                self.translate_un_op(*op, *op_type, tagged.loc);
            }

            TCExprKind::Conv { from, to, expr } => {
                self.translate_expr(env, expr);
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

            // TODO fix this with array members
            TCExprKind::Member { base, offset } => {
                let base_bytes = base.ty.repr_size();
                self.translate_expr(env, base);
                let want_bytes = expr.ty.repr_size();
                let top_bytes = base_bytes - want_bytes - offset;
                tagged.op = Opcode::Pop { bytes: top_bytes };
                self.opcodes.push(tagged);
                tagged.op = Opcode::PopKeep {
                    drop: *offset,
                    keep: want_bytes,
                };
                self.opcodes.push(tagged);
            }
            &TCExprKind::PtrMember { base, offset } => {
                let bytes = expr.ty.repr_size();
                self.translate_expr(env, base);
                if expr.ty.is_array() {
                    tagged.op = Opcode::MakeTempU64(offset as u64);
                    self.opcodes.push(tagged);
                    tagged.op = Opcode::AddU64;
                    self.opcodes.push(tagged);
                } else {
                    tagged.op = Opcode::Get {
                        offset: offset,
                        bytes,
                    };
                    self.opcodes.push(tagged);
                }
            }

            TCExprKind::Assign { target, value } => {
                self.translate_expr(env, value);
                let bytes = value.ty.repr_size();
                tagged.op = Opcode::PushDup { bytes };
                self.opcodes.push(tagged);
                self.translate_assign(env, target);
                tagged.op = Opcode::Set { offset: 0, bytes };
                self.opcodes.push(tagged);
            }

            TCExprKind::Deref(ptr) => {
                self.translate_expr(env, ptr);
                tagged.op = Opcode::Get {
                    offset: 0,
                    bytes: expr.ty.size().into(),
                };
                self.opcodes.push(tagged);
            }
            TCExprKind::Ref(lvalue) => self.translate_assign(env, lvalue),

            TCExprKind::Call { func, params } => {
                self.translate_expr(env, func); // callee is sequenced before params

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
                    self.translate_expr(env, param);
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

            TCExprKind::Ternary {
                condition,
                cond_ty,
                if_true,
                if_false,
            } => {
                let new_label = self.labels.len() as u32;
                self.labels.push(!0);
                self.translate_expr(env, condition);

                tagged.op = match cond_ty.size() {
                    1 => Opcode::JumpIfZero8(new_label),
                    2 => Opcode::JumpIfZero16(new_label),
                    4 => Opcode::JumpIfZero32(new_label),
                    8 => Opcode::JumpIfZero64(new_label),
                    _ => unreachable!(),
                };
                self.opcodes.push(tagged);

                self.translate_expr(env, if_true);
                self.labels[new_label as usize] = self.opcodes.len() as u32;
                self.translate_expr(env, if_false);
            }

            TCExprKind::Builtin(TCBuiltin::Ecall(ecall)) => {
                self.translate_expr(env, ecall);
                tagged.op = Opcode::EcallDyn;
                self.opcodes.push(tagged);
            }
            TCExprKind::Builtin(TCBuiltin::PushTempStack { ptr, size }) => {
                self.translate_expr(env, size);
                self.translate_expr(env, ptr);

                tagged.op = Opcode::PushDyn;
                self.opcodes.push(tagged);
            }

            x => unimplemented!("{:#?}", x),
        }
    }

    pub fn translate_assign(&mut self, env: ASMEnv, assign: &TCAssignTarget) {
        let mut tagged = TaggedOpcode::new(Opcode::Ret, assign.loc);

        match assign.kind {
            TCAssignTargetKind::Ptr(expr) => {
                self.translate_expr(env, expr);
                if assign.offset != 0 {
                    tagged.op = Opcode::MakeTempU64(assign.offset as u64);
                    self.opcodes.push(tagged);
                    tagged.op = Opcode::AddU64;
                    self.opcodes.push(tagged);
                }
            }
            TCAssignTargetKind::LocalIdent { label } => {
                let var = env.var_offsets[label as usize];
                tagged.op = Opcode::MakeTempStackFpPtr {
                    var,
                    offset: assign.offset,
                };
                self.opcodes.push(tagged);
            }
            _ => unimplemented!(),
        }
    }

    pub fn translate_un_op(&mut self, op: TCUnaryOp, op_type: TCPrimType, loc: CodeLoc) {
        use TCPrimType::*;
        use TCUnaryOp::*;
        let mut tagged = TaggedOpcode::new(Opcode::Ret, loc);
        match (op, op_type) {
            (Neg, I32) => {
                tagged.op = Opcode::MakeTempI32(-1);
                self.opcodes.push(tagged);
                tagged.op = Opcode::MulI32;
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

            (op, ptype) => unimplemented!("op={:?} type={:?}", op, ptype),
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

        let runtime_length = INIT_MAIN.len() as u32; // No overflow here because len is predefined
        let main_idx = main_idx + runtime_length;

        let mut opcodes: Vec<TaggedOpcode> = INIT_MAIN
            .iter()
            .map(|&op| TaggedOpcode { op, loc: main_loc })
            .collect();
        opcodes[INIT_MAIN_PLACEHOLDER_IDX] = TaggedOpcode {
            op: Opcode::MakeTempU64(main_idx as u64),
            loc: main_loc,
        };
        opcodes.append(&mut self.opcodes);

        for op in opcodes.iter_mut() {
            match &mut op.op {
                Opcode::Jump(target) => {
                    *target = self.labels[*target as usize] + runtime_length;
                }
                Opcode::JumpIfZero8(target) => {
                    *target = self.labels[*target as usize] + runtime_length;
                }
                Opcode::JumpIfZero16(target) => {
                    *target = self.labels[*target as usize] + runtime_length;
                }
                Opcode::JumpIfZero32(target) => {
                    *target = self.labels[*target as usize] + runtime_length;
                }
                Opcode::JumpIfZero64(target) => {
                    *target = self.labels[*target as usize] + runtime_length;
                }
                Opcode::JumpIfNotZero8(target) => {
                    *target = self.labels[*target as usize] + runtime_length;
                }
                Opcode::JumpIfNotZero16(target) => {
                    *target = self.labels[*target as usize] + runtime_length;
                }
                Opcode::JumpIfNotZero32(target) => {
                    *target = self.labels[*target as usize] + runtime_length;
                }
                Opcode::JumpIfNotZero64(target) => {
                    *target = self.labels[*target as usize] + runtime_length;
                }
                _ => {}
            }
        }

        for &temp in &self.function_temps {
            let idx = temp + runtime_length;

            if let Opcode::MakeTempU32(func) = opcodes[idx as usize].op {
                let function = &self.functions[func as usize];
                if let Some((fptr, _loc)) = function.func_header {
                    let fptr = fptr + runtime_length;
                    opcodes[idx as usize].op = Opcode::MakeTempU64(fptr as u64);
                } else {
                    let func_loc = function.decl_loc;
                    return Err(error!(
                        "couldn't find definition for function",
                        opcodes[idx as usize].loc, "called here", func_loc, "declared here"
                    ));
                }
            } else {
                panic!("invariant ruined");
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
        let frame = buckets.frame(total_size).unwrap();

        let files = FileDbRef::new(&frame, env);
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
