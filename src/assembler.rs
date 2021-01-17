use crate::buckets::*;
use crate::filedb::*;
use crate::runtime::*;
use crate::tc_ast::*;
use crate::util::*;
use core::mem;

#[derive(Debug)]
pub struct ASMFunc {
    pub func_type: TCFuncType,
    pub decl_loc: CodeLoc,
    pub func_header: Option<(VarPointer, CodeLoc)>, // first u32 points into opcodes buffer
}

pub struct FuncEnv {
    pub link_names: HashMap<u32, LinkName>,
    pub opcodes: VecU8,
    pub labels: Vec<LabelData>,
    pub gotos: Vec<u32>,
}

impl FuncEnv {
    pub fn new() -> Self {
        Self {
            link_names: HashMap::new(),
            opcodes: VecU8::new(),
            labels: Vec::new(),
            gotos: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.link_names.clear();
        self.opcodes.data.clear();
        self.labels.clear();
        self.gotos.clear();
    }
}

pub struct BinaryInit {
    pub init: BinaryData,
    pub main_call: VarPointer,
}

lazy_static! {
    pub static ref BINARY_INIT: BinaryInit = {
        let mut data = VecU8::new();

        data.push(Opcode::StackAlloc);
        data.push(4u32);
        data.push(Opcode::StackAlloc);
        data.push(8u32);
        data.push(Opcode::StackAlloc);
        data.push(4u32);

        data.push(Opcode::MakeU64);
        let main_call = data.data.len() as u32;

        data.push(0u64);

        data.push(Opcode::Call);
        data.push(Opcode::MakeSp);
        data.push(0i16);
        data.push(Opcode::Get);
        data.push(4);
        data.push(Opcode::StackDealloc); // for the return value

        data.push(Opcode::StackDealloc);
        data.push(Opcode::StackDealloc);

        data.push(Opcode::MakeU32);
        data.push(ECALL_EXIT);
        data.push(Opcode::Ecall);

        let mut init = BinaryData::new();
        let main_call = init.add_data(&mut data.data).with_offset(main_call);

        BinaryInit { init, main_call }
    };
}

pub struct Assembler {
    pub buckets: BucketListFactory,
    pub func_linkage: HashMap<LinkName, u32>,
    pub functions: Vec<ASMFunc>,
    pub function_temps: Vec<(VarPointer, CodeLoc)>,
    pub func: FuncEnv,
    pub data: BinaryData,
}

#[derive(Debug, Clone, Copy)]
pub struct ASMEnv<'a> {
    var_offsets: &'a [i16],
    link_names: &'a HashMap<u32, LinkName>,
}

pub fn asm_env<'a>(var_offsets: &'a [i16], link_names: &'a HashMap<u32, LinkName>) -> ASMEnv<'a> {
    ASMEnv {
        var_offsets,
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
    pub offset: u32,
    pub scope_idx: u32,
}

impl LabelData {
    pub fn uninit() -> Self {
        Self {
            offset: !0,
            scope_idx: !0,
        }
    }
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            buckets: BucketListFactory::new(),
            data: BINARY_INIT.init.clone(),
            func_linkage: HashMap::new(),
            functions: Vec::new(),
            function_temps: Vec::new(),
            func: FuncEnv::new(),
        }
    }

    pub fn add_file(&mut self, mut tu: TranslationUnit) -> Result<(), Error> {
        // Add function return sizes

        let mut link_names = HashMap::new();
        let mut defns = Vec::new();

        for (ident, tc_func) in std::mem::replace(&mut tu.functions, HashMap::new()) {
            let link_name = if tc_func.is_static {
                LinkName::new_static(ident, tu.file)
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

            let label_count = defn.label_count as usize;
            self.func.labels.resize(label_count, LabelData::uninit());

            let temps_begin = self.function_temps.len();

            self.func.opcodes.push(Opcode::Func);
            self.func.opcodes.push(link_name);
            self.func.opcodes.push(defn.loc);

            self.add_function(&link_names, &defn);

            let fptr = self.data.add_data(&mut self.func.opcodes.data);

            for (ptr, _) in &mut self.function_temps[temps_begin..] {
                let offset = ptr.offset();
                *ptr = fptr.with_offset(offset);
            }

            for &goto in self.func.gotos.iter() {
                let ptr = fptr.with_offset(goto);
                let label_ptr: VarPointer = self.data.read(ptr).unwrap();
                let label_offset = self.func.labels[label_ptr.offset() as usize].offset;
                self.data.write(ptr, fptr.with_offset(label_offset));
            }

            let header = &mut self.functions[self.func_linkage[&link_name] as usize].func_header;
            *header = Some((fptr, defn.loc));
            self.func.clear();
        }

        return Ok(());
    }

    pub fn add_function(&mut self, link_names: &HashMap<u32, LinkName>, defn: &TCFuncDefn) {
        if defn.ops.len() < 2 {
            unreachable!()
        }

        let jumps: Vec<u32> = Vec::new();
        // maps symbols to variable offsets
        let mut var_offsets: Vec<i16> = (0..defn.sym_count).map(|_| i16::MAX).collect();
        let mut next_offset = 0; // where should the next variable be allocated (relative to fp)?

        for idx in 0..defn.param_count {
            var_offsets[idx as usize] = -(idx as i16) - 2;
        }

        for t_op in defn.ops {
            match t_op.kind {
                TCOpcodeKind::Label { label, scope_idx } => {
                    self.func.labels[label as usize].scope_idx = scope_idx;
                }
                _ => {}
            }
        }

        if let TCOpcodeKind::ScopeBegin(vars, _) = defn.ops[0].kind {
            self.func.opcodes.push(Opcode::Loc);
            self.func.opcodes.push(defn.ops[0].loc);

            for (&var, &ty) in vars {
                if var_offsets[var as usize] < 0 {
                    continue;
                }

                self.func.opcodes.push(Opcode::StackAlloc);
                self.func.opcodes.push(ty.size());
                var_offsets[var as usize] = next_offset;
                next_offset += 1;
            }
        } else {
            panic!("idk what happened man");
        }

        for t_op in &defn.ops[1..(defn.ops.len() - 1)] {
            self.func.opcodes.push(Opcode::Loc);
            self.func.opcodes.push(t_op.loc);

            match t_op.kind {
                TCOpcodeKind::ScopeBegin(vars, _) => {
                    for (&var, &ty) in vars {
                        self.func.opcodes.push(Opcode::StackAlloc);
                        self.func.opcodes.push(ty.size());

                        var_offsets[var as usize] = next_offset;
                        next_offset += 1;
                    }
                }
                TCOpcodeKind::ScopeEnd { count, begin } => {
                    for _ in 0..count {
                        self.func.opcodes.push(Opcode::StackDealloc);
                    }

                    next_offset -= count as i16;
                }
                TCOpcodeKind::Ret => {
                    for _ in 0..next_offset {
                        self.func.opcodes.push(Opcode::StackDealloc);
                    }

                    self.func.opcodes.push(Opcode::Ret);
                }
                TCOpcodeKind::RetVal(val) => {
                    let env = asm_env(&var_offsets, link_names);
                    self.translate_expr(env, &val);

                    self.func.opcodes.push(Opcode::MakeFp);
                    self.func.opcodes.push(-1i16);
                    self.func.opcodes.push(Opcode::Set);
                    self.func.opcodes.push(val.ty.repr_size());

                    for _ in 0..next_offset {
                        self.func.opcodes.push(Opcode::StackDealloc);
                    }

                    self.func.opcodes.push(Opcode::Ret);
                }
                TCOpcodeKind::Expr(expr) => {
                    let env = asm_env(&var_offsets, link_names);
                    self.translate_expr(env, &expr);

                    let bytes = expr.ty.repr_size();
                    self.func.opcodes.push(Opcode::Pop);
                    self.func.opcodes.push(bytes);
                }

                TCOpcodeKind::Label { label, .. } => {
                    self.func.labels[label as usize].offset = self.func.opcodes.data.len() as u32;
                }
                TCOpcodeKind::Goto { goto, scope_idx } => {
                    let label = self.func.labels[goto as usize];
                    self.solve_scope_difference(defn.ops, scope_idx, label.scope_idx, t_op.loc);
                    self.func.opcodes.push(Opcode::Jump);
                    self.func.gotos.push(self.func.opcodes.data.len() as u32);
                    self.func.opcodes.push(VarPointer::new_binary(0, goto));
                }
                TCOpcodeKind::GotoIfZero {
                    cond,
                    cond_ty,
                    goto,
                    scope_idx,
                } => {
                    let env = asm_env(&var_offsets, link_names);
                    self.translate_expr(env, &cond);

                    let op = match cond_ty.size() {
                        1 => Opcode::JumpIfZero8,
                        2 => Opcode::JumpIfZero16,
                        4 => Opcode::JumpIfZero32,
                        8 => Opcode::JumpIfZero64,
                        _ => unreachable!(),
                    };

                    self.func.opcodes.push(op);
                    self.func.gotos.push(self.func.opcodes.data.len() as u32);
                    self.func.opcodes.push(VarPointer::new_binary(0, goto));
                }
                TCOpcodeKind::GotoIfNotZero {
                    cond,
                    cond_ty,
                    goto,
                    scope_idx,
                } => {
                    let env = asm_env(&var_offsets, link_names);
                    self.translate_expr(env, &cond);

                    let op = match cond_ty.size() {
                        1 => Opcode::JumpIfNotZero8,
                        2 => Opcode::JumpIfNotZero16,
                        4 => Opcode::JumpIfNotZero32,
                        8 => Opcode::JumpIfNotZero64,
                        _ => unreachable!(),
                    };

                    self.func.opcodes.push(op);
                    self.func.gotos.push(self.func.opcodes.data.len() as u32);
                    self.func.opcodes.push(VarPointer::new_binary(0, goto));
                }
                x => unimplemented!("{:?}", x),
            }
        }

        let last = defn.ops[defn.ops.len() - 1];
        if let TCOpcodeKind::ScopeEnd { count, begin } = last.kind {
            self.func.opcodes.push(Opcode::Loc);
            self.func.opcodes.push(last.loc);

            for _ in 0..(count - defn.param_count) {
                self.func.opcodes.push(Opcode::StackDealloc);
            }
        } else {
            panic!("invariant broken");
        }

        self.func.opcodes.push(Opcode::Ret);
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

        for (_, scope) in goto_scopes {
            for _ in 0..scope.len() {
                self.func.opcodes.push(Opcode::StackDealloc);
            }
        }

        for (_, scope) in label_scopes.into_iter().rev() {
            for (&var, &ty) in scope {
                self.func.opcodes.push(Opcode::StackAlloc);
                self.func.opcodes.push(ty.size());
            }
        }
    }

    pub fn translate_expr(&mut self, env: ASMEnv, expr: &TCExpr) {
        match &expr.kind {
            TCExprKind::I8Lit(val) => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                self.func.opcodes.push(Opcode::MakeI8);
                self.func.opcodes.push(*val);
            }
            TCExprKind::I32Lit(val) => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                self.func.opcodes.push(Opcode::MakeI32);
                self.func.opcodes.push(*val);
            }
            TCExprKind::I64Lit(val) => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                self.func.opcodes.push(Opcode::MakeI64);
                self.func.opcodes.push(*val);
            }
            TCExprKind::U64Lit(val) => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                self.func.opcodes.push(Opcode::MakeU64);
                self.func.opcodes.push(*val);
            }
            TCExprKind::StringLit(val) => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                let ptr = self.data.add_slice(val.as_bytes());
                self.data.data.push(0u8);
                self.func.opcodes.push(Opcode::MakeU64);
                self.func.opcodes.push(ptr);
            }
            TCExprKind::LocalIdent { label } => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                let var = env.var_offsets[*label as usize];

                self.func.opcodes.push(Opcode::MakeFp);
                self.func.opcodes.push(var);

                if !expr.ty.is_array() {
                    self.func.opcodes.push(Opcode::Get);
                    self.func.opcodes.push(expr.ty.repr_size());
                }
            }
            TCExprKind::FunctionIdent { ident } => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                let link_name = env.link_names[ident];
                let id = self.func_linkage[&link_name];
                self.func.opcodes.push(Opcode::MakeU64);

                let ptr = VarPointer::new_binary(0, self.func.opcodes.data.len() as u32);
                self.function_temps.push((ptr, expr.loc));
                self.func.opcodes.push(VarPointer::new_binary(0, id));
            }

            TCExprKind::ArrayInit { elems, elem_ty } => {
                let mut elem = TCExpr {
                    kind: TCExprKind::Uninit,
                    ty: *elem_ty,
                    loc: expr.loc,
                };
                for &expr_kind in elems.iter() {
                    elem.kind = expr_kind;
                    self.translate_expr(env, &elem);
                }
            }
            TCExprKind::ParenList(exprs) => {
                for (idx, expr) in exprs.iter().enumerate() {
                    self.translate_expr(env, expr);
                    let bytes = expr.ty.repr_size();

                    if idx + 1 < exprs.len() {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(bytes);
                    }
                }
            }

            TCExprKind::PostIncr { incr_ty, value } => {
                use TCPrimType::*;
                self.translate_assign(env, value);
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                self.func.opcodes.push(Opcode::Dup);
                self.func.opcodes.push(8u32);

                let bytes: u32 = incr_ty.size() as u32;
                self.func.opcodes.push(Opcode::Get);
                self.func.opcodes.push(bytes);

                self.func.opcodes.push(Opcode::Dup);
                self.func.opcodes.push(bytes);

                match incr_ty {
                    I32 | U32 => {
                        self.func.opcodes.push(Opcode::MakeU32);
                        self.func.opcodes.push(1u32);
                        self.func.opcodes.push(Opcode::AddU32);
                    }
                    I64 | U64 => {
                        self.func.opcodes.push(Opcode::MakeU64);
                        self.func.opcodes.push(1u64);
                        self.func.opcodes.push(Opcode::AddU64);
                    }
                    Pointer { stride } => {
                        let stride: u32 = stride.into();
                        self.func.opcodes.push(Opcode::MakeU64);
                        self.func.opcodes.push(stride as u64);
                        self.func.opcodes.push(Opcode::AddU64);
                    }
                    x => unimplemented!("post incr for {:?}", x),
                }

                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(bytes * 2);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(Opcode::Set);
                self.func.opcodes.push(bytes);
            }

            TCExprKind::BinOp {
                op,
                op_type,
                left,
                right,
            } => {
                self.translate_expr(env, left);
                self.translate_expr(env, right);
                self.translate_bin_op(*op, *op_type, expr.loc);
            }

            TCExprKind::UnaryOp {
                op,
                op_type,
                operand,
            } => {
                self.translate_expr(env, operand);
                self.translate_un_op(*op, *op_type, expr.loc);
            }

            TCExprKind::Conv { from, to, expr } => {
                self.translate_expr(env, expr);
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                match (from, to.size()) {
                    (TCPrimType::U8, 1) => {}
                    (TCPrimType::U8, 4) => self.func.opcodes.push(Opcode::ZExtend8To32),
                    (TCPrimType::U8, 8) => self.func.opcodes.push(Opcode::ZExtend8To64),
                    (TCPrimType::I8, 1) => {}
                    (TCPrimType::I8, 4) => self.func.opcodes.push(Opcode::SExtend8To32),
                    (TCPrimType::I8, 8) => self.func.opcodes.push(Opcode::SExtend8To64),

                    (TCPrimType::U32, 1) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(3u32)
                    }
                    (TCPrimType::U32, 4) => {}
                    (TCPrimType::U32, 8) => self.func.opcodes.push(Opcode::ZExtend32To64),
                    (TCPrimType::I32, 1) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(3u32)
                    }
                    (TCPrimType::I32, 4) => {}
                    (TCPrimType::I32, 8) => self.func.opcodes.push(Opcode::SExtend32To64),

                    (TCPrimType::U64, 1) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(7u32)
                    }
                    (TCPrimType::U64, 4) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(4u32)
                    }
                    (TCPrimType::U64, 8) => {}
                    (TCPrimType::I64, 1) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(7u32)
                    }
                    (TCPrimType::I64, 4) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(4u32)
                    }
                    (TCPrimType::I64, 8) => {}

                    (TCPrimType::Pointer { .. }, 1) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(7u32)
                    }
                    (TCPrimType::Pointer { .. }, 4) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(4u32)
                    }
                    (TCPrimType::Pointer { .. }, 8) => {}
                    (_, _) => unreachable!(),
                }
            }

            // TODO fix this with array members
            &TCExprKind::Member { base, offset } => {
                self.translate_expr(env, base);

                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);

                let base_bytes = base.ty.repr_size();
                let want_bytes = expr.ty.repr_size();
                let top_bytes = base_bytes - want_bytes - offset;
                self.func.opcodes.push(Opcode::Pop);
                self.func.opcodes.push(top_bytes);
                if offset != 0 {
                    self.func.opcodes.push(Opcode::Swap);
                    self.func.opcodes.push(want_bytes);
                    self.func.opcodes.push(offset);
                    self.func.opcodes.push(Opcode::Pop);
                    self.func.opcodes.push(offset);
                }
            }
            &TCExprKind::PtrMember { base, offset } => {
                self.translate_expr(env, base);

                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);

                let bytes = expr.ty.repr_size();
                self.func.opcodes.push(Opcode::MakeU64);
                self.func.opcodes.push(offset as u64);
                self.func.opcodes.push(Opcode::AddU64);
                if !expr.ty.is_array() {
                    self.func.opcodes.push(Opcode::Get);
                    self.func.opcodes.push(bytes);
                }
            }

            TCExprKind::Assign { target, value } => {
                self.translate_expr(env, value);

                let bytes: u32 = target.ty.size().into();
                self.func.opcodes.push(Opcode::Dup);
                self.func.opcodes.push(bytes);
                self.translate_assign(env, target);

                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                self.func.opcodes.push(Opcode::Set);
                self.func.opcodes.push(bytes);
            }

            TCExprKind::MutAssign {
                target,
                value,
                op,
                op_type,
            } => {
                self.translate_assign(env, target);

                self.func.opcodes.push(Opcode::Dup);
                self.func.opcodes.push(8u32);

                let bytes = target.ty.repr_size();
                self.func.opcodes.push(Opcode::Get);
                self.func.opcodes.push(bytes);

                self.translate_expr(env, value);

                self.translate_bin_op(*op, *op_type, expr.loc);

                self.func.opcodes.push(Opcode::Dup);
                self.func.opcodes.push(bytes);

                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(bytes * 2);
                self.func.opcodes.push(8 as u32);

                self.func.opcodes.push(Opcode::Set);
                self.func.opcodes.push(bytes);
            }

            TCExprKind::Deref(ptr) => {
                self.translate_expr(env, ptr);
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);

                self.func.opcodes.push(Opcode::Get);
                self.func.opcodes.push(expr.ty.size());
            }
            TCExprKind::Ref(lvalue) => self.translate_assign(env, lvalue),

            TCExprKind::Call { func, params } => {
                self.translate_expr(env, func); // callee is sequenced before params

                // Safety allocation to make sure varargs don't run off the stack
                self.func.opcodes.push(Opcode::StackAlloc);
                self.func.opcodes.push(0u32);

                for (idx, param) in params.iter().rev().enumerate() {
                    let bytes = param.ty.repr_size();

                    self.translate_expr(env, param);
                    self.func.opcodes.push(Opcode::StackAlloc);
                    self.func.opcodes.push(bytes);
                    self.func.opcodes.push(Opcode::MakeSp);
                    self.func.opcodes.push(0i16);
                    self.func.opcodes.push(Opcode::Set);
                    self.func.opcodes.push(bytes);
                }

                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);

                // Allocation for return value
                let rtype_size = expr.ty.repr_size();
                self.func.opcodes.push(Opcode::StackAlloc);
                self.func.opcodes.push(rtype_size);

                self.func.opcodes.push(Opcode::Call);

                self.func.opcodes.push(Opcode::MakeSp);
                self.func.opcodes.push(0i16);
                self.func.opcodes.push(Opcode::Get);
                self.func.opcodes.push(rtype_size);
                self.func.opcodes.push(Opcode::StackDealloc); // for the return value

                for _ in 0..params.len() {
                    self.func.opcodes.push(Opcode::StackDealloc);
                }
                self.func.opcodes.push(Opcode::StackDealloc); // for the safety allocation
            }

            TCExprKind::Ternary {
                condition,
                cond_ty,
                if_true,
                if_false,
            } => {
                let else_label = self.func.labels.len() as u32;
                self.func.labels.push(LabelData::uninit());
                let end_label = self.func.labels.len() as u32;
                self.func.labels.push(LabelData::uninit());
                self.translate_expr(env, condition);

                let op = match cond_ty.size() {
                    1 => Opcode::JumpIfZero8,
                    2 => Opcode::JumpIfZero16,
                    4 => Opcode::JumpIfZero32,
                    8 => Opcode::JumpIfZero64,
                    _ => unreachable!(),
                };

                self.func.opcodes.push(op);
                self.func.gotos.push(self.func.opcodes.data.len() as u32);
                let ptr = VarPointer::new_binary(0, else_label);
                self.func.opcodes.push(ptr);

                self.translate_expr(env, if_true);
                self.func.opcodes.push(Opcode::Jump);
                self.func.gotos.push(self.func.opcodes.data.len() as u32);
                self.func.opcodes.push(VarPointer::new_binary(0, end_label));
                self.func.labels[else_label as usize].offset = self.func.opcodes.data.len() as u32;
                self.translate_expr(env, if_false);
                self.func.labels[end_label as usize].offset = self.func.opcodes.data.len() as u32;
            }

            TCExprKind::Builtin(TCBuiltin::Ecall(ecall)) => {
                self.translate_expr(env, ecall);
                self.func.opcodes.push(Opcode::Ecall);
            }
            TCExprKind::Builtin(TCBuiltin::PushTempStack { ptr, size }) => {
                self.translate_expr(env, size);
                self.translate_expr(env, ptr);

                self.func.opcodes.push(Opcode::PushDyn);
            }

            x => unimplemented!("{:#?}", x),
        }
    }

    pub fn translate_assign(&mut self, env: ASMEnv, assign: &TCAssignTarget) {
        match assign.kind {
            TCAssignTargetKind::Ptr(expr) => {
                self.translate_expr(env, expr);
                if assign.offset != 0 {
                    self.func.opcodes.push(Opcode::Loc);
                    self.func.opcodes.push(assign.loc);
                    self.func.opcodes.push(Opcode::MakeU64);
                    self.func.opcodes.push(assign.offset as u64);
                    self.func.opcodes.push(Opcode::AddU64);
                }
            }
            TCAssignTargetKind::LocalIdent { label } => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(assign.loc);

                let var = env.var_offsets[label as usize];
                self.func.opcodes.push(Opcode::MakeFp);
                self.func.opcodes.push(var);
                self.func.opcodes.push(Opcode::MakeU64);
                self.func.opcodes.push(assign.offset as u64);
                self.func.opcodes.push(Opcode::AddU64);
            }
            _ => unimplemented!(),
        }
    }

    pub fn translate_un_op(&mut self, op: TCUnaryOp, op_type: TCPrimType, loc: CodeLoc) {
        use TCPrimType::*;
        use TCUnaryOp::*;

        self.func.opcodes.push(Opcode::Loc);
        self.func.opcodes.push(loc);

        match (op, op_type) {
            (Neg, I32) => {
                self.func.opcodes.push(Opcode::MakeI32);
                self.func.opcodes.push(-1i32);
                self.func.opcodes.push(Opcode::MulI32);
            }
            _ => unimplemented!(),
        }
    }

    pub fn translate_bin_op(&mut self, op: BinOp, op_type: TCPrimType, loc: CodeLoc) {
        self.func.opcodes.push(Opcode::Loc);
        self.func.opcodes.push(loc);

        let op = match (op, op_type) {
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
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(4u32);
                self.func.opcodes.push(4u32);
                Opcode::CompLtI32
            }
            (BinOp::Gt, TCPrimType::U64) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(8u32);
                Opcode::CompLtU64
            }

            (BinOp::Lt, TCPrimType::I32) => Opcode::CompLtI32,
            (BinOp::Lt, TCPrimType::U64) => Opcode::CompLtU64,

            (BinOp::Leq, TCPrimType::I32) => Opcode::CompLeqI32,

            (BinOp::Geq, TCPrimType::I32) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(4u32);
                self.func.opcodes.push(4u32);
                Opcode::CompLeqI32
            }
            (BinOp::Geq, TCPrimType::U64) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(8u32);
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

        self.func.opcodes.push(op);
    }

    pub fn assemble(mut self, env: &FileDb) -> Result<BinaryData, Error> {
        let no_main = || error!("missing main function definition");

        let main_link_name = LinkName {
            name: BuiltinSymbol::Main as u32,
            file: n32::NULL,
        };

        let main_func_idx = self.func_linkage.get(&main_link_name).ok_or_else(no_main)?;

        let main_func = &self.functions[*main_func_idx as usize];
        let (main_ptr, main_loc) = main_func.func_header.ok_or_else(no_main)?;

        self.data.write(BINARY_INIT.main_call, main_ptr);

        for (temp, loc) in &self.function_temps {
            let ptr: VarPointer = self.data.read(*temp).unwrap();
            let function = &self.functions[ptr.offset() as usize];
            if let Some((fptr, _loc)) = function.func_header {
                self.data.write(*temp, fptr);
            } else {
                let func_loc = function.decl_loc;
                return Err(error!(
                    "couldn't find definition for function",
                    *loc, "called here", func_loc, "declared here"
                ));
            }
        }

        return Ok(mem::replace(&mut self.data, BinaryData::new()));
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
