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
    pub func_header: Option<(VarPointer, CodeLoc)>,
}

#[derive(Debug)]
pub struct ASMVar {
    pub ty: TCType,
    pub decl_loc: CodeLoc,
    pub header: Option<(VarPointer, CodeLoc)>,
}

pub struct FuncEnv {
    pub opcodes: VecU8,
    pub labels: Vec<LabelData>,
    pub gotos: Vec<u32>,
    pub var_offsets: Vec<i16>,
}

impl FuncEnv {
    pub fn new() -> Self {
        Self {
            opcodes: VecU8::new(),
            labels: Vec::new(),
            gotos: Vec::new(),
            var_offsets: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.opcodes.data.clear();
        self.labels.clear();
        self.gotos.clear();
        self.var_offsets.clear();
    }
}

pub struct FileEnv {
    pub link_names: HashMap<u32, LinkName>,
    pub binary_offsets: Vec<u32>,
}

impl FileEnv {
    pub fn new() -> Self {
        Self {
            link_names: HashMap::new(),
            binary_offsets: Vec::new(),
        }
    }

    pub fn clear(&mut self) {
        self.link_names.clear();
        self.binary_offsets.clear();
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
        data.push(4u32);
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

    pub var_linkage: HashMap<LinkName, u32>,
    pub vars: Vec<ASMVar>,
    pub var_temps: Vec<(VarPointer, CodeLoc)>,

    pub func: FuncEnv,
    pub file: FileEnv,
    pub data: BinaryData,
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

            var_linkage: HashMap::new(),
            vars: Vec::new(),
            var_temps: Vec::new(),

            func: FuncEnv::new(),
            file: FileEnv::new(),
        }
    }

    pub fn add_file(&mut self, mut tu: TranslationUnit) -> Result<(), Error> {
        self.file.binary_offsets.resize(tu.var_count as usize, !0);

        for (loc, static_internal) in &tu.static_internal_vars {
            self.file.binary_offsets[static_internal.var_idx as usize] = self.vars.len() as u32;
            let (mut init, _) = {
                let (kind, ty, loc) = (static_internal.init, static_internal.ty, *loc);
                let expr = TCExpr { kind, ty, loc };
                self.make_var(expr)?
            };
            let vptr = self.data.add_data(&mut init);

            let var = ASMVar {
                ty: static_internal.ty,
                decl_loc: *loc,
                header: Some((vptr, *loc)),
            };

            self.vars.push(var);
        }

        for (&ident, global) in &tu.vars {
            if global.ty.is_function() {
                continue;
            }

            let link_name = if global.init.is_static() {
                LinkName::new_static(ident, tu.file)
            } else {
                LinkName::new(ident)
            };

            self.file.link_names.insert(ident, link_name);

            let prev = match self.var_linkage.entry(link_name) {
                Entry::Occupied(o) => *o.get(),
                Entry::Vacant(v) => {
                    let id = self.vars.len() as u32;
                    v.insert(id);

                    self.vars.push(ASMVar {
                        ty: global.ty,
                        decl_loc: global.loc,
                        header: None,
                    });

                    id
                }
            };

            self.file.binary_offsets[global.var_idx as usize] = prev;

            let init = match global.init {
                TCDeclInit::Extern => continue,
                TCDeclInit::DefaultUninit => TCExprKind::Uninit,
                TCDeclInit::Default(i) | TCDeclInit::Static(i) | TCDeclInit::ExternInit(i) => i,
            };

            if let Some((_, prev_loc)) = self.vars[prev as usize].header {
                return Err(error!(
                    "defined global variable twice",
                    prev_loc, "first definition here", global.loc, "second definition here"
                ));
            }

            let (mut init, _) = {
                let (kind, ty, loc) = (init, global.ty, global.loc);
                let expr = TCExpr { kind, ty, loc };
                self.make_var(expr)?
            };
            let vptr = self.data.add_data(&mut init);
            self.vars[prev as usize].header = Some((vptr, global.loc));
        }

        let mut defns = Vec::new();

        for (ident, tc_func) in std::mem::replace(&mut tu.functions, HashMap::new()) {
            let link_name = if tc_func.is_static {
                LinkName::new_static(ident, tu.file)
            } else {
                LinkName::new(ident)
            };

            self.file.link_names.insert(ident, link_name);

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

            let func_temps_begin = self.function_temps.len();
            let var_temps_begin = self.var_temps.len();

            self.func.opcodes.push(Opcode::Func);
            self.func.opcodes.push(link_name);
            self.func.opcodes.push(defn.loc);

            self.add_function(&defn);

            let fptr = self.data.add_data(&mut self.func.opcodes.data);

            for (ptr, _) in &mut self.function_temps[func_temps_begin..] {
                let offset = ptr.offset();
                *ptr = fptr.with_offset(offset);
            }
            for (ptr, _) in &mut self.var_temps[var_temps_begin..] {
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

        self.file.clear();
        return Ok(());
    }

    pub fn make_var(&self, expr: TCExpr) -> Result<(Vec<u8>, u32), Error> {
        let mut bytes = VecU8::new();

        match expr.kind {
            TCExprKind::Uninit => {
                debug!(expr.ty);
                for _ in 0u32..expr.ty.size().into() {
                    bytes.push(0u8);
                }
            }

            TCExprKind::I8Lit(i) => bytes.push(i),
            // TCExprKind::U8Lit(i) => bytes.push(i),
            // TCExprKind::I16Lit(i) => bytes.push(i),
            // TCExprKind::U16Lit(i) => bytes.push(i),
            TCExprKind::I32Lit(i) => bytes.push(i),
            TCExprKind::U32Lit(i) => bytes.push(i),
            TCExprKind::I64Lit(i) => bytes.push(i),
            TCExprKind::U64Lit(i) => bytes.push(i),
            TCExprKind::F32Lit(i) => bytes.push(i),
            TCExprKind::F64Lit(i) => bytes.push(i),

            TCExprKind::ArrayInit { elems, elem_ty: ty } => {
                for &(kind, loc) in elems {
                    let expr = TCExpr { kind, ty, loc };
                    let (mut data, _align) = self.make_var(expr)?;
                    bytes.append(&mut data);
                }
            }
            TCExprKind::StructLit { fields, size } => {
                for &field in fields {
                    let (mut data, align) = self.make_var(field)?;
                    bytes.align(align as usize);
                    bytes.append(&mut data);
                }

                bytes.data.resize(size as usize, 0);
            }

            TCExprKind::Conv { from, to, expr } => {
                use TCPrimType::*;

                let (data, align) = self.make_var(*expr)?;
                match (from, to) {
                    (I8, I8) => bytes.push(*u8_slice_as_any::<i8>(&data) as i8),
                    (I8, U8) => bytes.push(*u8_slice_as_any::<i8>(&data) as u8),
                    (I8, I16) => bytes.push(*u8_slice_as_any::<i8>(&data) as i16),
                    (I8, U16) => bytes.push(*u8_slice_as_any::<i8>(&data) as u16),
                    (I8, I32) => bytes.push(*u8_slice_as_any::<i8>(&data) as i32),
                    (I8, U32) => bytes.push(*u8_slice_as_any::<i8>(&data) as u32),
                    (I8, I64) => bytes.push(*u8_slice_as_any::<i8>(&data) as i64),
                    (I8, U64) => bytes.push(*u8_slice_as_any::<i8>(&data) as u64),
                    (I8, F32) => bytes.push(*u8_slice_as_any::<i8>(&data) as f32),
                    (I8, F64) => bytes.push(*u8_slice_as_any::<i8>(&data) as f64),

                    (U8, I8) => bytes.push(*u8_slice_as_any::<u8>(&data) as i8),
                    (U8, U8) => bytes.push(*u8_slice_as_any::<u8>(&data) as u8),
                    (U8, I16) => bytes.push(*u8_slice_as_any::<u8>(&data) as i16),
                    (U8, U16) => bytes.push(*u8_slice_as_any::<u8>(&data) as u16),
                    (U8, I32) => bytes.push(*u8_slice_as_any::<u8>(&data) as i32),
                    (U8, U32) => bytes.push(*u8_slice_as_any::<u8>(&data) as u32),
                    (U8, I64) => bytes.push(*u8_slice_as_any::<u8>(&data) as i64),
                    (U8, U64) => bytes.push(*u8_slice_as_any::<u8>(&data) as u64),
                    (U8, F32) => bytes.push(*u8_slice_as_any::<u8>(&data) as f32),
                    (U8, F64) => bytes.push(*u8_slice_as_any::<u8>(&data) as f64),

                    (I16, I8) => bytes.push(*u8_slice_as_any::<i16>(&data) as i8),
                    (I16, U8) => bytes.push(*u8_slice_as_any::<i16>(&data) as u8),
                    (I16, I16) => bytes.push(*u8_slice_as_any::<i16>(&data) as i16),
                    (I16, U16) => bytes.push(*u8_slice_as_any::<i16>(&data) as u16),
                    (I16, I32) => bytes.push(*u8_slice_as_any::<i16>(&data) as i32),
                    (I16, U32) => bytes.push(*u8_slice_as_any::<i16>(&data) as u32),
                    (I16, I64) => bytes.push(*u8_slice_as_any::<i16>(&data) as i64),
                    (I16, U64) => bytes.push(*u8_slice_as_any::<i16>(&data) as u64),
                    (I16, F32) => bytes.push(*u8_slice_as_any::<i16>(&data) as f32),
                    (I16, F64) => bytes.push(*u8_slice_as_any::<i16>(&data) as f64),

                    (U16, I8) => bytes.push(*u8_slice_as_any::<u16>(&data) as i8),
                    (U16, U8) => bytes.push(*u8_slice_as_any::<u16>(&data) as u8),
                    (U16, I16) => bytes.push(*u8_slice_as_any::<u16>(&data) as i16),
                    (U16, U16) => bytes.push(*u8_slice_as_any::<u16>(&data) as u16),
                    (U16, I32) => bytes.push(*u8_slice_as_any::<u16>(&data) as i32),
                    (U16, U32) => bytes.push(*u8_slice_as_any::<u16>(&data) as u32),
                    (U16, I64) => bytes.push(*u8_slice_as_any::<u16>(&data) as i64),
                    (U16, U64) => bytes.push(*u8_slice_as_any::<u16>(&data) as u64),
                    (U16, F32) => bytes.push(*u8_slice_as_any::<u16>(&data) as f32),
                    (U16, F64) => bytes.push(*u8_slice_as_any::<u16>(&data) as f64),

                    (I32, I8) => bytes.push(*u8_slice_as_any::<i32>(&data) as i8),
                    (I32, U8) => bytes.push(*u8_slice_as_any::<i32>(&data) as u8),
                    (I32, I16) => bytes.push(*u8_slice_as_any::<i32>(&data) as i16),
                    (I32, U16) => bytes.push(*u8_slice_as_any::<i32>(&data) as u16),
                    (I32, I32) => bytes.push(*u8_slice_as_any::<i32>(&data) as i32),
                    (I32, U32) => bytes.push(*u8_slice_as_any::<i32>(&data) as u32),
                    (I32, I64) => bytes.push(*u8_slice_as_any::<i32>(&data) as i64),
                    (I32, U64) => bytes.push(*u8_slice_as_any::<i32>(&data) as u64),
                    (I32, F32) => bytes.push(*u8_slice_as_any::<i32>(&data) as f32),
                    (I32, F64) => bytes.push(*u8_slice_as_any::<i32>(&data) as f64),

                    (U32, I8) => bytes.push(*u8_slice_as_any::<u32>(&data) as i8),
                    (U32, U8) => bytes.push(*u8_slice_as_any::<u32>(&data) as u8),
                    (U32, I16) => bytes.push(*u8_slice_as_any::<u32>(&data) as i16),
                    (U32, U16) => bytes.push(*u8_slice_as_any::<u32>(&data) as u16),
                    (U32, I32) => bytes.push(*u8_slice_as_any::<u32>(&data) as i32),
                    (U32, U32) => bytes.push(*u8_slice_as_any::<u32>(&data) as u32),
                    (U32, I64) => bytes.push(*u8_slice_as_any::<u32>(&data) as i64),
                    (U32, U64) => bytes.push(*u8_slice_as_any::<u32>(&data) as u64),
                    (U32, F32) => bytes.push(*u8_slice_as_any::<u32>(&data) as f32),
                    (U32, F64) => bytes.push(*u8_slice_as_any::<u32>(&data) as f64),

                    (I64, I8) => bytes.push(*u8_slice_as_any::<i64>(&data) as i8),
                    (I64, U8) => bytes.push(*u8_slice_as_any::<i64>(&data) as u8),
                    (I64, I16) => bytes.push(*u8_slice_as_any::<i64>(&data) as i16),
                    (I64, U16) => bytes.push(*u8_slice_as_any::<i64>(&data) as u16),
                    (I64, I32) => bytes.push(*u8_slice_as_any::<i64>(&data) as i32),
                    (I64, U32) => bytes.push(*u8_slice_as_any::<i64>(&data) as u32),
                    (I64, I64) => bytes.push(*u8_slice_as_any::<i64>(&data) as i64),
                    (I64, U64) => bytes.push(*u8_slice_as_any::<i64>(&data) as u64),
                    (I64, F32) => bytes.push(*u8_slice_as_any::<i64>(&data) as f32),
                    (I64, F64) => bytes.push(*u8_slice_as_any::<i64>(&data) as f64),

                    (U64, I8) => bytes.push(*u8_slice_as_any::<u64>(&data) as i8),
                    (U64, U8) => bytes.push(*u8_slice_as_any::<u64>(&data) as u8),
                    (U64, I16) => bytes.push(*u8_slice_as_any::<u64>(&data) as i16),
                    (U64, U16) => bytes.push(*u8_slice_as_any::<u64>(&data) as u16),
                    (U64, I32) => bytes.push(*u8_slice_as_any::<u64>(&data) as i32),
                    (U64, U32) => bytes.push(*u8_slice_as_any::<u64>(&data) as u32),
                    (U64, I64) => bytes.push(*u8_slice_as_any::<u64>(&data) as i64),
                    (U64, U64) => bytes.push(*u8_slice_as_any::<u64>(&data) as u64),
                    (U64, F32) => bytes.push(*u8_slice_as_any::<u64>(&data) as f32),
                    (U64, F64) => bytes.push(*u8_slice_as_any::<u64>(&data) as f64),

                    (Pointer { .. }, I8) => bytes.push(*u8_slice_as_any::<u64>(&data) as i8),
                    (Pointer { .. }, U8) => bytes.push(*u8_slice_as_any::<u64>(&data) as u8),
                    (Pointer { .. }, I16) => bytes.push(*u8_slice_as_any::<u64>(&data) as i16),
                    (Pointer { .. }, U16) => bytes.push(*u8_slice_as_any::<u64>(&data) as u16),
                    (Pointer { .. }, I32) => bytes.push(*u8_slice_as_any::<u64>(&data) as i32),
                    (Pointer { .. }, U32) => bytes.push(*u8_slice_as_any::<u64>(&data) as u32),
                    (Pointer { .. }, I64) => bytes.push(*u8_slice_as_any::<u64>(&data) as i64),
                    (Pointer { .. }, U64) => bytes.push(*u8_slice_as_any::<u64>(&data) as u64),

                    x => unimplemented!("static conversion {:?}", x),
                }
            }

            x => {
                return Err(error!(
                    "TCI only supports simple constant expressions right now",
                    expr.loc,
                    format!("expression here ({:?}) was too complicated", x)
                ))
            }
        }

        return Ok((bytes.data, expr.ty.align().into()));
    }

    pub fn add_function(&mut self, defn: &TCFuncDefn) {
        if defn.ops.len() < 2 {
            unreachable!()
        }

        assert_eq!(self.func.var_offsets.len(), 0);

        self.func
            .var_offsets
            .resize(defn.sym_count as usize, i16::MAX);
        let mut next_offset = 0; // where should the next variable be allocated (relative to fp)?

        for idx in 0..defn.param_count {
            self.func.var_offsets[idx as usize] = -(idx as i16) - 2;
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
                if self.func.var_offsets[var as usize] < 0 {
                    continue;
                }

                self.func.opcodes.push(Opcode::StackAlloc);
                self.func.opcodes.push(ty.size());
                self.func.var_offsets[var as usize] = next_offset;
                next_offset += 1;
            }
        } else {
            panic!("idk what happened man");
        }

        for t_op in &defn.ops[1..(defn.ops.len() - 1)] {
            match t_op.kind {
                TCOpcodeKind::ScopeBegin(vars, _) => {
                    for (&var, &ty) in vars {
                        self.func.opcodes.push(Opcode::StackAlloc);
                        self.func.opcodes.push(ty.size());

                        self.func.var_offsets[var as usize] = next_offset;
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
                    self.func.opcodes.push(Opcode::Loc);
                    self.func.opcodes.push(t_op.loc);

                    for _ in 0..next_offset {
                        self.func.opcodes.push(Opcode::StackDealloc);
                    }

                    self.func.opcodes.push(Opcode::Ret);
                }
                TCOpcodeKind::RetVal(val) => {
                    self.translate_expr(&val);

                    self.func.opcodes.push(Opcode::Loc);
                    self.func.opcodes.push(t_op.loc);

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
                    self.translate_expr(&expr);

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
                    self.translate_expr(&cond);

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
                    self.translate_expr(&cond);

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

                TCOpcodeKind::Switch {
                    expr,
                    cases,
                    default,
                } => {
                    let bytes = expr.ty.repr_size();
                    self.translate_expr(&expr);

                    for (case, take_case) in cases {
                        let skip_case = self.func.labels.len() as u32;
                        self.func.labels.push(LabelData::uninit());

                        self.func.opcodes.push(Opcode::Dup);
                        self.func.opcodes.push(bytes);
                        self.translate_expr(case);

                        let op = match bytes {
                            1 => Opcode::CompEq8,
                            2 => Opcode::CompEq16,
                            4 => Opcode::CompEq32,
                            8 => Opcode::CompEq64,
                            _ => unreachable!(),
                        };
                        self.func.opcodes.push(op);
                        self.func.opcodes.push(Opcode::JumpIfZero8);
                        self.func.gotos.push(self.func.opcodes.data.len() as u32);
                        let ptr = VarPointer::new_binary(0, skip_case);
                        self.func.opcodes.push(ptr);

                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(bytes);

                        self.func.opcodes.push(Opcode::Jump);
                        self.func.gotos.push(self.func.opcodes.data.len() as u32);
                        let ptr = VarPointer::new_binary(0, *take_case);
                        self.func.opcodes.push(ptr);

                        self.func.labels[skip_case as usize].offset =
                            self.func.opcodes.data.len() as u32;
                    }

                    self.func.opcodes.push(Opcode::Pop);
                    self.func.opcodes.push(bytes);

                    self.func.opcodes.push(Opcode::Jump);
                    self.func.gotos.push(self.func.opcodes.data.len() as u32);
                    let ptr = VarPointer::new_binary(0, default);
                    self.func.opcodes.push(ptr);
                }
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

    pub fn translate_expr(&mut self, expr: &TCExpr) {
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
            TCExprKind::U32Lit(val) => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);

                self.func.opcodes.push(Opcode::MakeU32);
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
            TCExprKind::F32Lit(val) => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);

                self.func.opcodes.push(Opcode::MakeF32);
                self.func.opcodes.push(*val);
            }
            TCExprKind::F64Lit(val) => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);

                self.func.opcodes.push(Opcode::MakeF64);
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

                let var = self.func.var_offsets[*label as usize];
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

                let link_name = self.file.link_names[ident];
                let id = self.func_linkage[&link_name];
                self.func.opcodes.push(Opcode::MakeU64);

                let ptr = VarPointer::new_binary(0, self.func.opcodes.data.len() as u32);
                self.function_temps.push((ptr, expr.loc));
                self.func.opcodes.push(VarPointer::new_binary(0, id));
            }
            TCExprKind::GlobalIdent { binary_offset } => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);

                let id = self.file.binary_offsets[*binary_offset as usize];
                self.func.opcodes.push(Opcode::MakeU64);
                let ptr = VarPointer::new_binary(0, self.func.opcodes.data.len() as u32);
                self.var_temps.push((ptr, expr.loc));
                self.func.opcodes.push(VarPointer::new_binary(0, id));

                if !expr.ty.is_array() {
                    self.func.opcodes.push(Opcode::Get);
                    self.func.opcodes.push(expr.ty.repr_size());
                }
            }

            TCExprKind::Uninit => {
                self.func.opcodes.push(Opcode::PushUndef);
                self.func.opcodes.push(expr.ty.repr_size());
            }
            &TCExprKind::ArrayInit { elems, elem_ty: ty } => {
                for &(kind, loc) in elems {
                    let elem = TCExpr { kind, ty, loc };
                    self.translate_expr(&elem);
                }
            }
            TCExprKind::StructLit { fields, size } => {
                let mut offset = 0;
                for field in *fields {
                    let aligned_offset = align_u32(offset, field.ty.align().into());
                    if aligned_offset > offset {
                        self.func.opcodes.push(Opcode::PushUndef);
                        self.func.opcodes.push(aligned_offset - offset);
                    }

                    self.translate_expr(field);
                    offset = aligned_offset + field.ty.repr_size();
                }

                if offset < *size {
                    self.func.opcodes.push(Opcode::PushUndef);
                    self.func.opcodes.push(*size - offset);
                }
            }
            TCExprKind::ParenList(exprs) => {
                for (idx, expr) in exprs.iter().enumerate() {
                    self.translate_expr(expr);
                    let bytes = expr.ty.repr_size();

                    if idx + 1 < exprs.len() {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(bytes);
                    }
                }
            }

            TCExprKind::PostIncr { incr_ty, value } => {
                use TCPrimType::*;
                self.translate_assign(value);
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
                        self.func.opcodes.push(Opcode::Add32);
                    }
                    I64 | U64 => {
                        self.func.opcodes.push(Opcode::MakeU64);
                        self.func.opcodes.push(1u64);
                        self.func.opcodes.push(Opcode::Add64);
                    }
                    Pointer { stride } => {
                        let stride: u32 = stride.into();
                        self.func.opcodes.push(Opcode::MakeU64);
                        self.func.opcodes.push(stride as u64);
                        self.func.opcodes.push(Opcode::Add64);
                    }
                    x => unimplemented!("post incr for {:?}", x),
                }

                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(bytes * 2);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(Opcode::Set);
                self.func.opcodes.push(bytes);
            }
            TCExprKind::PostDecr { decr_ty, value } => {
                use TCPrimType::*;
                self.translate_assign(value);
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                self.func.opcodes.push(Opcode::Dup);
                self.func.opcodes.push(8u32);

                let bytes = decr_ty.size() as u32;
                self.func.opcodes.push(Opcode::Get);
                self.func.opcodes.push(bytes);

                self.func.opcodes.push(Opcode::Dup);
                self.func.opcodes.push(bytes);

                match decr_ty {
                    I32 => {
                        self.func.opcodes.push(Opcode::MakeI32);
                        self.func.opcodes.push(1u32);
                        self.func.opcodes.push(Opcode::SubI32);
                    }
                    U32 => {
                        self.func.opcodes.push(Opcode::MakeU32);
                        self.func.opcodes.push(1u32);
                        self.func.opcodes.push(Opcode::SubU32);
                    }
                    I64 => {
                        self.func.opcodes.push(Opcode::MakeI64);
                        self.func.opcodes.push(1u64);
                        self.func.opcodes.push(Opcode::SubI64);
                    }
                    U64 => {
                        self.func.opcodes.push(Opcode::MakeU64);
                        self.func.opcodes.push(1u64);
                        self.func.opcodes.push(Opcode::SubU64);
                    }
                    Pointer { stride } => {
                        let stride: u32 = stride.into();
                        self.func.opcodes.push(Opcode::MakeU64);
                        self.func.opcodes.push(stride as u64);
                        self.func.opcodes.push(Opcode::SubU64);
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
                self.translate_expr(left);
                self.translate_expr(right);
                self.translate_bin_op(*op, *op_type, expr.loc);
            }

            TCExprKind::UnaryOp {
                op,
                op_type,
                operand,
            } => {
                self.translate_expr(operand);
                self.translate_un_op(*op, *op_type, expr.loc);
            }

            TCExprKind::Conv { from, to, expr } => {
                self.translate_expr(expr);
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);
                match (from, to.size(), to.is_floating_pt()) {
                    (TCPrimType::I8, 1, false) => {}
                    (TCPrimType::U8, 1, false) => {}
                    (TCPrimType::I8, 2, false) => self.func.opcodes.push(Opcode::SExtend8To16),
                    (TCPrimType::U8, 2, false) => self.func.opcodes.push(Opcode::ZExtend8To16),
                    (TCPrimType::I8, 4, false) => self.func.opcodes.push(Opcode::SExtend8To32),
                    (TCPrimType::U8, 4, false) => self.func.opcodes.push(Opcode::ZExtend8To32),
                    (TCPrimType::I8, 8, false) => self.func.opcodes.push(Opcode::SExtend8To64),
                    (TCPrimType::U8, 8, false) => self.func.opcodes.push(Opcode::ZExtend8To64),

                    (TCPrimType::I16, 1, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(1u32)
                    }
                    (TCPrimType::U16, 1, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(1u32)
                    }
                    (TCPrimType::I16, 2, false) => {}
                    (TCPrimType::U16, 2, false) => {}
                    (TCPrimType::I16, 4, false) => self.func.opcodes.push(Opcode::SExtend16To32),
                    (TCPrimType::U16, 4, false) => self.func.opcodes.push(Opcode::ZExtend16To32),
                    (TCPrimType::I16, 8, false) => self.func.opcodes.push(Opcode::SExtend16To64),
                    (TCPrimType::U16, 8, false) => self.func.opcodes.push(Opcode::ZExtend16To64),

                    (TCPrimType::I32, 1, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(3u32)
                    }
                    (TCPrimType::U32, 1, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(3u32)
                    }
                    (TCPrimType::I32, 2, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(2u32)
                    }
                    (TCPrimType::U32, 2, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(2u32)
                    }
                    (TCPrimType::I32, 4, false) => {}
                    (TCPrimType::U32, 4, false) => {}
                    (TCPrimType::I32, 8, false) => self.func.opcodes.push(Opcode::SExtend32To64),
                    (TCPrimType::U32, 8, false) => self.func.opcodes.push(Opcode::ZExtend32To64),

                    (TCPrimType::I64, 1, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(7u32)
                    }
                    (TCPrimType::U64, 1, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(7u32)
                    }
                    (TCPrimType::I64, 2, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(6u32)
                    }
                    (TCPrimType::U64, 2, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(6u32)
                    }
                    (TCPrimType::I64, 4, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(4u32)
                    }
                    (TCPrimType::U64, 4, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(4u32)
                    }
                    (TCPrimType::I64, 8, false) => {}
                    (TCPrimType::U64, 8, false) => {}

                    (TCPrimType::I8, 4, true) => self.func.opcodes.push(Opcode::I8ToF32),
                    (TCPrimType::U8, 4, true) => self.func.opcodes.push(Opcode::U8ToF32),
                    (TCPrimType::I8, 8, true) => self.func.opcodes.push(Opcode::I8ToF64),
                    (TCPrimType::U8, 8, true) => self.func.opcodes.push(Opcode::U8ToF64),
                    (TCPrimType::I16, 4, true) => self.func.opcodes.push(Opcode::I16ToF32),
                    (TCPrimType::U16, 4, true) => self.func.opcodes.push(Opcode::U16ToF32),
                    (TCPrimType::I16, 8, true) => self.func.opcodes.push(Opcode::I16ToF64),
                    (TCPrimType::U16, 8, true) => self.func.opcodes.push(Opcode::U16ToF64),
                    (TCPrimType::I32, 4, true) => self.func.opcodes.push(Opcode::I32ToF32),
                    (TCPrimType::U32, 4, true) => self.func.opcodes.push(Opcode::U32ToF32),
                    (TCPrimType::I32, 8, true) => self.func.opcodes.push(Opcode::I32ToF64),
                    (TCPrimType::U32, 8, true) => self.func.opcodes.push(Opcode::U32ToF64),
                    (TCPrimType::I64, 4, true) => self.func.opcodes.push(Opcode::I64ToF32),
                    (TCPrimType::U64, 4, true) => self.func.opcodes.push(Opcode::U64ToF32),
                    (TCPrimType::I64, 8, true) => self.func.opcodes.push(Opcode::I64ToF64),
                    (TCPrimType::U64, 8, true) => self.func.opcodes.push(Opcode::U64ToF64),

                    (TCPrimType::F32, 1, false) => self.func.opcodes.push(Opcode::F32ToI8),
                    (TCPrimType::F64, 1, false) => self.func.opcodes.push(Opcode::F64ToI8),
                    (TCPrimType::F32, 2, false) => self.func.opcodes.push(Opcode::F32ToI16),
                    (TCPrimType::F64, 2, false) => self.func.opcodes.push(Opcode::F64ToI16),
                    (TCPrimType::F32, 4, false) => self.func.opcodes.push(Opcode::F32ToI32),
                    (TCPrimType::F64, 4, false) => self.func.opcodes.push(Opcode::F64ToI32),
                    (TCPrimType::F32, 8, false) => self.func.opcodes.push(Opcode::F32ToI64),
                    (TCPrimType::F64, 8, false) => self.func.opcodes.push(Opcode::F64ToI64),

                    (TCPrimType::F32, 4, true) => {}
                    (TCPrimType::F64, 4, true) => self.func.opcodes.push(Opcode::F64ToF32),
                    (TCPrimType::F32, 8, true) => self.func.opcodes.push(Opcode::F32ToF64),
                    (TCPrimType::F64, 8, true) => {}

                    (TCPrimType::Pointer { .. }, 1, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(7u32)
                    }
                    (TCPrimType::Pointer { .. }, 2, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(6u32)
                    }
                    (TCPrimType::Pointer { .. }, 4, false) => {
                        self.func.opcodes.push(Opcode::Pop);
                        self.func.opcodes.push(4u32)
                    }
                    (TCPrimType::Pointer { .. }, 8, false) => {}
                    x => unimplemented!("conversion {:?}", x),
                }
            }

            // TODO fix this with array members
            &TCExprKind::Member { base, offset } => {
                self.translate_expr(base);

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
                self.translate_expr(base);

                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);

                let bytes = expr.ty.repr_size();
                self.func.opcodes.push(Opcode::MakeU64);
                self.func.opcodes.push(offset as u64);
                self.func.opcodes.push(Opcode::Add64);
                if !expr.ty.is_array() {
                    self.func.opcodes.push(Opcode::Get);
                    self.func.opcodes.push(bytes);
                }
            }

            TCExprKind::Assign { target, value } => {
                self.translate_expr(value);

                let bytes: u32 = target.ty.size().into();
                self.func.opcodes.push(Opcode::Dup);
                self.func.opcodes.push(bytes);
                self.translate_assign(target);

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
                self.translate_assign(target);

                self.func.opcodes.push(Opcode::Dup);
                self.func.opcodes.push(8u32);

                let bytes = target.ty.repr_size();
                self.func.opcodes.push(Opcode::Get);
                self.func.opcodes.push(bytes);

                self.translate_expr(value);

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
                self.translate_expr(ptr);
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(expr.loc);

                self.func.opcodes.push(Opcode::Get);
                self.func.opcodes.push(expr.ty.size());
            }
            TCExprKind::Ref(lvalue) => self.translate_assign(lvalue),

            TCExprKind::Call { func, params } => {
                self.translate_expr(func); // callee is sequenced before params

                // Safety allocation to make sure varargs don't run off the stack
                self.func.opcodes.push(Opcode::StackAlloc);
                self.func.opcodes.push(0u32);

                for param in params.iter().rev() {
                    let bytes = param.ty.repr_size();

                    self.translate_expr(param);
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
                self.translate_expr(condition);

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

                self.translate_expr(if_true);
                self.func.opcodes.push(Opcode::Jump);
                self.func.gotos.push(self.func.opcodes.data.len() as u32);
                self.func.opcodes.push(VarPointer::new_binary(0, end_label));
                self.func.labels[else_label as usize].offset = self.func.opcodes.data.len() as u32;
                self.translate_expr(if_false);
                self.func.labels[end_label as usize].offset = self.func.opcodes.data.len() as u32;
            }

            TCExprKind::Builtin(TCBuiltin::Ecall(ecall)) => {
                self.translate_expr(ecall);
                self.func.opcodes.push(Opcode::Ecall);
            }
            TCExprKind::Builtin(TCBuiltin::PushTempStack { ptr, size }) => {
                self.translate_expr(size);
                self.translate_expr(ptr);

                self.func.opcodes.push(Opcode::PushDyn);
            }
        }
    }

    pub fn translate_assign(&mut self, assign: &TCAssignTarget) {
        match assign.kind {
            TCAssignTargetKind::Ptr(expr) => {
                self.translate_expr(expr);
                if assign.offset != 0 {
                    self.func.opcodes.push(Opcode::Loc);
                    self.func.opcodes.push(assign.loc);
                    self.func.opcodes.push(Opcode::MakeU64);
                    self.func.opcodes.push(assign.offset as u64);
                    self.func.opcodes.push(Opcode::Add64);
                }
            }
            TCAssignTargetKind::LocalIdent { label } => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(assign.loc);

                let var = self.func.var_offsets[label as usize];
                self.func.opcodes.push(Opcode::MakeFp);
                self.func.opcodes.push(var);
                if assign.offset != 0 {
                    self.func.opcodes.push(Opcode::MakeU64);
                    self.func.opcodes.push(assign.offset as u64);
                    self.func.opcodes.push(Opcode::Add64);
                }
            }
            TCAssignTargetKind::GlobalIdent { binary_offset } => {
                self.func.opcodes.push(Opcode::Loc);
                self.func.opcodes.push(assign.loc);

                let id = self.file.binary_offsets[binary_offset as usize];
                self.func.opcodes.push(Opcode::MakeU64);
                let ptr = VarPointer::new_binary(0, self.func.opcodes.data.len() as u32);
                self.var_temps.push((ptr, assign.loc));
                self.func.opcodes.push(VarPointer::new_binary(0, id));

                if assign.offset != 0 {
                    self.func.opcodes.push(Opcode::MakeU64);
                    self.func.opcodes.push(assign.offset as u64);
                    self.func.opcodes.push(Opcode::Add64);
                }
            }
        }
    }

    pub fn translate_un_op(&mut self, op: TCUnaryOp, op_type: TCPrimType, loc: CodeLoc) {
        use TCPrimType::*;
        use TCUnaryOp::*;

        self.func.opcodes.push(Opcode::Loc);
        self.func.opcodes.push(loc);

        match (op, op_type) {
            (Neg, I8) => {
                self.func.opcodes.push(Opcode::MakeI8);
                self.func.opcodes.push(-1i8);
                self.func.opcodes.push(Opcode::MulI8);
            }
            (Neg, I16) => {
                self.func.opcodes.push(Opcode::MakeI16);
                self.func.opcodes.push(-1i16);
                self.func.opcodes.push(Opcode::MulI16);
            }
            (Neg, I32) => {
                self.func.opcodes.push(Opcode::MakeI32);
                self.func.opcodes.push(-1i32);
                self.func.opcodes.push(Opcode::MulI32);
            }
            (Neg, I64) => {
                self.func.opcodes.push(Opcode::MakeI64);
                self.func.opcodes.push(-1i64);
                self.func.opcodes.push(Opcode::MulI64);
            }
            (Neg, F32) => {
                self.func.opcodes.push(Opcode::MakeF32);
                self.func.opcodes.push(-1f32);
                self.func.opcodes.push(Opcode::MulF32);
            }
            (Neg, F64) => {
                self.func.opcodes.push(Opcode::MakeF64);
                self.func.opcodes.push(-1f64);
                self.func.opcodes.push(Opcode::MulF64);
            }

            (BoolNorm, I8) => self.func.opcodes.push(Opcode::BoolNorm8),
            (BoolNorm, U8) => self.func.opcodes.push(Opcode::BoolNorm8),
            (BoolNorm, I16) => self.func.opcodes.push(Opcode::BoolNorm16),
            (BoolNorm, U16) => self.func.opcodes.push(Opcode::BoolNorm16),
            (BoolNorm, I32) => self.func.opcodes.push(Opcode::BoolNorm32),
            (BoolNorm, U32) => self.func.opcodes.push(Opcode::BoolNorm32),
            (BoolNorm, I64) => self.func.opcodes.push(Opcode::BoolNorm64),
            (BoolNorm, U64) => self.func.opcodes.push(Opcode::BoolNorm64),
            (BoolNorm, Pointer { .. }) => self.func.opcodes.push(Opcode::BoolNorm64),

            (BoolNot, I8) => self.func.opcodes.push(Opcode::BoolNot8),
            (BoolNot, U8) => self.func.opcodes.push(Opcode::BoolNot8),
            (BoolNot, I16) => self.func.opcodes.push(Opcode::BoolNot16),
            (BoolNot, U16) => self.func.opcodes.push(Opcode::BoolNot16),
            (BoolNot, I32) => self.func.opcodes.push(Opcode::BoolNot32),
            (BoolNot, U32) => self.func.opcodes.push(Opcode::BoolNot32),
            (BoolNot, I64) => self.func.opcodes.push(Opcode::BoolNot64),
            (BoolNot, U64) => self.func.opcodes.push(Opcode::BoolNot64),
            (BoolNot, Pointer { .. }) => self.func.opcodes.push(Opcode::BoolNot64),

            (BitNot, I8) => self.func.opcodes.push(Opcode::BitNot8),
            (BitNot, U8) => self.func.opcodes.push(Opcode::BitNot8),
            (BitNot, I16) => self.func.opcodes.push(Opcode::BitNot16),
            (BitNot, U16) => self.func.opcodes.push(Opcode::BitNot16),
            (BitNot, I32) => self.func.opcodes.push(Opcode::BitNot32),
            (BitNot, U32) => self.func.opcodes.push(Opcode::BitNot32),
            (BitNot, I64) => self.func.opcodes.push(Opcode::BitNot64),
            (BitNot, U64) => self.func.opcodes.push(Opcode::BitNot64),

            x => unimplemented!("unary op: {:?}", x),
        }
    }

    pub fn translate_bin_op(&mut self, op: BinOp, op_type: TCPrimType, loc: CodeLoc) {
        self.func.opcodes.push(Opcode::Loc);
        self.func.opcodes.push(loc);

        let op = match (op, op_type) {
            (BinOp::Add, TCPrimType::I8) => Opcode::Add8,
            (BinOp::Add, TCPrimType::U8) => Opcode::Add8,
            (BinOp::Add, TCPrimType::I16) => Opcode::Add16,
            (BinOp::Add, TCPrimType::U16) => Opcode::Add16,
            (BinOp::Add, TCPrimType::I32) => Opcode::Add32,
            (BinOp::Add, TCPrimType::U32) => Opcode::Add32,
            (BinOp::Add, TCPrimType::I64) => Opcode::Add64,
            (BinOp::Add, TCPrimType::U64) => Opcode::Add64,
            (BinOp::Add, TCPrimType::F32) => Opcode::AddF32,
            (BinOp::Add, TCPrimType::F64) => Opcode::AddF64,
            (BinOp::Add, TCPrimType::Pointer { .. }) => Opcode::Add64,

            (BinOp::Sub, TCPrimType::I8) => Opcode::SubI8,
            (BinOp::Sub, TCPrimType::U8) => Opcode::SubU8,
            (BinOp::Sub, TCPrimType::I16) => Opcode::SubI16,
            (BinOp::Sub, TCPrimType::U16) => Opcode::SubU16,
            (BinOp::Sub, TCPrimType::I32) => Opcode::SubI32,
            (BinOp::Sub, TCPrimType::U32) => Opcode::SubU32,
            (BinOp::Sub, TCPrimType::I64) => Opcode::SubI64,
            (BinOp::Sub, TCPrimType::U64) => Opcode::SubU64,
            (BinOp::Sub, TCPrimType::F32) => Opcode::SubF32,
            (BinOp::Sub, TCPrimType::F64) => Opcode::SubF64,
            (BinOp::Sub, TCPrimType::Pointer { .. }) => Opcode::SubU64,

            (BinOp::Mul, TCPrimType::I8) => Opcode::MulI8,
            (BinOp::Mul, TCPrimType::U8) => Opcode::MulU8,
            (BinOp::Mul, TCPrimType::I16) => Opcode::MulI16,
            (BinOp::Mul, TCPrimType::U16) => Opcode::MulU16,
            (BinOp::Mul, TCPrimType::I32) => Opcode::MulI32,
            (BinOp::Mul, TCPrimType::U32) => Opcode::MulU32,
            (BinOp::Mul, TCPrimType::I64) => Opcode::MulI64,
            (BinOp::Mul, TCPrimType::U64) => Opcode::MulU64,
            (BinOp::Mul, TCPrimType::F32) => Opcode::MulF32,
            (BinOp::Mul, TCPrimType::F64) => Opcode::MulF64,

            (BinOp::Div, TCPrimType::I8) => Opcode::DivI8,
            (BinOp::Div, TCPrimType::U8) => Opcode::DivU8,
            (BinOp::Div, TCPrimType::I16) => Opcode::DivI16,
            (BinOp::Div, TCPrimType::U16) => Opcode::DivU16,
            (BinOp::Div, TCPrimType::I32) => Opcode::DivI32,
            (BinOp::Div, TCPrimType::U32) => Opcode::DivU32,
            (BinOp::Div, TCPrimType::I64) => Opcode::DivI64,
            (BinOp::Div, TCPrimType::U64) => Opcode::DivU64,
            (BinOp::Div, TCPrimType::F32) => Opcode::DivF32,
            (BinOp::Div, TCPrimType::F64) => Opcode::DivF64,

            (BinOp::Mod, TCPrimType::I8) => Opcode::ModI8,
            (BinOp::Mod, TCPrimType::U8) => Opcode::ModU8,
            (BinOp::Mod, TCPrimType::I16) => Opcode::ModI16,
            (BinOp::Mod, TCPrimType::U16) => Opcode::ModU16,
            (BinOp::Mod, TCPrimType::I32) => Opcode::ModI32,
            (BinOp::Mod, TCPrimType::U32) => Opcode::ModU32,
            (BinOp::Mod, TCPrimType::I64) => Opcode::ModI64,
            (BinOp::Mod, TCPrimType::U64) => Opcode::ModU64,
            (BinOp::Mod, TCPrimType::F32) => Opcode::ModF32,
            (BinOp::Mod, TCPrimType::F64) => Opcode::ModF64,

            (BinOp::Eq, TCPrimType::I8) => Opcode::CompEq8,
            (BinOp::Eq, TCPrimType::U8) => Opcode::CompEq8,
            (BinOp::Eq, TCPrimType::I16) => Opcode::CompEq16,
            (BinOp::Eq, TCPrimType::U16) => Opcode::CompEq16,
            (BinOp::Eq, TCPrimType::I32) => Opcode::CompEq32,
            (BinOp::Eq, TCPrimType::U32) => Opcode::CompEq32,
            (BinOp::Eq, TCPrimType::I64) => Opcode::CompEq64,
            (BinOp::Eq, TCPrimType::U64) => Opcode::CompEq64,
            (BinOp::Eq, TCPrimType::Pointer { .. }) => Opcode::CompEq64,
            (BinOp::Eq, TCPrimType::F32) => Opcode::CompEqF32,
            (BinOp::Eq, TCPrimType::F64) => Opcode::CompEqF64,

            (BinOp::Neq, TCPrimType::I8) => Opcode::CompNeq8,
            (BinOp::Neq, TCPrimType::U8) => Opcode::CompNeq8,
            (BinOp::Neq, TCPrimType::I16) => Opcode::CompNeq16,
            (BinOp::Neq, TCPrimType::U16) => Opcode::CompNeq16,
            (BinOp::Neq, TCPrimType::I32) => Opcode::CompNeq32,
            (BinOp::Neq, TCPrimType::U32) => Opcode::CompNeq32,
            (BinOp::Neq, TCPrimType::I64) => Opcode::CompNeq64,
            (BinOp::Neq, TCPrimType::U64) => Opcode::CompNeq64,
            (BinOp::Neq, TCPrimType::Pointer { .. }) => Opcode::CompNeq64,
            (BinOp::Neq, TCPrimType::F32) => Opcode::CompNeqF32,
            (BinOp::Neq, TCPrimType::F64) => Opcode::CompNeqF64,

            (BinOp::Gt, TCPrimType::I8) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(1u32);
                self.func.opcodes.push(1u32);
                Opcode::CompLtI8
            }
            (BinOp::Gt, TCPrimType::U8) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(1u32);
                self.func.opcodes.push(1u32);
                Opcode::CompLtU8
            }
            (BinOp::Gt, TCPrimType::I16) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(22u32);
                self.func.opcodes.push(2u32);
                Opcode::CompLtI16
            }
            (BinOp::Gt, TCPrimType::U16) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(2u32);
                self.func.opcodes.push(2u32);
                Opcode::CompLtU16
            }
            (BinOp::Gt, TCPrimType::I32) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(4u32);
                self.func.opcodes.push(4u32);
                Opcode::CompLtI32
            }
            (BinOp::Gt, TCPrimType::U32) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(4u32);
                self.func.opcodes.push(4u32);
                Opcode::CompLtU32
            }
            (BinOp::Gt, TCPrimType::I64) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(8u32);
                Opcode::CompLtI64
            }
            (BinOp::Gt, TCPrimType::U64) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(8u32);
                Opcode::CompLtU64
            }
            (BinOp::Gt, TCPrimType::F32) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(8u32);
                Opcode::CompLtF32
            }
            (BinOp::Gt, TCPrimType::F64) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(8u32);
                Opcode::CompLtF64
            }

            (BinOp::Lt, TCPrimType::I8) => Opcode::CompLtI8,
            (BinOp::Lt, TCPrimType::U8) => Opcode::CompLtU8,
            (BinOp::Lt, TCPrimType::I16) => Opcode::CompLtI16,
            (BinOp::Lt, TCPrimType::U16) => Opcode::CompLtU16,
            (BinOp::Lt, TCPrimType::I32) => Opcode::CompLtI32,
            (BinOp::Lt, TCPrimType::U32) => Opcode::CompLtU32,
            (BinOp::Lt, TCPrimType::I64) => Opcode::CompLtI64,
            (BinOp::Lt, TCPrimType::U64) => Opcode::CompLtU64,
            (BinOp::Lt, TCPrimType::F32) => Opcode::CompLtF32,
            (BinOp::Lt, TCPrimType::F64) => Opcode::CompLtF64,

            (BinOp::Leq, TCPrimType::I8) => Opcode::CompLeqI8,
            (BinOp::Leq, TCPrimType::U8) => Opcode::CompLeqU8,
            (BinOp::Leq, TCPrimType::I16) => Opcode::CompLeqI16,
            (BinOp::Leq, TCPrimType::U16) => Opcode::CompLeqU16,
            (BinOp::Leq, TCPrimType::I32) => Opcode::CompLeqI32,
            (BinOp::Leq, TCPrimType::U32) => Opcode::CompLeqU32,
            (BinOp::Leq, TCPrimType::I64) => Opcode::CompLeqI64,
            (BinOp::Leq, TCPrimType::U64) => Opcode::CompLeqU64,
            (BinOp::Leq, TCPrimType::F32) => Opcode::CompLeqF32,
            (BinOp::Leq, TCPrimType::F64) => Opcode::CompLeqF64,

            (BinOp::Geq, TCPrimType::I8) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(1u32);
                self.func.opcodes.push(1u32);
                Opcode::CompLeqI8
            }
            (BinOp::Geq, TCPrimType::U8) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(1u32);
                self.func.opcodes.push(1u32);
                Opcode::CompLeqU8
            }
            (BinOp::Geq, TCPrimType::I16) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(22u32);
                self.func.opcodes.push(2u32);
                Opcode::CompLeqI16
            }
            (BinOp::Geq, TCPrimType::U16) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(2u32);
                self.func.opcodes.push(2u32);
                Opcode::CompLeqU16
            }
            (BinOp::Geq, TCPrimType::I32) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(4u32);
                self.func.opcodes.push(4u32);
                Opcode::CompLeqI32
            }
            (BinOp::Geq, TCPrimType::U32) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(4u32);
                self.func.opcodes.push(4u32);
                Opcode::CompLeqU32
            }
            (BinOp::Geq, TCPrimType::I64) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(8u32);
                Opcode::CompLeqI64
            }
            (BinOp::Geq, TCPrimType::U64) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(8u32);
                Opcode::CompLeqU64
            }
            (BinOp::Geq, TCPrimType::F32) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(8u32);
                Opcode::CompLeqF32
            }
            (BinOp::Geq, TCPrimType::F64) => {
                self.func.opcodes.push(Opcode::Swap);
                self.func.opcodes.push(8u32);
                self.func.opcodes.push(8u32);
                Opcode::CompLeqF64
            }

            (BinOp::LShift, TCPrimType::I8) => Opcode::LShiftI8,
            (BinOp::LShift, TCPrimType::U8) => Opcode::LShiftU8,
            (BinOp::LShift, TCPrimType::I16) => Opcode::LShiftI16,
            (BinOp::LShift, TCPrimType::U16) => Opcode::LShiftU16,
            (BinOp::LShift, TCPrimType::I32) => Opcode::LShiftI32,
            (BinOp::LShift, TCPrimType::U32) => Opcode::LShiftU32,
            (BinOp::LShift, TCPrimType::I64) => Opcode::LShiftI64,
            (BinOp::LShift, TCPrimType::U64) => Opcode::LShiftU64,

            (BinOp::RShift, TCPrimType::I8) => Opcode::RShiftI8,
            (BinOp::RShift, TCPrimType::U8) => Opcode::RShiftU8,
            (BinOp::RShift, TCPrimType::I16) => Opcode::RShiftI16,
            (BinOp::RShift, TCPrimType::U16) => Opcode::RShiftU16,
            (BinOp::RShift, TCPrimType::I32) => Opcode::RShiftI32,
            (BinOp::RShift, TCPrimType::U32) => Opcode::RShiftU32,
            (BinOp::RShift, TCPrimType::I64) => Opcode::RShiftI64,
            (BinOp::RShift, TCPrimType::U64) => Opcode::RShiftU64,

            (BinOp::BitAnd, TCPrimType::I8) => Opcode::BitAnd8,
            (BinOp::BitAnd, TCPrimType::U8) => Opcode::BitAnd8,
            (BinOp::BitAnd, TCPrimType::I16) => Opcode::BitAnd16,
            (BinOp::BitAnd, TCPrimType::U16) => Opcode::BitAnd16,
            (BinOp::BitAnd, TCPrimType::I32) => Opcode::BitAnd32,
            (BinOp::BitAnd, TCPrimType::U32) => Opcode::BitAnd32,
            (BinOp::BitAnd, TCPrimType::I64) => Opcode::BitAnd64,
            (BinOp::BitAnd, TCPrimType::U64) => Opcode::BitAnd64,

            (BinOp::BitOr, TCPrimType::I8) => Opcode::BitOr8,
            (BinOp::BitOr, TCPrimType::U8) => Opcode::BitOr8,
            (BinOp::BitOr, TCPrimType::I16) => Opcode::BitOr16,
            (BinOp::BitOr, TCPrimType::U16) => Opcode::BitOr16,
            (BinOp::BitOr, TCPrimType::I32) => Opcode::BitOr32,
            (BinOp::BitOr, TCPrimType::U32) => Opcode::BitOr32,
            (BinOp::BitOr, TCPrimType::I64) => Opcode::BitOr64,
            (BinOp::BitOr, TCPrimType::U64) => Opcode::BitOr64,

            (BinOp::BitXor, TCPrimType::I8) => Opcode::BitXor8,
            (BinOp::BitXor, TCPrimType::U8) => Opcode::BitXor8,
            (BinOp::BitXor, TCPrimType::I16) => Opcode::BitXor16,
            (BinOp::BitXor, TCPrimType::U16) => Opcode::BitXor16,
            (BinOp::BitXor, TCPrimType::I32) => Opcode::BitXor32,
            (BinOp::BitXor, TCPrimType::U32) => Opcode::BitXor32,
            (BinOp::BitXor, TCPrimType::I64) => Opcode::BitXor64,
            (BinOp::BitXor, TCPrimType::U64) => Opcode::BitXor64,

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

        for (temp, loc) in &self.var_temps {
            let ptr: VarPointer = self.data.read(*temp).unwrap();
            let var = &self.vars[ptr.offset() as usize];
            if let Some((vptr, _loc)) = var.header {
                self.data.write(*temp, vptr);
            } else {
                return Err(error!(
                    "couldn't find definition for variable",
                    *loc, "used here", var.decl_loc, "declared here"
                ));
            }
        }

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
