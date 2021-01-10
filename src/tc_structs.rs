use crate::buckets::*;
use crate::filedb::*;
use crate::tc_ast::*;
use crate::util::*;

pub struct ContBr {
    pub cont: u32,
    pub br: u32,
}

pub struct FuncEnv {
    pub ops: Vec<TCOpcode>,
    pub symbol_to_label: HashMap<u32, u32>,
    pub translate_gotos: Vec<u32>,
    pub next_label: u32,
    pub next_symbol_label: u32,

    // const fields
    pub return_type: TCType,
    pub decl_loc: CodeLoc,
}

pub enum TypeEnvKind<'a> {
    Global(GlobalTypeEnv<'a>),
    Local {
        symbols: HashMap<u32, TCVar>,
        scope_idx: u32,
        cont_label: n32,
        break_label: n32,
        parent: *const TypeEnv<'a>,
        global: *const TypeEnv<'a>,
    },
    LocalSwitch {
        symbols: HashMap<u32, TCVar>,
        scope_idx: u32,
        cont_label: n32,
        break_label: u32,
        cases: Vec<(TCExpr, u32)>,
        parent: *const TypeEnv<'a>,
        global: *const TypeEnv<'a>,
    },
}

pub struct TypeEnv<'a> {
    pub kind: TypeEnvKind<'a>,
    pub structs: HashMap<LabelOrLoc, TCStruct>,
    pub structs_in_progress: HashMap<u32, CodeLoc>,
    pub typedefs: HashMap<u32, (&'static TCType, CodeLoc)>,
}

pub struct GlobalTypeEnv<'a> {
    tu: TranslationUnit,
    files: &'a FileDb,
    next_var: u32,
}

pub struct LocalTypeEnv<'a> {
    pub symbols: &'a HashMap<u32, TCVar>,
    pub cases: Option<&'a Vec<(TCExpr, u32)>>,
    pub typedefs: &'a HashMap<u32, (&'static TCType, CodeLoc)>,
}

impl<'a> Allocator<'static> for TypeEnv<'a> {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        return self.globals().0.tu.buckets.alloc(layout);
    }
}

impl<'a> Allocator<'static> for GlobalTypeEnv<'a> {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        return self.tu.buckets.alloc(layout);
    }
}

impl FuncEnv {
    pub fn new(return_type: TCType, decl_loc: CodeLoc) -> Self {
        return FuncEnv {
            ops: Vec::new(),
            symbol_to_label: HashMap::new(),
            translate_gotos: Vec::new(),
            next_label: 0,
            next_symbol_label: 0,
            return_type,
            decl_loc,
        };
    }

    #[inline]
    pub fn label(&mut self) -> u32 {
        let label = self.next_label;
        self.next_label += 1;
        label
    }

    #[inline]
    pub fn symbol(&mut self) -> u32 {
        let sym = self.next_symbol_label;
        self.next_symbol_label += 1;
        sym
    }
}

// impl<'a> Drop for TypeEnv<'a> {
//     fn drop(&mut self) {
//         let scope_idx = match self.kind {
//             TypeEnvKind::Local { scope_idx, .. } => scope_idx,
//             TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
//             TypeEnvKind::Global { .. } => return,
//         };
//
//         if scope_idx != !0 {
//             panic!("didn't close scope");
//         }
//     }
// }

impl<'a> TypeEnv<'a> {
    pub fn global(files: &'a FileDb) -> Self {
        Self {
            kind: TypeEnvKind::Global(GlobalTypeEnv {
                tu: TranslationUnit::new(),
                files,
                next_var: 0,
            }),
            structs: HashMap::new(),
            structs_in_progress: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }

    pub fn tu(mut self) -> TranslationUnit {
        return match &mut self.kind {
            TypeEnvKind::Global(GlobalTypeEnv { tu, .. }) => {
                std::mem::replace(tu, TranslationUnit::new())
            }
            _ => unreachable!(),
        };
    }

    pub fn files(&self) -> &FileDb {
        return &self.globals().0.files;
    }

    pub fn globals(&self) -> (&GlobalTypeEnv<'a>, &TypeEnv<'a>) {
        let global: *const TypeEnv = match self.kind {
            TypeEnvKind::Global(_) => self,
            TypeEnvKind::Local { global, .. } => global,
            TypeEnvKind::LocalSwitch { global, .. } => global,
        };
        let global = unsafe { &*global };

        let global_env = match &global.kind {
            TypeEnvKind::Global(globals) => globals,
            _ => unreachable!(),
        };

        return (global_env, global);
    }

    pub fn globals_mut(&mut self) -> &mut GlobalTypeEnv<'a> {
        let global: *mut TypeEnv = match self.kind {
            TypeEnvKind::Global(_) => self,
            TypeEnvKind::Local { global, .. } => global as *mut TypeEnv,
            TypeEnvKind::LocalSwitch { global, .. } => global as *mut TypeEnv,
        };
        let global = unsafe { &mut *global };

        let global_env = match &mut global.kind {
            TypeEnvKind::Global(globals) => globals,
            _ => unreachable!(),
        };

        return global_env;
    }

    pub fn is_global(&self) -> bool {
        match self.kind {
            TypeEnvKind::Global { .. } => true,
            TypeEnvKind::Local { global, .. } => false,
            TypeEnvKind::LocalSwitch { global, .. } => false,
        }
    }

    pub fn loop_child(&mut self, env: &mut FuncEnv, loc: CodeLoc) -> (Self, ContBr) {
        let cont_label = env.label();
        let break_label = env.label();

        let scope_idx = env.ops.len() as u32;
        let (_, global) = self.globals();

        let kind = TypeEnvKind::Local {
            symbols: HashMap::new(),
            scope_idx,
            cont_label: cont_label.into(),
            break_label: break_label.into(),
            parent: self,
            global,
        };

        let cb = ContBr {
            cont: cont_label,
            br: break_label,
        };

        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::ScopeBegin(HashRef::empty(), !0),
            loc,
        });

        let sel = Self {
            kind,
            structs: HashMap::new(),
            structs_in_progress: HashMap::new(),
            typedefs: HashMap::new(),
        };

        (sel, cb)
    }

    pub fn child(&mut self, env: &mut FuncEnv, loc: CodeLoc) -> Self {
        let (cont_label, break_label) = match self.kind {
            TypeEnvKind::Local {
                cont_label,
                break_label,
                ..
            } => (cont_label, break_label),
            TypeEnvKind::LocalSwitch {
                cont_label,
                break_label,
                ..
            } => (cont_label, break_label.into()),
            TypeEnvKind::Global(_) => (n32::NULL, n32::NULL),
        };
        let (_, global) = self.globals();

        let kind = TypeEnvKind::Local {
            symbols: HashMap::new(),
            scope_idx: env.ops.len() as u32,
            cont_label,
            break_label,
            parent: self,
            global,
        };

        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::ScopeBegin(HashRef::empty(), !0),
            loc,
        });

        Self {
            kind,
            structs: HashMap::new(),
            structs_in_progress: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }

    pub fn switch(&mut self, env: &mut FuncEnv, loc: CodeLoc) -> (Self, u32) {
        let cont_label = match self.kind {
            TypeEnvKind::Local { cont_label, .. } => cont_label,
            TypeEnvKind::LocalSwitch { cont_label, .. } => cont_label,
            _ => unreachable!(),
        };
        let (_, global) = self.globals();

        let break_label = env.label();

        let kind = TypeEnvKind::LocalSwitch {
            symbols: HashMap::new(),
            scope_idx: env.ops.len() as u32,
            cont_label,
            break_label,
            cases: Vec::new(),
            parent: self,
            global,
        };

        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::ScopeBegin(HashRef::empty(), !0),
            loc,
        });

        let sel = Self {
            kind,
            structs: HashMap::new(),
            structs_in_progress: HashMap::new(),
            typedefs: HashMap::new(),
        };

        (sel, break_label)
    }

    pub fn label(&self, env: &mut FuncEnv, label: u32, loc: CodeLoc) {
        let scope_idx = match self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            TypeEnvKind::Global { .. } => unreachable!(),
        };

        let op = TCOpcode {
            kind: TCOpcodeKind::Label { label, scope_idx },
            loc,
        };

        env.ops.push(op);
    }

    pub fn goto(&self, env: &mut FuncEnv, goto: u32, loc: CodeLoc) {
        let scope_idx = match self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            TypeEnvKind::Global { .. } => unreachable!(),
        };

        let op = TCOpcode {
            kind: TCOpcodeKind::Goto { goto, scope_idx },
            loc,
        };

        env.ops.push(op);
    }

    pub fn goto_ifz(&self, env: &mut FuncEnv, cond: TCExpr, goto: u32, loc: CodeLoc) -> bool {
        let scope_idx = match self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            TypeEnvKind::Global { .. } => unreachable!(),
        };

        let cond_ty = if let Some(ty) = cond.ty.to_prim_type() {
            ty
        } else {
            return true;
        };

        let op = TCOpcode {
            kind: TCOpcodeKind::GotoIfZero {
                cond,
                cond_ty,
                goto,
                scope_idx,
            },
            loc,
        };

        env.ops.push(op);
        return false;
    }

    pub fn close_scope(mut self, env: &mut FuncEnv) {
        let scope_idx = match &mut self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            _ => panic!("you don't need to close the global scope"),
        };

        let scope_idx = {
            let idx = *scope_idx;
            if idx == !0 {
                panic!("already closed scope");
            }

            *scope_idx = !0;
            idx
        };

        let symbols = match &self.kind {
            TypeEnvKind::Local { symbols, .. } => symbols,
            TypeEnvKind::LocalSwitch { symbols, .. } => symbols,
            _ => panic!("you don't need to close the global scope"),
        };

        let (scope_end, capa) = (env.ops.len() as u32, symbols.len() * 3 / 2);
        let symbols = symbols.iter().filter_map(|(_, v)| match v.symbol_label {
            LabelOrLoc::Ident(id) => Some((id, v.ty)),
            LabelOrLoc::Loc(_) => None,
        });
        let symbols = HashRef::new_iter(&self, capa, symbols);

        env.ops[scope_idx as usize].kind = TCOpcodeKind::ScopeBegin(symbols, scope_end);
        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::ScopeEnd {
                count: symbols.len() as u32,
                begin: scope_idx,
            },
            loc: env.ops[scope_idx as usize].loc,
        });
    }

    pub fn search_scopes<T, F>(&self, f: F) -> Option<T>
    where
        F: Fn(&TypeEnv) -> Option<T>,
    {
        let mut c_env: *const TypeEnv = self;

        while !c_env.is_null() {
            let current = unsafe { &*c_env };
            if let Some(c) = f(current) {
                return Some(c);
            }

            c_env = match current.kind {
                TypeEnvKind::Global { .. } => break,
                TypeEnvKind::Local { parent, .. } => parent,
                TypeEnvKind::LocalSwitch { parent, .. } => parent,
            };
        }

        return None;
    }

    pub fn search_local_scopes<T, F>(&self, f: F) -> Option<T>
    where
        F: for<'b> Fn(LocalTypeEnv<'b>) -> Option<T>,
    {
        return self.search_scopes(|sel| match &sel.kind {
            TypeEnvKind::Global { .. } => None,
            TypeEnvKind::LocalSwitch { symbols, cases, .. } => f(LocalTypeEnv {
                symbols,
                cases: Some(cases),
                typedefs: &sel.typedefs,
            }),
            TypeEnvKind::Local { symbols, .. } => f(LocalTypeEnv {
                symbols,
                cases: None,
                typedefs: &sel.typedefs,
            }),
        });
    }

    pub fn open_struct_defn(&mut self, id: n32, decl_loc: CodeLoc) -> Result<LabelOrLoc, Error> {
        if id == n32::NULL {
            let (defn, sa) = (None, TC_UNKNOWN_SA);
            let tc_struct = TCStruct { defn, sa, decl_loc };
            let prev = self.structs.insert(LabelOrLoc::Loc(decl_loc), tc_struct);
            assert!(prev.is_none());

            return Ok(LabelOrLoc::Loc(decl_loc));
        } else {
            let prev = self.structs_in_progress.insert(id.into(), decl_loc);
            if let Some(prev) = prev {
                return Err(error!(
                    "nested redefinition of struct",
                    prev, "previous definition here", decl_loc, "new definition here"
                ));
            }

            return Ok(LabelOrLoc::Ident(id.into()));
        }
    }

    pub fn close_struct_defn(
        &mut self,
        id: LabelOrLoc,
        sa: SizeAlign,
        fields: HashMap<u32, TCStructField>,
    ) -> Result<TCTypeBase, Error> {
        debug_assert!(sa.size != n32::NULL);
        debug_assert!(sa.align != n32::NULL);

        let fields = HashRef::new(&*self, &fields);

        let ident = match id {
            LabelOrLoc::Ident(ident) => ident,
            LabelOrLoc::Loc(loc) => {
                let tc_struct = self.structs.get_mut(&id).unwrap();
                tc_struct.defn = Some(TCStructDefn { fields, loc });
                tc_struct.sa = sa;
                return Ok(TCTypeBase::UnnamedStruct { loc, sa });
            }
        };

        let loc = self.structs_in_progress.remove(&ident).unwrap();
        let defn = Some(TCStructDefn { fields, loc });
        match self.structs.entry(id) {
            Entry::Vacant(v) => {
                let decl_loc = loc;
                v.insert(TCStruct { defn, sa, decl_loc });
            }
            Entry::Occupied(mut o) => {
                if let Some(defn) = o.get().defn {
                    return Err(error!(
                        "redefinition of struct",
                        defn.loc, "previous defninition here", loc, "redefinition here"
                    ));
                }

                o.get_mut().defn = defn;
                o.get_mut().sa = sa;
            }
        }

        return Ok(TCTypeBase::NamedStruct { ident, sa });
    }

    pub fn check_struct_decl(&mut self, ident: u32, decl_loc: CodeLoc) -> TCTypeBase {
        match self.structs.entry(LabelOrLoc::Ident(ident)) {
            Entry::Occupied(o) => {
                let sa = o.get().sa;
                return TCTypeBase::NamedStruct { ident, sa };
            }
            Entry::Vacant(v) => {
                let (defn, sa) = (None, TC_UNKNOWN_SA);
                v.insert(TCStruct { defn, sa, decl_loc });

                return TCTypeBase::NamedStruct { ident, sa };
            }
        }
    }

    pub fn get_struct_fields(
        &self,
        id: LabelOrLoc,
    ) -> Option<HashRef<'static, u32, TCStructField>> {
        let opt = self.search_scopes(|env| env.structs.get(&id).map(|a| a.defn));
        return opt.flatten().map(|d| d.fields);
    }

    pub fn check_typedef(&self, ident: u32, loc: CodeLoc) -> Result<TCTypeBase, Error> {
        if let Some((ty, td_loc)) = self.search_scopes(|sel| sel.typedefs.get(&ident).map(|a| *a)) {
            return Ok(TCTypeBase::Typedef {
                refers_to: ty,
                typedef: (ident, td_loc),
            });
        }

        return Err(error!(
            "couldn't find typedef",
            loc, "typedef referenced here"
        ));
    }

    pub fn add_param(&mut self, env: &mut FuncEnv, param: &TCParamDecl) -> Result<(), Error> {
        let symbols = match &mut self.kind {
            TypeEnvKind::Local { symbols, .. } => symbols,
            _ => unreachable!(),
        };

        let TCParamDecl { ty, ident, loc } = *param;
        let symbol_label = LabelOrLoc::Ident(env.symbol());

        let tc_var = TCVar {
            symbol_label,
            ty,
            loc,
        };

        if let Some(prev) = symbols.insert(ident, tc_var) {
            return Err(variable_redeclaration(prev.loc, loc));
        }

        return Ok(());
    }

    pub fn add_local(&mut self, env: &mut FuncEnv, decl: &TCDecl) -> Result<(), Error> {
        let TCDecl { init, ident, .. } = *decl;
        let TCDecl { ty, loc, .. } = *decl;

        let (symbol_label, init_expr) = match init {
            TCDeclInit::Extern | TCDeclInit::ExternInit(_) => unimplemented!(),
            TCDeclInit::Static(_) => {
                let global_env = self.globals_mut();
                let global_ident = TCIdent::ScopedIdent { scope: loc, ident };
                let global_var = TCGlobalVar {
                    init,
                    ty,
                    loc,
                    var_idx: global_env.next_var,
                };

                global_env.tu.variables.insert(global_ident, global_var);
                global_env.next_var += 1;

                (LabelOrLoc::Loc(loc), None)
            }
            TCDeclInit::DefaultUninit => {
                let idx = env.symbol();

                (LabelOrLoc::Ident(idx), None)
            }
            TCDeclInit::Default(init) => {
                let label = env.symbol();

                (LabelOrLoc::Ident(label), Some((label, init)))
            }
        };

        let tc_var = TCVar {
            symbol_label,
            ty,
            loc,
        };

        let symbols = match &mut self.kind {
            TypeEnvKind::Local { symbols, .. } => symbols,
            TypeEnvKind::LocalSwitch { symbols, .. } => symbols,
            _ => unreachable!(),
        };

        let mut prev = match symbols.entry(ident) {
            Entry::Occupied(o) => o,
            Entry::Vacant(v) => {
                v.insert(tc_var);

                if let Some((label, init_expr)) = init_expr {
                    let op = TCOpcode::init_local(self, label, init_expr, ty, loc);
                    env.ops.push(op);
                }

                return Ok(());
            }
        };

        let (prev_ty, prev_loc, decl_loc) = (prev.get().ty, prev.get().loc, decl.loc);
        let or_else = move || variable_redeclaration(prev_loc, decl_loc);
        let (rt, params) = decl.ty.func_parts_strict().ok_or_else(or_else)?;
        let (prev_rt, prev_params) = prev_ty.func_parts_strict().ok_or_else(or_else)?;

        if !TCType::ty_eq(&prev_rt, &rt) {
            return Err(mismatched_return_types(prev.get().loc, decl.loc));
        }

        let prev_static = prev.get().symbol_label.is_loc();
        let is_static = decl.init.is_static();

        if prev_static && !is_static {
            return Err(mismatched_static_decl(prev.get().loc, decl.loc));
        }

        if prev_params[0] != TCTypeModifier::UnknownParams {
            if params[0] != TCTypeModifier::UnknownParams && prev_params != params {
                return Err(mismatched_params(prev_loc, decl.loc));
            }
        } else if params[0] != TCTypeModifier::UnknownParams {
            prev.get_mut().ty = decl.ty;
            prev.get_mut().loc = decl.loc;
            prev.get_mut().symbol_label = symbol_label;
            // TODO this stupid motherfucking language allows functions to be declared in
            // a local scope but still requires a global definition
        }

        return Ok(());
    }

    pub fn add_global(&mut self, decl: &TCDecl) -> Result<(), Error> {
        let global_env = match &mut self.kind {
            TypeEnvKind::Global(g) => g,
            _ => unreachable!(),
        };

        let global_ident = TCIdent::Ident(decl.ident);
        let global_var = TCGlobalVar {
            init: decl.init,
            ty: decl.ty,
            loc: decl.loc,
            var_idx: global_env.next_var,
        };
        global_env.next_var += 1;

        let mut prev = match global_env.tu.variables.entry(global_ident) {
            Entry::Occupied(o) => o,
            Entry::Vacant(v) => {
                v.insert(global_var);

                if let Some(func_type) = decl.ty.to_func_type_strict(&*global_env.tu.buckets) {
                    let tc_function = TCFunction {
                        is_static: decl.init.is_static(),
                        func_type,
                        decl_loc: decl.loc,
                        defn: None,
                    };
                    global_env.tu.functions.insert(decl.ident, tc_function);
                }
                return Ok(());
            }
        };

        let (prev_ty, prev_loc, decl_loc) = (prev.get().ty, prev.get().loc, decl.loc);
        let or_else = move || variable_redeclaration(prev_loc, decl_loc);
        let (rt, params) = decl.ty.func_parts_strict().ok_or_else(or_else)?;
        let (prev_rt, prev_params) = prev_ty.func_parts_strict().ok_or_else(or_else)?;

        if !TCType::ty_eq(&prev_rt, &rt) {
            return Err(mismatched_return_types(prev.get().loc, decl.loc));
        }

        let prev_static = prev.get().init.is_static();
        let is_static = decl.init.is_static();

        if prev_static && !is_static {
            return Err(mismatched_static_decl(prev.get().loc, decl.loc));
        }

        if prev_params[0] != TCTypeModifier::UnknownParams {
            if params[0] != TCTypeModifier::UnknownParams && prev_params != params {
                return Err(mismatched_params(prev_loc, decl.loc));
            }
        } else if params[0] != TCTypeModifier::UnknownParams {
            let prev_func = global_env.tu.functions.get_mut(&decl.ident).unwrap();
            prev.get_mut().ty = decl.ty;
            prev.get_mut().loc = decl.loc;
            prev.get_mut().init = decl.init;

            let mut param_types = Vec::new();
            let mut varargs = false;
            for param in params {
                match param {
                    TCTypeModifier::BeginParam(ty) => param_types.push(*ty),
                    TCTypeModifier::Param(ty) => param_types.push(*ty),
                    TCTypeModifier::VarargsParam => varargs = true,
                    _ => unreachable!(),
                }
            }

            let types = global_env.tu.buckets.add_array(param_types);
            let params = TCParamType { types, varargs };

            prev_func.func_type.params = Some(params);
            prev_func.decl_loc = decl.loc;
            prev_func.is_static = is_static;
        }

        return Ok(());
    }

    pub fn complete_func_defn(&mut self, ident: u32, env: FuncEnv) -> Result<(), Error> {
        let global_env = match &mut self.kind {
            TypeEnvKind::Global(g) => g,
            _ => unreachable!(),
        };
        let func = if let Some(func) = global_env.tu.functions.get_mut(&ident) {
            func
        } else {
            panic!("function being defined doesn't exist")
        };

        if let Some(prev) = func.defn {
            return Err(error!(
                "function already defined",
                prev.loc, "previous definition here", env.decl_loc, "repeated definition here"
            ));
        }

        let param_count = if let Some(params) = func.func_type.params {
            params.types.len() as u32
        } else {
            0
        };

        func.defn = Some(TCFuncDefn {
            ops: global_env.tu.buckets.add_array(env.ops),
            sym_count: env.next_symbol_label,
            param_count,
            loc: env.decl_loc,
        });
        return Ok(());
    }

    pub fn ident(&self, ident: u32, loc: CodeLoc) -> Result<TCExpr, Error> {
        debug_assert!(!self.is_global());

        // search locals
        if let Some(tc_var) = self.search_local_scopes(|sel| sel.symbols.get(&ident).map(|a| *a)) {
            match tc_var.symbol_label {
                LabelOrLoc::Ident(label) => {
                    return Ok(TCExpr {
                        kind: TCExprKind::LocalIdent { label },
                        ty: tc_var.ty,
                        loc,
                    });
                }
                LabelOrLoc::Loc(scope) => {
                    let (global_env, _) = self.globals();
                    let global_var =
                        global_env.tu.variables[&TCIdent::ScopedIdent { scope, ident }];
                    let binary_offset = global_var.var_idx;

                    return Ok(TCExpr {
                        kind: TCExprKind::GlobalIdent { binary_offset },
                        ty: tc_var.ty,
                        loc,
                    });
                }
            }
        }

        // search globals
        let (global_env, _) = self.globals();
        if let Some(global_var) = global_env.tu.variables.get(&TCIdent::Ident(ident)) {
            if global_var.ty.is_function() {
                if let TCDeclInit::Default(kind) = global_var.init {
                    return Ok(TCExpr {
                        kind,
                        ty: global_var.ty,
                        loc,
                    });
                }
            }

            let binary_offset = global_var.var_idx;

            return Ok(TCExpr {
                kind: TCExprKind::GlobalIdent { binary_offset },
                ty: global_var.ty,
                loc,
            });
        }

        return Err(error!("couldn't find symbol", loc, "symbol used here"));
    }

    pub fn assign_ident(&self, ident: u32, loc: CodeLoc) -> Result<TCAssignTarget, Error> {
        debug_assert!(!self.is_global());

        // search locals
        if let Some(tc_var) = self.search_local_scopes(|sel| sel.symbols.get(&ident).map(|a| *a)) {
            if tc_var.ty.is_function() {
                return Err(error!(
                    "can't assign to function type",
                    loc, "assignment to name happens here"
                ));
            }

            match tc_var.symbol_label {
                LabelOrLoc::Ident(label) => {
                    return Ok(TCAssignTarget {
                        kind: TCAssignTargetKind::LocalIdent { label },
                        defn_loc: tc_var.loc,
                        ty: tc_var.ty,
                        loc,
                        offset: 0,
                    });
                }
                LabelOrLoc::Loc(scope) => {
                    let (global_env, _) = self.globals();
                    let global_var =
                        global_env.tu.variables[&TCIdent::ScopedIdent { scope, ident }];
                    let binary_offset = global_var.var_idx;

                    return Ok(TCAssignTarget {
                        kind: TCAssignTargetKind::GlobalIdent { binary_offset },
                        defn_loc: tc_var.loc,
                        ty: tc_var.ty,
                        loc,
                        offset: 0,
                    });
                }
            }
        }

        // search globals
        let (global_env, _) = self.globals();
        if let Some(tc_var) = global_env.tu.variables.get(&TCIdent::Ident(ident)) {
            if tc_var.ty.is_function() {
                return Err(error!(
                    "can't assign to function type",
                    loc, "assignment to name happens here"
                ));
            }

            let binary_offset = tc_var.var_idx;

            return Ok(TCAssignTarget {
                kind: TCAssignTargetKind::GlobalIdent { binary_offset },
                defn_loc: tc_var.loc,
                ty: tc_var.ty,
                loc,
                offset: 0,
            });
        }

        return Err(error!("couldn't find symbol", loc, "symbol used here"));
    }

    pub fn add_typedef(&mut self, ty: TCType, id: u32, loc: CodeLoc) {
        self.typedefs.insert(id, (self.add(ty), loc));
    }
}

pub fn mismatched_return_types(prev_loc: CodeLoc, decl_loc: CodeLoc) -> Error {
    return error!(
        "mismatched declared return types",
        prev_loc, "previous declaration here", decl_loc, "new declaration here"
    );
}

pub fn mismatched_params(prev_loc: CodeLoc, decl_loc: CodeLoc) -> Error {
    return error!(
        "mismatched declared parameter types",
        prev_loc, "previous declaration here", decl_loc, "new declaration here"
    );
}

pub fn mismatched_static_decl(prev_loc: CodeLoc, decl_loc: CodeLoc) -> Error {
    return error!(
        "previous declaration of function is static and new declaration is not",
        prev_loc, "previous declaration here", decl_loc, "new declaration here"
    );
}

pub fn variable_redeclaration(prev: CodeLoc, new: CodeLoc) -> Error {
    return error!(
        "variable already exists in current scope",
        prev, "previous declaration here", new, "new variable of same name declared here"
    );
}
