use crate::filedb::*;
use crate::tc_ast::*;
use crate::util::*;

pub struct ContBr {
    pub cont: u32,
    pub br: u32,
}

pub struct FuncEnv {
    pub ops: Vec<TCOpcode>,
    pub symbol_to_label: HashMap<u32, (u32, CodeLoc)>,
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
        parent: *mut TypeEnv<'a>,
        global: *mut TypeEnv<'a>,
    },
    LocalSwitch {
        symbols: HashMap<u32, TCVar>,
        scope_idx: u32,
        cont_label: n32,
        break_label: u32,
        ty: TCType,
        cases: Vec<(TCExpr, u32)>,
        default: n32,
        default_loc: CodeLoc,
        parent: *mut TypeEnv<'a>,
        global: *mut TypeEnv<'a>,
    },
}

pub struct TypeEnv<'a> {
    pub kind: TypeEnvKind<'a>,
    pub structs: HashMap<LabelOrLoc, TCStruct>,
    pub unions: HashMap<LabelOrLoc, TCStruct>,
    pub structs_in_progress: HashMap<u32, CodeLoc>,
    pub unions_in_progress: HashMap<u32, CodeLoc>,
    pub typedefs: HashMap<u32, (&'static TCType, CodeLoc)>,
}

pub struct GlobalTypeEnv<'a> {
    tu: TranslationUnit,
    symbols: &'a Symbols,
}

pub struct LocalTypeEnv<'a> {
    pub symbols: &'a HashMap<u32, TCVar>,
    pub cases: Option<&'a Vec<(TCExpr, u32)>>,
    pub typedefs: &'a HashMap<u32, (&'static TCType, CodeLoc)>,
}

unsafe impl<'a> Allocator for TypeEnv<'a> {
    fn allocate(
        &self,
        layout: alloc::alloc::Layout,
    ) -> Result<core::ptr::NonNull<[u8]>, aliu::AllocError> {
        return self.globals().0.tu.buckets.allocate(layout);
    }

    // deallocation doesn't do anything
    unsafe fn deallocate(&self, ptr: core::ptr::NonNull<u8>, layout: alloc::alloc::Layout) {
        return self.globals().0.tu.buckets.deallocate(ptr, layout);
    }
}

unsafe impl<'a> Allocator for GlobalTypeEnv<'a> {
    fn allocate(
        &self,
        layout: alloc::alloc::Layout,
    ) -> Result<core::ptr::NonNull<[u8]>, aliu::AllocError> {
        return self.tu.buckets.allocate(layout);
    }

    // deallocation doesn't do anything
    unsafe fn deallocate(&self, ptr: core::ptr::NonNull<u8>, layout: alloc::alloc::Layout) {
        return self.tu.buckets.deallocate(ptr, layout);
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

impl<'a> TypeEnv<'a> {
    pub fn global(file: u32, symbols: &'a Symbols) -> Self {
        Self {
            kind: TypeEnvKind::Global(GlobalTypeEnv {
                tu: TranslationUnit::new(file),
                symbols,
            }),
            structs: HashMap::new(),
            unions: HashMap::new(),
            structs_in_progress: HashMap::new(),
            unions_in_progress: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }

    pub fn tu(mut self) -> TranslationUnit {
        return match &mut self.kind {
            TypeEnvKind::Global(GlobalTypeEnv { tu, .. }) => {
                core::mem::replace(tu, TranslationUnit::new(!0))
            }
            _ => unreachable!(),
        };
    }

    pub fn symbols(&self) -> &Symbols {
        return &self.globals().0.symbols;
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
        let parent_scope = match self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            TypeEnvKind::Global { .. } => unreachable!(),
        };

        let global = match self.kind {
            TypeEnvKind::Local { global, .. } => global,
            TypeEnvKind::LocalSwitch { global, .. } => global,
            TypeEnvKind::Global { .. } => self,
        };

        let cont_label = env.label();
        let break_label = env.label();

        let scope_idx = env.ops.len() as u32;

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
            kind: TCOpcodeKind::ScopeBegin(aliu::HashRef::empty(), parent_scope),
            loc,
        });

        let sel = Self {
            kind,
            structs: HashMap::new(),
            unions: HashMap::new(),
            structs_in_progress: HashMap::new(),
            unions_in_progress: HashMap::new(),
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

        let parent_scope = match self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            TypeEnvKind::Global { .. } => !0,
        };

        let global = match self.kind {
            TypeEnvKind::Local { global, .. } => global,
            TypeEnvKind::LocalSwitch { global, .. } => global,
            TypeEnvKind::Global { .. } => self,
        };

        let kind = TypeEnvKind::Local {
            symbols: HashMap::new(),
            scope_idx: env.ops.len() as u32,
            cont_label,
            break_label,
            parent: self,
            global,
        };

        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::ScopeBegin(aliu::HashRef::empty(), parent_scope),
            loc,
        });

        Self {
            kind,
            structs: HashMap::new(),
            unions: HashMap::new(),
            structs_in_progress: HashMap::new(),
            unions_in_progress: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }

    pub fn switch(
        &mut self,
        expr: TCExpr,
        env: &mut FuncEnv,
        loc: CodeLoc,
    ) -> Result<(Self, u32), Error> {
        if !expr.ty.is_integer() {
            return Err(error!(
                "switch expression's type must be an integer type",
                expr.loc,
                format!("expression has type {}", expr.ty.display(self.symbols()))
            ));
        }

        let cont_label = match self.kind {
            TypeEnvKind::Local { cont_label, .. } => cont_label,
            TypeEnvKind::LocalSwitch { cont_label, .. } => cont_label,
            _ => unreachable!(),
        };

        let parent_scope = match self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            TypeEnvKind::Global { .. } => !0,
        };

        let global = match self.kind {
            TypeEnvKind::Local { global, .. } => global,
            TypeEnvKind::LocalSwitch { global, .. } => global,
            TypeEnvKind::Global { .. } => self,
        };

        let break_label = env.label();

        let kind = TypeEnvKind::LocalSwitch {
            symbols: HashMap::new(),
            scope_idx: env.ops.len() as u32,
            cont_label,
            break_label,
            ty: expr.ty,
            cases: Vec::new(),
            default: n32::NULL,
            default_loc: NO_FILE,
            parent: self,
            global,
        };

        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::ScopeBegin(aliu::HashRef::empty(), parent_scope),
            loc,
        });
        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::Switch {
                expr,
                cases: &[],
                default: !0,
            },
            loc: expr.loc,
        });

        let sel = Self {
            kind,
            structs: HashMap::new(),
            unions: HashMap::new(),
            structs_in_progress: HashMap::new(),
            unions_in_progress: HashMap::new(),
            typedefs: HashMap::new(),
        };

        Ok((sel, break_label))
    }

    pub fn default(&mut self, env: &mut FuncEnv, loc: CodeLoc) -> Result<(), Error> {
        let mut c_env: *mut TypeEnv = self;

        while !c_env.is_null() {
            let current = unsafe { &mut *c_env };

            let (scope_idx, default, d_loc) = match &mut current.kind {
                TypeEnvKind::Global { .. } => break,
                TypeEnvKind::LocalSwitch {
                    scope_idx,
                    default,
                    default_loc,
                    ..
                } => (*scope_idx, default, default_loc),
                TypeEnvKind::Local { parent, .. } => {
                    c_env = *parent;
                    continue;
                }
            };

            if *default != n32::NULL {
                return Err(error!(
                    "default case used multiple times in same switch statement",
                    *d_loc, "first default case here", loc, "second default case here"
                ));
            }

            let label = env.label();
            *default = label.into();
            *d_loc = loc;

            let op = TCOpcode {
                kind: TCOpcodeKind::Label { label, scope_idx },
                loc,
            };
            env.ops.push(op);

            return Ok(());
        }

        return Err(error!(
            "default used when not in a switch",
            loc, "default used here"
        ));
    }

    pub fn case(&mut self, env: &mut FuncEnv, expr: TCExpr, loc: CodeLoc) -> Result<(), Error> {
        if !expr.ty.is_integer() {
            return Err(error!(
                "case expression's type must be an integer type",
                expr.loc,
                format!("expression has type {}", expr.ty.display(self.symbols()))
            ));
        }

        let mut c_env: *mut TypeEnv = self;

        while !c_env.is_null() {
            let current = unsafe { &mut *c_env };

            let (scope_idx, cases, ty) = match &mut current.kind {
                TypeEnvKind::Global { .. } => break,
                TypeEnvKind::LocalSwitch {
                    scope_idx,
                    cases,
                    ty,
                    ..
                } => (*scope_idx, cases, *ty),
                TypeEnvKind::Local { parent, .. } => {
                    c_env = *parent;
                    continue;
                }
            };

            let or_else = || {
                error!(
                    "couldn't convert case value to switch expression type",
                    expr.loc,
                    format!(
                        "case expression (type={}) couldn't be converted to {}",
                        expr.ty.display(self.symbols()),
                        ty.display(self.symbols())
                    )
                )
            };
            let expr = self
                .assign_convert(ty, expr, expr.loc)
                .ok_or_else(or_else)?;
            let label = env.label();
            cases.push((expr, label));
            let op = TCOpcode {
                kind: TCOpcodeKind::Label { label, scope_idx },
                loc: expr.loc,
            };
            env.ops.push(op);

            return Ok(());
        }

        return Err(error!(
            "case used when not in a switch",
            loc, "case used here"
        ));
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

    pub fn user_label(
        &self,
        env: &mut FuncEnv,
        user_label: u32,
        loc: CodeLoc,
    ) -> Result<(), Error> {
        let scope_idx = match self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            TypeEnvKind::Global { .. } => unreachable!(),
        };

        let label = env.label();
        if let Some((_, prev_loc)) = env.symbol_to_label.insert(user_label, (label, loc)) {
            return Err(error!(
                "label defined multiple times in same function",
                prev_loc, "first definition here", loc, "second definition here"
            ));
        }

        let op = TCOpcode {
            kind: TCOpcodeKind::Label { label, scope_idx },
            loc,
        };

        env.ops.push(op);
        return Ok(());
    }

    pub fn user_goto(&self, env: &mut FuncEnv, goto: u32, loc: CodeLoc) {
        let scope_idx = match self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            TypeEnvKind::Global { .. } => unreachable!(),
        };

        let op = TCOpcode {
            kind: TCOpcodeKind::Goto { goto, scope_idx },
            loc,
        };

        env.translate_gotos.push(env.ops.len() as u32);
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

    pub fn goto_ifnz(&self, env: &mut FuncEnv, cond: TCExpr, goto: u32, loc: CodeLoc) -> bool {
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
            kind: TCOpcodeKind::GotoIfNotZero {
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

    pub fn br(&self, env: &mut FuncEnv, loc: CodeLoc) -> bool {
        let scope_idx = match self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            TypeEnvKind::Global { .. } => unreachable!(),
        };
        let break_label = match self.kind {
            TypeEnvKind::Local { break_label, .. } => break_label,
            TypeEnvKind::LocalSwitch { break_label, .. } => break_label.into(),
            TypeEnvKind::Global { .. } => unreachable!(),
        };

        if break_label == n32::NULL {
            return true;
        }

        let goto: u32 = break_label.into();
        let op = TCOpcode {
            kind: TCOpcodeKind::Goto { goto, scope_idx },
            loc,
        };

        env.ops.push(op);
        return false;
    }

    pub fn cont(&self, env: &mut FuncEnv, loc: CodeLoc) -> bool {
        let scope_idx = match self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            TypeEnvKind::Global { .. } => unreachable!(),
        };
        let cont_label = match self.kind {
            TypeEnvKind::Local { cont_label, .. } => cont_label,
            TypeEnvKind::LocalSwitch { cont_label, .. } => cont_label,
            TypeEnvKind::Global { .. } => unreachable!(),
        };

        if cont_label == n32::NULL {
            return true;
        }

        let goto: u32 = cont_label.into();
        let op = TCOpcode {
            kind: TCOpcodeKind::Goto { goto, scope_idx },
            loc,
        };

        env.ops.push(op);
        return false;
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
            LabelOrLoc::Loc(_) => None, // static variable
        });
        let symbols = aliu::HashRef::new_iter(&self, capa, symbols);

        if let TCOpcodeKind::ScopeBegin(syms, _) = &mut env.ops[scope_idx as usize].kind {
            *syms = symbols;
        } else {
            panic!("scope_idx pointed to wrong opcode")
        }

        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::ScopeEnd {
                count: symbols.len() as u32,
                begin: scope_idx,
            },
            loc: env.ops[scope_idx as usize].loc,
        });

        let (s_cases, s_default, br) = match &self.kind {
            TypeEnvKind::LocalSwitch {
                cases,
                default,
                break_label,
                ..
            } => (cases, default, break_label),
            _ => return,
        };

        let s_default = s_default.unwrap_or(*br);

        if let TCOpcodeKind::Switch {
            expr,
            cases,
            default,
        } = &mut env.ops[scope_idx as usize + 1].kind
        {
            *cases = self.add_slice(s_cases);
            *default = s_default;
        } else {
            panic!("scope_idx pointed to wrong opcode")
        }
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

    pub fn open_union_defn(&mut self, id: n32, decl_loc: CodeLoc) -> Result<LabelOrLoc, Error> {
        if id == n32::NULL {
            let (defn, sa) = (None, TC_UNKNOWN_SA);
            let tc_struct = TCStruct { defn, sa, decl_loc };
            let prev = self.unions.insert(LabelOrLoc::Loc(decl_loc), tc_struct);
            assert!(prev.is_none());

            return Ok(LabelOrLoc::Loc(decl_loc));
        } else {
            let prev = self.unions_in_progress.insert(id.into(), decl_loc);
            if let Some(prev) = prev {
                return Err(error!(
                    "nested redefinition of union",
                    prev, "previous definition here", decl_loc, "new definition here"
                ));
            }

            return Ok(LabelOrLoc::Ident(id.into()));
        }
    }

    pub fn close_union_defn(
        &mut self,
        id: LabelOrLoc,
        sa: SizeAlign,
        fields: Vec<TCStructField>,
    ) -> Result<TCTypeBase, Error> {
        debug_assert!(sa.size != n32::NULL);
        debug_assert!(sa.align != n32::NULL);

        let fields = self.add_slice(&*fields);

        let ident = match id {
            LabelOrLoc::Ident(ident) => ident,
            LabelOrLoc::Loc(loc) => {
                let tc_struct = self.unions.get_mut(&id).unwrap();
                tc_struct.defn = Some(TCStructDefn { fields, loc });
                tc_struct.sa = sa;
                return Ok(TCTypeBase::UnnamedUnion { loc, sa });
            }
        };

        let loc = self.unions_in_progress.remove(&ident).unwrap();
        let defn = Some(TCStructDefn { fields, loc });
        match self.unions.entry(id) {
            Entry::Vacant(v) => {
                let decl_loc = loc;
                v.insert(TCStruct { defn, sa, decl_loc });
            }
            Entry::Occupied(mut o) => {
                if let Some(defn) = o.get().defn {
                    return Err(error!(
                        "redefinition of union",
                        defn.loc, "previous defninition here", loc, "redefinition here"
                    ));
                }

                o.get_mut().defn = defn;
                o.get_mut().sa = sa;
            }
        }

        return Ok(TCTypeBase::NamedUnion { ident, sa });
    }

    pub fn check_union_decl(&mut self, ident: u32, decl_loc: CodeLoc) -> TCTypeBase {
        let label = LabelOrLoc::Ident(ident);
        if let Some(sa) = self.search_scopes(|te| te.unions.get(&label).map(|a| a.sa)) {
            return TCTypeBase::NamedUnion { ident, sa };
        }

        let (defn, sa) = (None, TC_UNKNOWN_SA);
        self.unions.insert(label, TCStruct { defn, sa, decl_loc });
        return TCTypeBase::NamedUnion { ident, sa };
    }

    pub fn open_struct_defn(&mut self, id: n32, decl_loc: CodeLoc) -> Result<LabelOrLoc, Error> {
        if id == n32::NULL {
            let (defn, sa) = (None, TC_UNKNOWN_SA);
            let tc_struct = TCStruct { defn, sa, decl_loc };
            let prev = self.structs.insert(LabelOrLoc::Loc(decl_loc), tc_struct);

            // if prev is not none, a file defining an anonymous struct was included multiple times

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
        fields: Vec<TCStructField>,
    ) -> Result<TCTypeBase, Error> {
        debug_assert!(sa.size != n32::NULL);
        debug_assert!(sa.align != n32::NULL);

        let fields = self.add_slice(&*fields);

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
        let label = LabelOrLoc::Ident(ident);
        if let Some(sa) = self.search_scopes(|te| te.structs.get(&label).map(|a| a.sa)) {
            return TCTypeBase::NamedStruct { ident, sa };
        }

        let (defn, sa) = (None, TC_UNKNOWN_SA);
        self.structs.insert(label, TCStruct { defn, sa, decl_loc });
        return TCTypeBase::NamedStruct { ident, sa };
    }

    pub fn get_struct_fields(&self, id: LabelOrLoc) -> Option<&'static [TCStructField]> {
        let opt = self.search_scopes(|env| env.structs.get(&id).map(|a| a.defn));
        return opt.flatten().map(|d| d.fields);
    }

    pub fn get_union_fields(&self, id: LabelOrLoc) -> Option<&'static [TCStructField]> {
        let opt = self.search_scopes(|env| env.unions.get(&id).map(|a| a.defn));
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

    pub fn add_var(&mut self, env: Option<&mut FuncEnv>, decl: &TCDecl) -> Result<(), Error> {
        let env = if let Some(env) = env {
            env
        } else {
            return Self::add_global(self.globals_mut(), decl);
        };

        let TCDecl { init, ident, .. } = *decl;
        let TCDecl { ty, loc, .. } = *decl;

        let (symbol_label, init_expr) = match init {
            TCDeclInit::Extern | TCDeclInit::ExternInit(_) => unimplemented!(),
            TCDeclInit::Static(init) => {
                let global_env = self.globals_mut();
                let global_ident = LabelOrLoc::Loc(loc);
                let global_var = TCStaticInternalVar {
                    init,
                    ty,
                    var_idx: global_env.tu.var_count,
                };

                global_env.tu.static_internal_vars.insert(loc, global_var);
                global_env.tu.var_count += 1;

                (global_ident, None)
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
                    let op = TCOpcode::init_local(&*self, label, init_expr, ty, loc);
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

        if let Some((label, init_expr)) = init_expr {
            let op = TCOpcode::init_local(&*self, label, init_expr, ty, loc);
            env.ops.push(op);
        }

        return Ok(());
    }

    pub fn add_global(global_env: &mut GlobalTypeEnv, decl: &TCDecl) -> Result<(), Error> {
        let global_var = TCGlobalVar {
            init: decl.init,
            ty: decl.ty,
            loc: decl.loc,
            var_idx: global_env.tu.var_count,
        };
        global_env.tu.var_count += 1;

        let mut prev = match global_env.tu.vars.entry(decl.ident) {
            Entry::Occupied(o) => o,
            Entry::Vacant(v) => {
                v.insert(global_var);

                if let Some(func_type) = decl.ty.to_func_type_strict(&global_env.tu.buckets) {
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

        if let TCDeclInit::Extern = prev.get().init {
            if !TCType::ty_eq(&prev.get().ty, &decl.ty) {
                return Err(error!(
                    "previous declaration of variable had different type",
                    prev.get().loc,
                    "previous declaration was extern so there's no redeclaration error",
                    decl.loc,
                    "new declaration was here with different type"
                ));
            }

            prev.get_mut().init = decl.init;
            prev.get_mut().loc = decl.loc;
            return Ok(());
        }

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

            let types = global_env.tu.buckets.add_slice(&*param_types);
            let params = TCParamType { types, varargs };

            prev_func.func_type.params = Some(params);
            prev_func.decl_loc = decl.loc;
            prev_func.is_static = is_static;
        }

        return Ok(());
    }

    pub fn complete_func_defn(&mut self, ident: u32, mut env: FuncEnv) -> Result<(), Error> {
        let global_env = match &mut self.kind {
            TypeEnvKind::Global(g) => g,
            _ => unreachable!(),
        };
        let func = if let Some(func) = global_env.tu.functions.get_mut(&ident) {
            func
        } else {
            return Err(error!(
                "function being defined doesn't exist (this is a bug in TCI)",
                env.decl_loc, "function definition here"
            ));
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

        let or_else = |loc: CodeLoc| {
            move || error!("couldn't find label", loc, "referenced by goto found here")
        };
        for goto_idx in env.translate_gotos {
            let op = &mut env.ops[goto_idx as usize];
            let goto = match &mut op.kind {
                TCOpcodeKind::Goto { goto, scope_idx } => goto,
                _ => {
                    return Err(error!(
                        "goto index was incorrect (this is a bug in TCI)",
                        op.loc, "this was supposed to be a goto"
                    ))
                }
            };

            let (label, loc) = env.symbol_to_label.get(&goto).ok_or_else(or_else(op.loc))?;
            *goto = *label;
        }

        func.defn = Some(TCFuncDefn {
            ops: global_env.tu.buckets.add_slice(&*env.ops),
            sym_count: env.next_symbol_label,
            label_count: env.next_label,
            param_count,
            loc: env.decl_loc,
        });
        return Ok(());
    }

    pub fn ident(&self, ident: u32, loc: CodeLoc) -> Result<TCExpr, Error> {
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
                LabelOrLoc::Loc(label) => {
                    let (global_env, _) = self.globals();
                    let global_var = global_env.tu.static_internal_vars[&label];
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
        if let Some(global_var) = global_env.tu.vars.get(&ident) {
            if global_var.ty.is_function() {
                return Ok(TCExpr {
                    kind: TCExprKind::FunctionIdent { ident },
                    ty: global_var.ty,
                    loc,
                });
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
                LabelOrLoc::Loc(label) => {
                    let (global_env, _) = self.globals();
                    let global_var = global_env.tu.static_internal_vars[&label];
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
        if let Some(tc_var) = global_env.tu.vars.get(&ident) {
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
        self.typedefs.insert(id, (self.new(ty), loc));
    }

    pub fn assign_convert(&self, ty: TCType, expr: TCExpr, loc: CodeLoc) -> Option<TCExpr> {
        if TCType::ty_eq(&ty, &expr.ty) {
            return Some(expr);
        }

        if ty.is_void() {
            let mut exprs = vec![expr];
            exprs.push(TCExpr {
                kind: TCExprKind::Uninit,
                ty: TCType::new(TCTypeBase::Void),
                loc,
            });

            return Some(TCExpr {
                kind: TCExprKind::ParenList(self.add_slice(&*exprs)),
                ty: TCType::new(TCTypeBase::Void),
                loc,
            });
        }

        let to = ty.to_prim_type()?;

        use TCExprKind::*;
        use TCPrimType::*;
        let kind = match (expr.kind, to) {
            (I32Lit(i), I8) => I8Lit(i as i8),
            (I32Lit(i), U8) => U8Lit(i as u8),
            (I32Lit(i), I16) => I16Lit(i as i16),
            (I32Lit(i), U16) => U16Lit(i as u16),
            (I32Lit(i), I32) => I32Lit(i as i32),
            (I32Lit(i), U32) => U32Lit(i as u32),
            (I32Lit(i), I64) => I64Lit(i as i64),
            (I32Lit(i), U64) => U64Lit(i as u64),
            (I32Lit(i), F32) => F32Lit(i as f32),
            (I32Lit(i), F64) => F64Lit(i as f64),
            (I32Lit(i), Pointer { .. }) => U64Lit(i as u64),

            (kind, to) => {
                let from = expr.ty.to_prim_type()?;
                let expr = self.new(expr);

                if core::mem::discriminant(&from) == core::mem::discriminant(&to) {
                    TypePun(expr)
                } else {
                    Conv { from, to, expr }
                }
            }
        };

        return Some(TCExpr { kind, ty, loc });
    }

    // TODO size checks require a lookup because definitions can be completed later
    // pub fn ty_base_size(&self, base: TCTypeBase) -> n32 {
    //     match self {
    //         TCTypeBase::I8 | TCTypeBase::U8 => 1u32.into(),
    //         TCTypeBase::I16 | TCTypeBase::U16 => 2u32.into(),
    //         TCTypeBase::U32 | TCTypeBase::I32 | TCTypeBase::F32 => 4u32.into(),
    //         TCTypeBase::U64 | TCTypeBase::I64 | TCTypeBase::F64 => 8u32.into(),
    //         TCTypeBase::Void => return n32::NULL,
    //         TCTypeBase::NamedStruct { ident, .. } => sa.size,
    //         TCTypeBase::UnnamedStruct { loc, .. } => sa.size,
    //         TCTypeBase::NamedUnion { ident, .. } => sa.size,
    //         TCTypeBase::UnnamedUnion { loc, .. } => sa.size,
    //         TCTypeBase::InternalTypedef(def) => self.ty_size(def),
    //         TCTypeBase::Typedef { refers_to, .. } => self.ty_size(refers_to),
    //     }
    // }

    // pub fn ty_size(&self, ty: &impl TCTy) -> n32 {}
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
