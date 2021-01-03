use crate::buckets::*;
use crate::new_tc_ast::*;
use crate::util::*;

pub struct FuncEnv {
    pub ops: Vec<TCOpcode>,
    pub gotos: Vec<u32>,
    pub next_label: u32,
    pub next_decl_idx: i16,

    // const fields
    pub return_type: TCType,
    pub rtype_loc: CodeLoc,
}

pub enum TypeEnvKind {
    Global(GlobalTypeEnv),
    Local {
        symbols: HashMap<u32, TCVar>,
        parent: *const TypeEnv,
        global: *const TypeEnv,
        decl_idx: i16,
    },
    LocalSwitch {
        symbols: HashMap<u32, TCVar>,
        cases: HashMap<TCExpr, u32>,
        parent: *const TypeEnv,
        global: *const TypeEnv,
        decl_idx: i16,
    },
}

pub struct TypeEnv {
    pub kind: TypeEnvKind,
    pub typedefs: HashMap<u32, (&'static TCType, CodeLoc)>,
    pub builtins_enabled: bool,
}

pub struct GlobalTypeEnv {
    symbols: HashMap<TCIdent, TCGlobalVar>,
    functions: HashMap<u32, TCFunction>,
    tu: TranslationUnit,
    next_var: u32,
}

pub struct LocalTypeEnv<'a> {
    pub symbols: &'a HashMap<u32, TCVar>,
    pub cases: Option<&'a HashMap<TCExpr, u32>>,
    pub decl_idx: &'a i16,
    pub typedefs: &'a HashMap<u32, (&'static TCType, CodeLoc)>,
    pub builtins_enabled: bool,
}

impl Allocator<'static> for TypeEnv {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        return self.globals().0.tu.buckets.alloc(layout);
    }
}

impl Allocator<'static> for GlobalTypeEnv {
    unsafe fn alloc(&self, layout: std::alloc::Layout) -> *mut u8 {
        return self.tu.buckets.alloc(layout);
    }
}

impl TypeEnv {
    pub fn global() -> Self {
        Self {
            kind: TypeEnvKind::Global(GlobalTypeEnv {
                symbols: HashMap::new(),
                functions: HashMap::new(),
                tu: TranslationUnit::new(),
                next_var: 0,
            }),
            typedefs: HashMap::new(),
            builtins_enabled: false,
        }
    }

    pub fn tu(self) -> TranslationUnit {
        return match self.kind {
            TypeEnvKind::Global(GlobalTypeEnv { tu, .. }) => tu,
            _ => unreachable!(),
        };
    }

    pub fn globals(&self) -> (&GlobalTypeEnv, &TypeEnv) {
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

    pub fn globals_mut(&mut self) -> &mut GlobalTypeEnv {
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

    pub fn new_func(&mut self) -> Self {
        debug_assert!(self.is_global());

        let (_, global) = self.globals();

        let kind = TypeEnvKind::Local {
            symbols: HashMap::new(),
            parent: self,
            global,
            decl_idx: -1,
        };

        Self {
            kind,
            typedefs: HashMap::new(),
            builtins_enabled: false,
        }
    }

    pub fn child(&mut self) -> Self {
        let (_, global) = self.globals();

        let decl_idx = match self.kind {
            TypeEnvKind::Global { .. } => unreachable!(),
            TypeEnvKind::Local { decl_idx, .. } => decl_idx,
            TypeEnvKind::LocalSwitch { decl_idx, .. } => decl_idx,
        };

        let kind = TypeEnvKind::Local {
            symbols: HashMap::new(),
            parent: self,
            global,
            decl_idx,
        };

        Self {
            kind,
            typedefs: HashMap::new(),
            builtins_enabled: false,
        }
    }

    pub fn switch(&mut self) -> Self {
        let (_, global) = self.globals();

        let decl_idx = match self.kind {
            TypeEnvKind::Global { .. } => unreachable!(),
            TypeEnvKind::Local { decl_idx, .. } => decl_idx,
            TypeEnvKind::LocalSwitch { decl_idx, .. } => decl_idx,
        };

        let kind = TypeEnvKind::LocalSwitch {
            symbols: HashMap::new(),
            cases: HashMap::new(),
            parent: self,
            global,
            decl_idx,
        };

        Self {
            kind,
            typedefs: HashMap::new(),
            builtins_enabled: false,
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
        F: for<'a> Fn(LocalTypeEnv<'a>) -> Option<T>,
    {
        return self.search_scopes(|sel| match &sel.kind {
            TypeEnvKind::Global { .. } => None,
            TypeEnvKind::LocalSwitch {
                symbols,
                decl_idx,
                cases,
                ..
            } => f(LocalTypeEnv {
                symbols,
                cases: Some(cases),
                decl_idx,
                typedefs: &sel.typedefs,
                builtins_enabled: sel.builtins_enabled,
            }),
            TypeEnvKind::Local {
                symbols, decl_idx, ..
            } => f(LocalTypeEnv {
                symbols,
                cases: None,
                decl_idx,
                typedefs: &sel.typedefs,
                builtins_enabled: sel.builtins_enabled,
            }),
        });
    }

    pub fn check_typedef(&self, ident: u32, loc: CodeLoc) -> Result<TCTypeBase, Error> {
        if let Some((ty, loc)) = self.search_scopes(|sel| sel.typedefs.get(&ident).map(|a| *a)) {
            return Ok(TCTypeBase::Typedef {
                refers_to: ty,
                typedef: (ident.into(), loc),
            });
        }

        return Err(error!(
            "couldn't find typedef",
            loc, "typedef referenced here"
        ));
    }

    pub fn add_param(&mut self, ty: TCType, id: u32, loc: CodeLoc) -> Result<(), Error> {
        let (decl_idx, symbols) = match &mut self.kind {
            TypeEnvKind::Local {
                decl_idx, symbols, ..
            } => (decl_idx, symbols),
            _ => unreachable!(),
        };

        let idx = *decl_idx;
        debug_assert!(idx < 0);

        *decl_idx -= 1;

        let tc_var = TCVar {
            var_offset: OffsetOrLoc::LocalOffset(idx),
            ty,
            loc,
        };

        if let Some(prev) = symbols.insert(id, tc_var) {
            return Err(error!(
                "variable already exists in current scope",
                prev.loc, "previous declared here", loc, "new variable of same name declared here"
            ));
        }

        return Ok(());
    }

    pub fn add_local(&mut self, ty: TCType, id: u32, loc: CodeLoc) -> Result<(), Error> {
        let (decl_idx, symbols) = match &mut self.kind {
            TypeEnvKind::Local {
                decl_idx, symbols, ..
            } => (decl_idx, symbols),
            TypeEnvKind::LocalSwitch {
                decl_idx, symbols, ..
            } => (decl_idx, symbols),
            _ => unreachable!(),
        };

        let idx = if *decl_idx < 0 {
            *decl_idx = 1;
            0
        } else {
            *decl_idx += 1;
            *decl_idx - 1
        };

        let tc_var = TCVar {
            var_offset: OffsetOrLoc::LocalOffset(idx),
            ty,
            loc,
        };

        if let Some(prev) = symbols.insert(id, tc_var) {
            return Err(error!(
                "variable already exists in current scope",
                prev.loc, "previous declared here", loc, "new variable of same name declared here"
            ));
        }

        return Ok(());
    }

    pub fn add_static_local(&mut self, id: u32, loc: CodeLoc, init: TCExpr) -> Result<(), Error> {
        let (decl_idx, symbols) = match &mut self.kind {
            TypeEnvKind::Local {
                decl_idx, symbols, ..
            } => (decl_idx, symbols),
            TypeEnvKind::LocalSwitch {
                decl_idx, symbols, ..
            } => (decl_idx, symbols),
            _ => unreachable!(),
        };

        let tc_var = TCVar {
            var_offset: OffsetOrLoc::StaticLoc(loc),
            ty: init.ty,
            loc,
        };

        if let Some(prev) = symbols.insert(id, tc_var) {
            return Err(error!(
                "variable already exists in current scope",
                prev.loc, "previous declared here", loc, "new variable of same name declared here"
            ));
        }

        let (global_env, ident) = (self.globals_mut(), id);
        let global_ident = TCIdent::ScopedIdent { scope: loc, ident };
        let global_var = TCGlobalVar {
            init: TCGlobalInit::Static(init.kind),
            ty: init.ty,
            loc,
            var_idx: global_env.next_var,
        };
        global_env.symbols.insert(global_ident, global_var);
        global_env.tu.variables.insert(global_ident, global_var);
        global_env.next_var += 1;

        return Ok(());
    }

    pub fn add_global(&mut self, id: u32, loc: CodeLoc, init: TCExpr) -> Result<(), Error> {
        let global_env = match &mut self.kind {
            TypeEnvKind::Global(g) => g,
            _ => unreachable!(),
        };

        let global_ident = TCIdent::Ident(id);
        let global_var = TCGlobalVar {
            init: TCGlobalInit::Default(init.kind),
            ty: init.ty,
            loc,
            var_idx: global_env.next_var,
        };

        if let Some(prev) = global_env.symbols.insert(global_ident, global_var) {
            return Err(error!(
                "variable already exists in current scope",
                prev.loc, "previous declared here", loc, "new variable of same name declared here"
            ));
        }

        global_env.tu.variables.insert(global_ident, global_var);
        global_env.next_var += 1;

        if let Some(func_type) = init.ty.to_tc_func_type(&*global_env) {
            let tc_function = TCFunction {
                is_static: false,
                func_type,
                defn: None,
            };

            global_env.functions.insert(id, tc_function);
            global_env.tu.functions.insert(id, tc_function);
        }

        return Ok(());
    }

    pub fn add_static_global(&mut self, id: u32, loc: CodeLoc, init: TCExpr) -> Result<(), Error> {
        let global_env = match &mut self.kind {
            TypeEnvKind::Global(g) => g,
            _ => unreachable!(),
        };

        let global_ident = TCIdent::Ident(id);
        let global_var = TCGlobalVar {
            init: TCGlobalInit::Static(init.kind),
            ty: init.ty,
            loc,
            var_idx: global_env.next_var,
        };

        if let Some(prev) = global_env.symbols.insert(global_ident, global_var) {
            return Err(error!(
                "variable already exists in current scope",
                prev.loc, "previous declared here", loc, "new variable of same name declared here"
            ));
        }

        global_env.tu.variables.insert(global_ident, global_var);
        global_env.next_var += 1;

        if let Some(func_type) = init.ty.to_tc_func_type(&*global_env) {
            let tc_function = TCFunction {
                is_static: false,
                func_type,
                defn: None,
            };

            global_env.functions.insert(id, tc_function);
            global_env.tu.functions.insert(id, tc_function);
        }

        return Ok(());
    }

    pub fn add_extern_global(&mut self, ty: TCType, id: u32, loc: CodeLoc) -> Result<(), Error> {
        let global_env = match &mut self.kind {
            TypeEnvKind::Global(g) => g,
            _ => unreachable!(),
        };

        let global_ident = TCIdent::Ident(id);
        let global_var = TCGlobalVar {
            init: TCGlobalInit::Extern,
            ty,
            loc,
            var_idx: global_env.next_var,
        };

        if let Some(prev) = global_env.symbols.insert(global_ident, global_var) {
            return Err(error!(
                "variable already exists in current scope",
                prev.loc, "previous declared here", loc, "new variable of same name declared here"
            ));
        }

        global_env.tu.variables.insert(global_ident, global_var);
        global_env.next_var += 1;
        return Ok(());
    }

    pub fn ident(&self, ident: u32, loc: CodeLoc) -> Result<TCExpr, Error> {
        debug_assert!(!self.is_global());

        // search locals
        if let Some(tc_var) = self.search_local_scopes(|sel| sel.symbols.get(&ident).map(|a| *a)) {
            match tc_var.var_offset {
                OffsetOrLoc::LocalOffset(var_offset) => {
                    return Ok(TCExpr {
                        kind: TCExprKind::LocalIdent { var_offset },
                        ty: tc_var.ty,
                        loc,
                    });
                }
                OffsetOrLoc::StaticLoc(scope) => {
                    let (global_env, _) = self.globals();
                    let global_var = global_env.symbols[&TCIdent::ScopedIdent { scope, ident }];
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
        if let Some(global_var) = global_env.symbols.get(&TCIdent::Ident(ident)) {
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
            match tc_var.var_offset {
                OffsetOrLoc::LocalOffset(var_offset) => {
                    return Ok(TCAssignTarget {
                        kind: TCAssignTargetKind::LocalIdent { var_offset },
                        defn_loc: tc_var.loc,
                        ty: tc_var.ty,
                        loc,
                        offset: 0,
                    });
                }
                OffsetOrLoc::StaticLoc(scope) => {
                    let (global_env, _) = self.globals();
                    let global_var = global_env.symbols[&TCIdent::ScopedIdent { scope, ident }];
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
        if let Some(tc_var) = global_env.symbols.get(&TCIdent::Ident(ident)) {
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
}
