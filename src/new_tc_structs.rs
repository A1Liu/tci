use crate::buckets::*;
use crate::filedb::*;
use crate::new_tc_ast::*;
use crate::util::*;

pub struct FuncEnv {
    pub ops: Vec<TCOpcode>,
    pub gotos: Vec<u32>,
    pub next_label: u32,

    // const fields
    pub return_type: TCType,
    pub rtype_loc: CodeLoc,
}

pub enum TypeEnvKind<'a> {
    Global(GlobalTypeEnv<'a>),
    Local {
        symbols: HashMap<u32, TCVar>,
        parent: *const TypeEnv<'a>,
        global: *const TypeEnv<'a>,
        decl_idx: i16,
    },
    LocalSwitch {
        symbols: HashMap<u32, TCVar>,
        cases: HashMap<TCExpr, u32>,
        parent: *const TypeEnv<'a>,
        global: *const TypeEnv<'a>,
        decl_idx: i16,
    },
}

pub struct TypeEnv<'a> {
    pub kind: TypeEnvKind<'a>,
    pub typedefs: HashMap<u32, (&'static TCType, CodeLoc)>,
    pub builtins_enabled: bool,
}

pub struct GlobalTypeEnv<'a> {
    symbols: HashMap<TCIdent, TCGlobalVar>,
    functions: HashMap<u32, TCFunction>,
    tu: TranslationUnit,
    files: &'a FileDb,
    next_var: u32,
}

pub struct LocalTypeEnv<'a> {
    pub symbols: &'a HashMap<u32, TCVar>,
    pub cases: Option<&'a HashMap<TCExpr, u32>>,
    pub decl_idx: &'a i16,
    pub typedefs: &'a HashMap<u32, (&'static TCType, CodeLoc)>,
    pub builtins_enabled: bool,
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
    pub fn new(return_type: TCType, rtype_loc: CodeLoc) -> Self {
        return FuncEnv {
            ops: Vec::new(),
            gotos: Vec::new(),
            next_label: 0,
            return_type,
            rtype_loc,
        };
    }
}

impl<'a> TypeEnv<'a> {
    pub fn global(files: &'a FileDb) -> Self {
        Self {
            kind: TypeEnvKind::Global(GlobalTypeEnv {
                symbols: HashMap::new(),
                functions: HashMap::new(),
                tu: TranslationUnit::new(),
                files,
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

    pub fn new_func(&mut self) -> Self {
        debug_assert!(self.is_global());

        let (_, global) = self.globals();

        let kind = TypeEnvKind::Local {
            symbols: HashMap::new(),
            parent: self,
            global,
            decl_idx: -2,
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
        F: for<'b> Fn(LocalTypeEnv<'b>) -> Option<T>,
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

    pub fn add_local(&mut self, decl: &TCDecl) -> Result<(), Error> {
        let TCDecl {
            init,
            ident,
            ty,
            loc,
        } = *decl;

        let var_offset = match init {
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

                global_env.symbols.insert(global_ident, global_var);
                global_env.tu.variables.insert(global_ident, global_var);
                global_env.next_var += 1;

                OffsetOrLoc::StaticLoc(loc)
            }
            TCDeclInit::DefaultUninit | TCDeclInit::Default(_) => {
                let decl_idx = match &mut self.kind {
                    TypeEnvKind::Local { decl_idx, .. } => decl_idx,
                    TypeEnvKind::LocalSwitch { decl_idx, .. } => decl_idx,
                    _ => unreachable!(),
                };

                let idx = if *decl_idx < 0 {
                    *decl_idx = 1;
                    0
                } else {
                    *decl_idx += 1;
                    *decl_idx - 1
                };

                OffsetOrLoc::LocalOffset(idx)
            }
        };

        let tc_var = TCVar {
            var_offset,
            ty,
            loc,
        };

        let symbols = match &mut self.kind {
            TypeEnvKind::Local { symbols, .. } => symbols,
            TypeEnvKind::LocalSwitch { symbols, .. } => symbols,
            _ => unreachable!(),
        };

        if let Some(prev) = symbols.insert(ident, tc_var) {
            return Err(error!(
                "variable already exists in current scope",
                prev.loc, "previous declared here", loc, "new variable of same name declared here"
            ));
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

        if let Some(prev) = global_env.symbols.insert(global_ident, global_var) {
            return Err(error!(
                "variable already exists in current scope",
                prev.loc,
                "previous declared here",
                decl.loc,
                "new variable of same name declared here"
            ));
        }

        global_env.tu.variables.insert(global_ident, global_var);
        global_env.next_var += 1;

        if let Some(func_type) = decl.ty.to_func_type_strict(&*global_env) {
            let tc_function = TCFunction {
                is_static: false,
                func_type,
                defn: None,
            };

            global_env.functions.insert(decl.ident, tc_function);
            global_env.tu.functions.insert(decl.ident, tc_function);
        }

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
            if tc_var.ty.is_function() {
                return Err(error!(
                    "can't assign to function type",
                    loc, "assignment to name happens here"
                ));
            }

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

    pub fn add_typedef(&mut self, ty: TCType, id: u32, loc: CodeLoc) {
        self.typedefs.insert(id, (self.add(ty), loc));
    }
}
