use crate::buckets::*;
use crate::filedb::*;
use crate::new_tc_ast::*;
use crate::util::*;

pub struct FuncEnv {
    pub ops: Vec<TCOpcode>,
    pub gotos: Vec<u32>,
    pub next_label: u32,
    pub next_symbol_label: u32,

    // const fields
    pub return_type: TCType,
    pub rtype_loc: CodeLoc,
}

pub enum TypeEnvKind<'a> {
    Global(GlobalTypeEnv<'a>),
    Local {
        symbols: HashMap<u32, TCVar>,
        scope_idx: u32,
        parent: *const TypeEnv<'a>,
        global: *const TypeEnv<'a>,
    },
    LocalSwitch {
        symbols: HashMap<u32, TCVar>,
        scope_idx: u32,
        cases: Vec<(TCExpr, u32)>,
        parent: *const TypeEnv<'a>,
        global: *const TypeEnv<'a>,
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
    pub cases: Option<&'a Vec<(TCExpr, u32)>>,
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
            next_symbol_label: 0,
            return_type,
            rtype_loc,
        };
    }
}

impl<'a> Drop for TypeEnv<'a> {
    fn drop(&mut self) {
        let scope_idx = match self.kind {
            TypeEnvKind::Local { scope_idx, .. } => scope_idx,
            TypeEnvKind::LocalSwitch { scope_idx, .. } => scope_idx,
            TypeEnvKind::Global { .. } => return,
        };

        if scope_idx != !0 {
            panic!("didn't close scope");
        }
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

    pub fn tu(mut self) -> TranslationUnit {
        return match &mut self.kind {
            TypeEnvKind::Global(GlobalTypeEnv { tu, .. }) => {
                std::mem::replace(tu, TranslationUnit::new())
            }
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

    pub fn new_func(&mut self, env: &mut FuncEnv, loc: CodeLoc) -> Self {
        debug_assert!(self.is_global());

        let (_, global) = self.globals();

        let kind = TypeEnvKind::Local {
            symbols: HashMap::new(),
            scope_idx: 0,
            parent: self,
            global,
        };

        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::ScopeBegin(HashRef::empty(), !0),
            loc,
        });

        Self {
            kind,
            typedefs: HashMap::new(),
            builtins_enabled: false,
        }
    }

    pub fn child(&mut self, env: &mut FuncEnv, loc: CodeLoc) -> Self {
        let (_, global) = self.globals();

        let kind = TypeEnvKind::Local {
            symbols: HashMap::new(),
            scope_idx: env.ops.len() as u32,
            parent: self,
            global,
        };

        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::ScopeBegin(HashRef::empty(), !0),
            loc,
        });

        Self {
            kind,
            typedefs: HashMap::new(),
            builtins_enabled: false,
        }
    }

    pub fn switch(&mut self, env: &mut FuncEnv, loc: CodeLoc) -> Self {
        let (_, global) = self.globals();

        let kind = TypeEnvKind::LocalSwitch {
            symbols: HashMap::new(),
            scope_idx: env.ops.len() as u32,
            cases: Vec::new(),
            parent: self,
            global,
        };

        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::ScopeBegin(HashRef::empty(), !0),
            loc,
        });

        Self {
            kind,
            typedefs: HashMap::new(),
            builtins_enabled: false,
        }
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
            LabelOrLoc::Param(id) => Some((id, v.ty)),
            LabelOrLoc::LocalIdent(id) => Some((id, v.ty)),
            LabelOrLoc::StaticLoc(_) => None,
        });
        let symbols = HashRef::new_iter(&self, capa, symbols);
        env.ops[scope_idx as usize].kind = TCOpcodeKind::ScopeBegin(symbols, scope_end);
        env.ops.push(TCOpcode {
            kind: TCOpcodeKind::ScopeEnd(scope_idx),
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
                builtins_enabled: sel.builtins_enabled,
            }),
            TypeEnvKind::Local { symbols, .. } => f(LocalTypeEnv {
                symbols,
                cases: None,
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

    pub fn add_param(&mut self, env: &mut FuncEnv, param: &TCParamDecl) -> Result<(), Error> {
        let symbols = match &mut self.kind {
            TypeEnvKind::Local { symbols, .. } => symbols,
            _ => unreachable!(),
        };

        let TCParamDecl { ty, ident, loc } = *param;
        let symbol_label = LabelOrLoc::Param(env.next_symbol_label);
        env.next_symbol_label += 1;

        let tc_var = TCVar {
            symbol_label,
            ty,
            loc,
        };

        if let Some(prev) = symbols.insert(ident, tc_var) {
            return Err(error!(
                "variable already exists in current scope",
                prev.loc, "previous declared here", loc, "new variable of same name declared here"
            ));
        }

        return Ok(());
    }

    pub fn add_local(&mut self, env: &mut FuncEnv, decl: &TCDecl) -> Result<(), Error> {
        let TCDecl {
            init,
            ident,
            ty,
            loc,
        } = *decl;

        let symbol_label = match init {
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

                LabelOrLoc::StaticLoc(loc)
            }
            TCDeclInit::DefaultUninit | TCDeclInit::Default(_) => {
                let idx = env.next_symbol_label;
                env.next_symbol_label += 1;

                LabelOrLoc::LocalIdent(idx)
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
            match tc_var.symbol_label {
                LabelOrLoc::Param(label) => {
                    return Ok(TCExpr {
                        kind: TCExprKind::LocalIdent { label },
                        ty: tc_var.ty,
                        loc,
                    });
                }
                LabelOrLoc::LocalIdent(label) => {
                    return Ok(TCExpr {
                        kind: TCExprKind::LocalIdent { label },
                        ty: tc_var.ty,
                        loc,
                    });
                }
                LabelOrLoc::StaticLoc(scope) => {
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
                LabelOrLoc::Param(label) => {
                    return Ok(TCAssignTarget {
                        kind: TCAssignTargetKind::LocalIdent { label },
                        defn_loc: tc_var.loc,
                        ty: tc_var.ty,
                        loc,
                        offset: 0,
                    });
                }
                LabelOrLoc::LocalIdent(label) => {
                    return Ok(TCAssignTarget {
                        kind: TCAssignTargetKind::LocalIdent { label },
                        defn_loc: tc_var.loc,
                        ty: tc_var.ty,
                        loc,
                        offset: 0,
                    });
                }
                LabelOrLoc::StaticLoc(scope) => {
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
