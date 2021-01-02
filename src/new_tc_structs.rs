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

    pub fn is_global(&self) -> bool {
        match self.kind {
            TypeEnvKind::Global { .. } => true,
            TypeEnvKind::Local { global, .. } => false,
            TypeEnvKind::LocalSwitch { global, .. } => false,
        }
    }

    pub fn new_func(&self) -> Self {
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

    pub fn child(&self) -> Self {
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

    pub fn switch(&self) -> Self {
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
            decl_type: ty,
            loc,
        };

        symbols.insert(id, tc_var);

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
            decl_type: ty,
            loc,
        };

        symbols.insert(id, tc_var);

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
                        ty: tc_var.decl_type,
                        loc,
                    });
                }
                OffsetOrLoc::StaticLoc(scope) => {
                    let (global_env, _) = self.globals();
                    let global_var = global_env.symbols[&TCIdent::ScopedIdent { scope, ident }];
                    let binary_offset = global_var.var_idx;

                    return Ok(TCExpr {
                        kind: TCExprKind::GlobalIdent { binary_offset },
                        ty: tc_var.decl_type,
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
                ty: global_var.decl_type,
                loc,
            });
        }

        // search functions
        if let Some(func) = global_env.functions.get(&ident) {
            return Ok(TCExpr {
                kind: TCExprKind::FunctionIdent { ident },
                ty: func.expr_type,
                loc,
            });
        }

        return Err(error!("couldn't find symbol", loc, "symbol used here"));
    }
}
