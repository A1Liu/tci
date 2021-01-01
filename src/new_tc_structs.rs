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
    Global {
        symbols: HashMap<u32, TCGlobalVar>,
        functions: HashMap<u32, TCFunction>,
        tu: TranslationUnit,
        next_var: u32,
    },
    Local {
        symbols: HashMap<u32, TCVar>,
        parent: *const TypeEnv,
        global: *const TypeEnv,
    },
    LocalSwitch {
        symbols: HashMap<u32, TCVar>,
        cases: HashMap<TCExpr, u32>,
        parent: *const TypeEnv,
        global: *const TypeEnv,
    },
}

pub struct TypeEnv {
    pub kind: TypeEnvKind,
    pub typedefs: HashMap<u32, (&'static TCType, CodeLoc)>,
    pub builtins_enabled: bool,
}

impl TypeEnv {
    pub fn global() -> Self {
        Self {
            kind: TypeEnvKind::Global {
                symbols: HashMap::new(),
                functions: HashMap::new(),
                tu: TranslationUnit::new(),
                next_var: 0,
            },
            typedefs: HashMap::new(),
            builtins_enabled: false,
        }
    }

    pub fn tu(self) -> TranslationUnit {
        return match self.kind {
            TypeEnvKind::Global { tu, .. } => tu,
            _ => unreachable!(),
        };
    }

    pub fn globals(&self) -> &TypeEnv {
        let global: *const TypeEnv = match self.kind {
            TypeEnvKind::Global { .. } => self,
            TypeEnvKind::Local { global, .. } => global,
            TypeEnvKind::LocalSwitch { global, .. } => global,
        };

        return unsafe { &*global };
    }

    pub fn is_global(&self) -> bool {
        match self.kind {
            TypeEnvKind::Global { .. } => true,
            TypeEnvKind::Local { global, .. } => false,
            TypeEnvKind::LocalSwitch { global, .. } => false,
        }
    }

    pub fn child(&self) -> Self {
        let global = self.globals();

        let kind = TypeEnvKind::Local {
            symbols: HashMap::new(),
            parent: self,
            global,
        };

        Self {
            kind,
            typedefs: HashMap::new(),
            builtins_enabled: false,
        }
    }

    pub fn switch(&self) -> Self {
        let global = self.globals();

        let kind = TypeEnvKind::LocalSwitch {
            symbols: HashMap::new(),
            cases: HashMap::new(),
            parent: self,
            global,
        };

        Self {
            kind,
            typedefs: HashMap::new(),
            builtins_enabled: false,
        }
    }

    pub fn check_typedef(&self, ident: u32, loc: CodeLoc) -> Result<TCTypeBase, Error> {
        let mut c_env: *const TypeEnv = self;

        loop {
            let current = unsafe { &*c_env };
            if let Some((ty, loc)) = self.typedefs.get(&ident) {
                return Ok(TCTypeBase::Typedef {
                    refers_to: ty,
                    typedef: (ident.into(), *loc),
                });
            }

            c_env = match current.kind {
                TypeEnvKind::Global { .. } => break,
                TypeEnvKind::Local { parent, .. } => parent,
                TypeEnvKind::LocalSwitch { parent, .. } => parent,
            };
        }

        return Err(error!(
            "couldn't find typedef",
            loc, "typedef referenced here"
        ));
    }
}
