use crate::buckets::*;
use crate::new_ast::*;
use crate::new_tc_ast::*;
use crate::new_tc_structs::*;
use crate::util::*;

#[derive(Hash, PartialEq, Eq)]
pub struct TypeDeclSpec {
    unsigned: u8,
    long: u8,
    short: u8,
    char: u8,
    int: u8,
    float: u8,
    signed: u8,
    double: u8,
    void: u8,
}

impl TypeDeclSpec {
    pub fn new() -> Self {
        TypeDeclSpec {
            unsigned: 0,
            long: 0,
            short: 0,
            char: 0,
            int: 0,
            float: 0,
            signed: 0,
            double: 0,
            void: 0,
        }
    }
}

macro_rules! gen_type_decl_spec {
    ($map:expr, $ast:ident, $( $ident:ident )* ) => {{
        let mut decl = TypeDeclSpec::new();

        $(
            gen_type_decl_spec!(@INCR_CORRECT, decl, $ident);
        )*

        $map.insert(decl, TCTypeBase::$ast);
    }};
    (@INCR_CORRECT, $decl:expr, void) => {
        $decl.void += 1;
    };
    (@INCR_CORRECT, $decl:expr, unsigned) => {
        $decl.unsigned += 1;
    };
    (@INCR_CORRECT, $decl:expr, long) => {
        $decl.long += 1;
    };
    (@INCR_CORRECT, $decl:expr, short) => {
        $decl.short += 1;
    };
    (@INCR_CORRECT, $decl:expr, char) => {
        $decl.char += 1;
    };
    (@INCR_CORRECT, $decl:expr, int) => {
        $decl.int += 1;
    };
    (@INCR_CORRECT, $decl:expr, float) => {
        $decl.float += 1;
    };
    (@INCR_CORRECT, $decl:expr, signed) => {
        $decl.signed += 1;
    };
    (@INCR_CORRECT, $decl:expr, double) => {
        $decl.double += 1;
    };
    (@INCR_CORRECT, $decl:expr, ident) => {
        $decl.ident += 1;
    };
}

lazy_static! {
    pub static ref CORRECT_TYPES: HashMap<TypeDeclSpec, TCTypeBase> = {
        let mut map: HashMap<TypeDeclSpec, TCTypeBase> = HashMap::new();

        gen_type_decl_spec!(map, I32, int);
        gen_type_decl_spec!(map, I64, long);
        gen_type_decl_spec!(map, I8, char);
        gen_type_decl_spec!(map, Void, void);
        gen_type_decl_spec!(map, I8, signed char);
        gen_type_decl_spec!(map, U32, unsigned);
        gen_type_decl_spec!(map, U8,unsigned char);

        gen_type_decl_spec!(map, I64,long int);
        gen_type_decl_spec!(map, I64,long long int);
        gen_type_decl_spec!(map, I64,long long);

        gen_type_decl_spec!(map, U32, unsigned int);

        gen_type_decl_spec!(map, U64, unsigned long);
        gen_type_decl_spec!(map, U64, unsigned long int);
        gen_type_decl_spec!(map, U64, unsigned long long);
        gen_type_decl_spec!(map, U64, unsigned long long int);

        map
    };
}

pub fn parse_decl_specs(
    locals: &TypeEnv,
    decl_specs: &[DeclarationSpecifier],
) -> Result<(StorageClass, TCTypeBase), Error> {
    let mut sc = StorageClass::Default;
    let mut ds = TypeDeclSpec::new();

    for decl_spec in decl_specs {
        // use crate::new_ast::TypeQualifier as TyQual;
        use crate::new_ast::TypeSpecifier as TySpec;
        use DeclarationSpecifierKind::*;
        match decl_spec.kind {
            Extern => {
                sc = StorageClass::Extern;
            }
            Static => {
                sc = StorageClass::Static;
            }
            Typedef => {
                sc = StorageClass::Typedef;
            }

            TypeQualifier(qual) => {}
            Inline => {}
            Noreturn => {}

            TypeSpecifier(TySpec::Ident(id)) => {
                let ty = locals.check_typedef(id, decl_spec.loc)?;
                return Ok((sc, ty));
            }
            TypeSpecifier(TySpec::Void) => {
                return Ok((sc, TCTypeBase::Void));
            }

            TypeSpecifier(TySpec::Char) => {
                ds.char = ds.char.saturating_add(1);
            }
            TypeSpecifier(TySpec::Short) => {
                ds.short = ds.short.saturating_add(1);
            }
            TypeSpecifier(TySpec::Int) => {
                ds.int = ds.int.saturating_add(1);
            }
            TypeSpecifier(TySpec::Long) => {
                ds.long = ds.long.saturating_add(1);
            }
            TypeSpecifier(TySpec::Signed) => {
                ds.signed = ds.signed.saturating_add(1);
            }
            TypeSpecifier(TySpec::Unsigned) => {
                ds.unsigned = ds.unsigned.saturating_add(1);
            }
            TypeSpecifier(TySpec::Float) => {
                ds.float = ds.float.saturating_add(1);
            }
            TypeSpecifier(TySpec::Double) => {
                ds.double = ds.double.saturating_add(1);
            }

            _ => unreachable!(),
        }
    }

    let begin = decl_specs[0].loc;
    let end = decl_specs.last().unwrap().loc;

    let or_else = || error!("incorrect ");
    let base = *CORRECT_TYPES.get(&ds).ok_or_else(or_else)?;

    return Ok((sc, base));
}

pub fn check_declarator(
    locals: &TypeEnv,
    buckets: BucketListRef<'static>,
    decl_specs: &[DeclarationSpecifier],
    decl: &Declarator,
) -> Result<(StorageClass, TCTypeOwned, n32), Error> {
    let (sc, mut tc_type, ident) = match decl.kind {
        DeclaratorKind::Declarator(decl) => check_declarator(locals, buckets, decl_specs, decl)?,
        DeclaratorKind::Identifier(ident) => {
            let (sc, base) = parse_decl_specs(locals, decl_specs)?;

            (sc, TCTypeOwned::new(base), ident.into())
        }
        DeclaratorKind::Abstract => {
            let (sc, base) = parse_decl_specs(locals, decl_specs)?;

            (sc, TCTypeOwned::new(base), n32::NULL)
        }
    };

    use DerivedDeclaratorKind as DDK;
    for derived in decl.derived {
        match derived.kind {
            DDK::Array(array_decl) => {
                if array_decl.qualifiers.len() != 0 {
                    return Err(error!(
                        "TCI doesn't handle array qualifiers right now",
                        array_decl.loc, "qualfiiers found around here"
                    ));
                }

                match array_decl.size.kind {
                    ArraySizeKind::Unknown => {
                        tc_type.mods.push(TCTypeModifier::VariableArray);
                    }
                    ArraySizeKind::VariableExpression(expr) => {
                        let expr = eval_expr(expr)?;
                        let (expr, loc) = if let ExprKind::IntLiteral(i) = expr.kind {
                            (i, expr.loc)
                        } else {
                            unreachable!()
                        };

                        if expr < 0 {
                            return Err(error!(
                                "array must have positive size",
                                loc, "size found here"
                            ));
                        }

                        tc_type.mods.push(TCTypeModifier::Array(expr as u32));
                    }
                }
            }
            DDK::Function(func) => {
                let param = func.parameters[0];
                let (sc, param_type) = if let Some(decl) = param.declarator {
                    let (sc, tc_type, _) =
                        check_declarator(locals, buckets, param.specifiers, &decl)?;
                    (sc, tc_type.to_ref(buckets))
                } else {
                    let (sc, base) = parse_decl_specs(locals, param.specifiers)?;
                    let tc_type = TCType { base, mods: &[] };

                    (sc, tc_type)
                };

                debug_assert!(let_expr!(StorageClass::Default = sc));

                if param_type.is_void() {
                    if func.parameters.len() == 1 {
                        tc_type.mods.push(TCTypeModifier::NoParams);
                        continue;
                    } else {
                        return Err(error!(
                            "cannot have a parameter of type void",
                            param.loc, "parameter found here"
                        ));
                    }
                }

                tc_type.mods.push(TCTypeModifier::BeginParam(param_type));

                for param in &func.parameters[1..] {
                    let (sc, param_type) = if let Some(decl) = param.declarator {
                        let (sc, tc_type, _) =
                            check_declarator(locals, buckets, param.specifiers, &decl)?;
                        (sc, tc_type.to_ref(buckets))
                    } else {
                        let (sc, base) = parse_decl_specs(locals, param.specifiers)?;
                        let tc_type = TCType { base, mods: &[] };

                        (sc, tc_type)
                    };

                    debug_assert!(let_expr!(StorageClass::Default = sc));

                    if let TCTypeBase::Void = param_type.base {
                        if param_type.mods.len() == 0 {
                            return Err(error!(
                                "cannot have a parameter of type void",
                                param.loc, "parameter found here"
                            ));
                        }
                    }

                    tc_type.mods.push(TCTypeModifier::Param(param_type));
                }

                if func.varargs {
                    tc_type.mods.push(TCTypeModifier::VarargsParam);
                }
            }
            DDK::EmptyFunction => {
                tc_type.mods.push(TCTypeModifier::UnknownParams);
            }
            DDK::Pointer(ptr_qual) => {
                tc_type.mods.push(TCTypeModifier::Pointer);
            }
        }
    }

    return Ok((sc, tc_type, ident));
}

pub fn eval_expr(expr: &Expr) -> Result<&Expr, Error> {
    // TODO cmon man
    if let ExprKind::IntLiteral(i) = expr.kind {
        return Ok(expr);
    } else {
        return Err(error!(
            "cannot evaluate constant expression",
            expr.loc, "expression found here"
        ));
    }
}

pub fn check_tree(tree: &[GlobalStatement]) -> Result<TranslationUnit, Error> {
    let mut globals = TypeEnv::global();

    for decl in tree {
        match decl.kind {
            GlobalStatementKind::Declaration(decl) => {
                check_declaration(&globals, decl)?;
            }
            GlobalStatementKind::FunctionDefinition(func) => {
                check_function_defn(&globals, func)?;
            }
            GlobalStatementKind::Pragma(pragma) => {
                if pragma == "tci enable_builtins" {
                    globals.builtins_enabled = true;
                }
            }
        }
    }

    return Ok(globals.tu());
}

pub fn check_declaration(globals: &TypeEnv, declaration: Declaration) -> Result<(), Error> {
    return Ok(());
}

pub fn check_function_defn(globals: &TypeEnv, defn: FunctionDefinition) -> Result<(), Error> {
    return Ok(());
}
