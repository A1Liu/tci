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

pub fn check_func_defn_decl(
    locals: &TypeEnv,
    buckets: &impl Allocator<'static>,
    decl: &FunctionDefinition,
) -> Result<TCFunctionDeclarator, Error> {
    let (sc, base) = parse_decl_specs(locals, decl.specifiers)?;
    let mut rtype = TCTypeOwned::new(base);

    for modifier in decl.pointer {
        // TODO warn when there are qualifiers
        rtype.mods.push(TCTypeModifier::Pointer);
    }

    let params_decl = if let Some(params) = decl.params {
        params
    } else {
        return Ok(TCFunctionDeclarator {
            sc,
            return_type: rtype.to_ref(&buckets),
            ident: decl.ident,
            params: None,
        });
    };

    let params = check_param_types(locals, buckets, params_decl.parameters)?;
    let mut out = Vec::new();
    for (idx, (ty, id)) in params.into_iter().enumerate() {
        if id == n32::NULL {
            return Err(error!(
                "missing identifier",
                params_decl.parameters[idx].loc, "parameter found here"
            ));
        } else {
            out.push(TCParamDeclaration {
                ty,
                ident: id.into(),
                loc: params_decl.parameters[idx].loc,
            });
        }
    }

    return Ok(TCFunctionDeclarator {
        sc,
        return_type: rtype.to_ref(&buckets),
        ident: decl.ident,
        params: Some(TCParamsDeclarator {
            params: buckets.add_array(out),
            varargs: params_decl.varargs,
        }),
    });
}

pub fn check_decl(
    locals: &TypeEnv,
    buckets: &impl Allocator<'static>,
    decl_specs: &[DeclarationSpecifier],
    decl: &Declarator,
) -> Result<(StorageClass, TCTypeOwned, n32), Error> {
    let (sc, ty, id) = check_decl_rec(locals, buckets, decl_specs, decl)?;

    let mut was_array = false;
    let mut was_function = false;
    for modifier in &ty.mods {
        match modifier {
            TCTypeModifier::Array(_) | TCTypeModifier::VariableArray => {
                if was_function {
                    return Err(error!(
                        "function type returns array (functions can't return arrays in C)",
                        decl.loc, "type declaration found here"
                    ));
                }

                was_array = true;
                was_function = false;
            }
            TCTypeModifier::BeginParam(_)
            | TCTypeModifier::UnknownParams
            | TCTypeModifier::NoParams => {
                if was_array {
                    return Err(error!(
                        "type is an array of functions (functions don't have a constant size\
                            so you can't have an array of them)",
                        decl.loc, "type declaration found here"
                    ));
                }

                was_function = true;
                was_array = false;
            }
            TCTypeModifier::VarargsParam | TCTypeModifier::Param(_) => {}
            TCTypeModifier::Pointer => {
                was_array = false;
                was_function = false;
            }
        }
    }

    if was_array && ty.is_void() {
        return Err(error!(
            "can't have an array of void",
            decl.loc, "this is an array type"
        ));
    }

    return Ok((sc, ty, id));
}

pub fn check_param_types(
    locals: &TypeEnv,
    buckets: &impl Allocator<'static>,
    params: &[ParameterDeclaration],
) -> Result<Vec<(TCType, n32)>, Error> {
    let param = params[0];
    let (sc, mut param_type, id) = if let Some(decl) = param.declarator {
        let (sc, tc_type, id) = check_decl(locals, buckets, param.specifiers, &decl)?;
        (sc, tc_type, id)
    } else {
        let (sc, base) = parse_decl_specs(locals, param.specifiers)?;

        (sc, TCTypeOwned::new(base), n32::NULL)
    };

    debug_assert!(let_expr!(StorageClass::Default = sc));

    if param_type.is_void() {
        if params.len() == 1 && id == n32::NULL {
            return Ok(Vec::new());
        } else {
            return Err(error!(
                "cannot have a parameter of type void",
                param.loc, "parameter found here"
            ));
        }
    }

    param_type.canonicalize_param();
    let param_type = param_type.to_ref(buckets);

    let mut out = Vec::new();
    out.push((param_type, id));

    for param in &params[1..] {
        let (sc, param_type, id) = if let Some(decl) = param.declarator {
            let (sc, tc_type, id) = check_decl(locals, buckets, param.specifiers, &decl)?;
            (sc, tc_type.to_ref(buckets), id)
        } else {
            let (sc, base) = parse_decl_specs(locals, param.specifiers)?;
            let tc_type = TCType { base, mods: &[] };

            (sc, tc_type, n32::NULL)
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

        out.push((param_type, id));
    }

    return Ok(out);
}

pub fn check_decl_rec(
    locals: &TypeEnv,
    buckets: &impl Allocator<'static>,
    decl_specs: &[DeclarationSpecifier],
    decl: &Declarator,
) -> Result<(StorageClass, TCTypeOwned, n32), Error> {
    let (sc, mut tc_type, ident) = match decl.kind {
        DeclaratorKind::Declarator(decl) => check_decl_rec(locals, buckets, decl_specs, decl)?,
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
                let params = check_param_types(locals, buckets, func.parameters)?;
                if params.len() == 0 {
                    tc_type.mods.push(TCTypeModifier::NoParams);
                    continue;
                }

                tc_type.mods.push(TCTypeModifier::BeginParam(params[0].0));
                for param in &params[1..] {
                    tc_type.mods.push(TCTypeModifier::Param(param.0));
                }

                if func.varargs {
                    tc_type.mods.push(TCTypeModifier::VarargsParam);
                }
            }
            DDK::EmptyFunction => {
                tc_type.mods.push(TCTypeModifier::UnknownParams);
            }
            DDK::Pointer(ptr_qual) => {
                // TODO warn when there are qualifiers
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
                // check_function_defn(&globals, func)?;
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

pub fn check_declaration(locals: &TypeEnv, declaration: Declaration) -> Result<(), Error> {
    return Ok(());
}
