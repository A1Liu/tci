use crate::buckets::*;
use crate::lexer::*;
use crate::new_ast::*;
use crate::util::*;
use std::cell::RefCell;
use std::collections::HashMap;

pub struct ParseEnv {
    pub symbol_is_type: RefCell<Vec<HashMap<u32, bool>>>, // true is type
    pub locs: Vec<CodeLoc>,
    pub buckets_begin: BucketListRef<'static>,
    pub buckets: BucketListRef<'static>,
    pub tree: Vec<GlobalStatement>,
}

impl ParseEnv {
    pub fn new(locs: &[Token]) -> Self {
        let buckets = BucketList::new();

        Self {
            // TODO This is a hack to work around stuff in rust-peg
            symbol_is_type: RefCell::new(vec![HashMap::new()]),
            locs: locs.iter().map(|tok| tok.loc).collect(),
            buckets_begin: buckets,
            tree: Vec::new(),
            buckets,
        }
    }

    pub fn enter_scope(&self) {
        self.symbol_is_type.borrow_mut().push(HashMap::new());
    }

    pub fn leave_scope(&self) {
        self.symbol_is_type.borrow_mut().pop().unwrap();
    }

    pub fn is_typename(&self, ident: u32) -> bool {
        for scope in self.symbol_is_type.borrow().iter().rev() {
            if let Some(symbol) = scope.get(&ident) {
                return *symbol;
            }
        }
        false
    }

    pub fn handle_declarator(&self, mut declarator: &Declarator, is_type: bool) {
        loop {
            match declarator.kind {
                DeclaratorKind::Abstract => return,
                DeclaratorKind::Identifier(i) => {
                    self.add_symbol(i, is_type);
                    break;
                }
                DeclaratorKind::Declarator(d) => declarator = d,
            }
        }
    }

    pub fn add_symbol(&self, sym: u32, is_type: bool) {
        let mut raii_borrow = self.symbol_is_type.borrow_mut();
        let scope_o = raii_borrow.last_mut();
        let scope = scope_o.expect("at least one scope should be always present");
        scope.insert(sym, is_type);
    }
}

#[inline]
pub fn concat<E>(mut a: Vec<E>, b: Vec<E>) -> Vec<E> {
    a.extend(b);
    return a;
}

peg::parser! {

// Translated from https://github.com/vickenty/lang-c/blob/master/grammar.rustpeg
pub grammar c_parser(env: &ParseEnv) for [TokenKind<'static>] {

rule list0<E>(x: rule<E>) -> (Vec<E>, CodeLoc) = pos:position!() v:(x()*) pos2:position!() {
    if pos == pos2 {
        (v, NO_FILE)
    } else {
        (v, l_from(env.locs[pos], env.locs[pos2 - 1]))
    }
}

rule list1<E>(x: rule<E>) -> (Vec<E>, CodeLoc) = pos:position!() v:(x()+) pos2:position!() {
    (v, l_from(env.locs[pos], env.locs[pos2 - 1]))
}

rule cs0<E>(x: rule<E>) -> (Vec<E>, CodeLoc) = pos:position!() v:(x() ** [TokenKind::Comma]) pos2:position!() {
    if pos == pos2 {
        (v, NO_FILE)
    } else {
        (v, l_from(env.locs[pos], env.locs[pos2 - 1]))
    }
}

rule cs1<E>(x: rule<E>) -> (Vec<E>, CodeLoc) =
    pos:position!() v:(x() ** [TokenKind::Comma]) [TokenKind::Comma] last:x() pos2:position!() {
    let mut v = v;
    v.push(last);
    (v, l_from(env.locs[pos], env.locs[pos2 - 1]))
}

rule list_010<E>(b: rule<E>, s: rule<E>, a: rule<E>) -> (Vec<E>, CodeLoc) =
    before:list0(<b()>) pos:position!() single:s() after:list0(<a()>)
{
    let (mut before, mut begin_loc) = before;
    let (mut after, mut end_loc) = after;
    let single_loc = env.locs[pos];
    if begin_loc == NO_FILE {
        begin_loc = single_loc;
    }

    if end_loc == NO_FILE {
        end_loc = single_loc;
    }

    let loc = l_from(begin_loc, end_loc);

    let mut before = before;
    before.push(single);
    before.extend(after);
    (before, loc)
}

// A list containing *exactly* one element of a, and any of b.
rule list_eq1_n<E>(a: rule<E>, b: rule<E>) -> (Vec<E>,  CodeLoc) = v:list_010(<b()>,<a()>, <b()>)

// A list containing *at least* one element of a, and any of b.
rule list_ge1_n<E>(a: rule<E>, b: rule<E>) -> (Vec<E>, CodeLoc) = v:list_010(<b()>, <a()>, <a() / b()>)

rule scoped<E>(e: rule<E>) -> E = ({ env.enter_scope(); }) e:e()? {? env.leave_scope(); e.ok_or("") }

rule ident() -> (u32, CodeLoc) = pos:position!() n:$[TokenKind::Ident(_)] {
    match n[0] {
        TokenKind::Ident(n) => (n, env.locs[pos]),
        _ => unreachable!(),
    }
}

rule int() -> (i32, CodeLoc) = pos:position!() n:$[TokenKind::IntLiteral(_)] {
    match n[0] {
        TokenKind::IntLiteral(n) => (n, env.locs[pos]),
        _ => unreachable!(),
    }
}

rule string() -> (&'static str, CodeLoc) = pos:position!() n:$([TokenKind::StringLiteral(_)]+) pos2:position!() {
    let mut string = String::new();
    let loc = l_from(env.locs[pos], env.locs[pos2 - 1]);
    for token in n {
        let s = match token {
            TokenKind::StringLiteral(s) => s,
            _ => unreachable!(),
        };

        string.push_str(s);
    }

    (env.buckets.add_str(&string), loc)
}

rule constant_expr() -> Expr =
    n:int() {
        let (n, loc) = n;

        Expr {
            kind: ExprKind::IntLiteral(n),
            loc,
        }
    } /
    n:string() {
        let (n, loc) = n;

        Expr {
            kind: ExprKind::StringLiteral(n),
            loc,
        }
    }

rule atom() -> Expr =
    constant_expr() /
    n:ident() {
        let (n, loc) = n;

        Expr {
            kind: ExprKind::Ident(n),
            loc,
        }
    } /
    pos:position!() [TokenKind::LParen] v:(expr() ** [TokenKind::Comma]) pos2:position!() [TokenKind::RParen] {
        Expr {
            kind: ExprKind::ParenList(env.buckets.add_array(v)),
            loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    }


rule expr() -> Expr = precedence! {
    x:(@) [TokenKind::Plus] y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Add, x, y) }
    }
    x:(@) [TokenKind::Dash] y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Sub, x, y) }
    }
    --
    n:atom() { n }
}

rule assignment_expression() -> Expr = expr()

pub rule declaration() -> Declaration = d:declaration1() [TokenKind::Semicolon] {
    Declaration {
        loc: d.2,
        specifiers: env.buckets.add_array(d.0),
        declarators: env.buckets.add_array(d.1),
    }
}

rule declaration_seq<E, T>(h: rule<(Vec<E>, CodeLoc)>, t: rule<(Vec<E>, Vec<T>, CodeLoc)>)
    -> (Vec<E>, Vec<T>, CodeLoc) = head:h() tail:t()
{
    (concat(head.0, tail.0), tail.1, l_from(head.1, tail.2))
}

rule declaration1() -> (Vec<DeclarationSpecifier>, Vec<InitDeclarator>, CodeLoc) =
    declaration_seq(<decl_specs_unique()>, <declaration2()>)

rule declaration2() -> (Vec<DeclarationSpecifier>, Vec<InitDeclarator>, CodeLoc) =
    declaration_seq(<declaration_typedef()>, <declaration_typedef_tail()>)


// What can follow a type specifier keyword or typename in a declaration
rule declaration_tail(s: rule<(Vec<DeclarationSpecifier>, CodeLoc)>)
    -> (Vec<DeclarationSpecifier>, Vec<InitDeclarator>, CodeLoc) =
    declaration_seq(<s()>, <declaration_tail1(<s()>)>)

rule declaration_tail1(s: rule<(Vec<DeclarationSpecifier>, CodeLoc)>)
    -> (Vec<DeclarationSpecifier>, Vec<InitDeclarator>, CodeLoc) =
    declaration_seq(<declaration_typedef()>, <declaration_typedef_tail1(<s()>)>) /
    d:declaration_init_declarators() { (Vec::new(), d.0, d.1) }

// What can follow a typedef keyword
rule declaration_typedef_tail() -> (Vec<DeclarationSpecifier>, Vec<InitDeclarator>, CodeLoc) =
    declaration_seq(<decl_specs_unique()>, <declaration_typedef_tail0()>)

rule declaration_typedef_tail0() -> (Vec<DeclarationSpecifier>, Vec<InitDeclarator>, CodeLoc) =
    declaration_seq(<declaration_unique_type()>, <declaration_typedef_tail1(<decl_specs_unique()>)>) /
    declaration_seq(<declaration_nonunique_type()>, <declaration_typedef_tail1(<decl_specs_nonunique()>)>)

// What can follow after typedef + type name
rule declaration_typedef_tail1(s: rule<(Vec<DeclarationSpecifier>, CodeLoc)>)
    -> (Vec<DeclarationSpecifier>, Vec<InitDeclarator>, CodeLoc)
    = s:s() d:declaration_type_declarators() { (s.0, d.0, l_from(s.1, d.1)) }

rule declaration_unique_type() -> (Vec<DeclarationSpecifier>, CodeLoc) =
    n:decl_spec_unique_type0() { (vec![ n ], n.loc) }

rule declaration_nonunique_type() -> (Vec<DeclarationSpecifier>, CodeLoc) =
    n:decl_spec_nonunique_type0() { (vec![ n ], n.loc) }

rule decl_specs() -> (Vec<DeclarationSpecifier>, CodeLoc) =
    s:decl_specs_unique() t:decl_specs_tail() { (concat(s.0, t.0), l_from(s.1, t.1)) }

rule decl_specs_tail() -> (Vec<DeclarationSpecifier>, CodeLoc) =
    t:declaration_unique_type() s:decl_specs_unique() { (concat(t.0, s.0), l_from(t.1, s.1)) } /
    t:declaration_nonunique_type() s:decl_specs_nonunique() { (concat(t.0, s.0), l_from(t.1, s.1)) }

rule decl_specs_unique() -> (Vec<DeclarationSpecifier>, CodeLoc) = list0(<decl_spec_nontype()>)

rule decl_specs_nonunique() -> (Vec<DeclarationSpecifier>, CodeLoc) =
    list0(<decl_spec_nontype() / decl_spec_nonunique_type0()>)

rule decl_spec_nontype() -> DeclarationSpecifier =
    s:storage_class_specifier() { s } /
    s:type_qualifier() {
        DeclarationSpecifier {
            kind: DeclarationSpecifierKind::TypeQualifier(s),
            loc: s.loc,
        }
    }
    // s:function_specifier() { s }

rule declaration_typedef() -> (Vec<DeclarationSpecifier>, CodeLoc) =
    s:declaration_typedef0() { (vec![ s ], s.loc) }

rule declaration_typedef0() -> DeclarationSpecifier =
    s:storage_class_typedef() { s }

rule decl_spec_unique_type0() -> DeclarationSpecifier = s:type_specifier_unique() {
    DeclarationSpecifier { loc: s.loc, kind: DeclarationSpecifierKind::TypeSpecifier(s) }
}

rule decl_spec_nonunique_type0() -> DeclarationSpecifier = s:type_specifier_nonunique() {
    DeclarationSpecifier { loc: s.loc, kind: DeclarationSpecifierKind::TypeSpecifier(s) }
}

rule declaration_init_declarators() -> (Vec<InitDeclarator>, CodeLoc) = cs0(<init_declarator()>)

rule declaration_type_declarators() -> (Vec<InitDeclarator>, CodeLoc) = cs0(<type_declarator()>)

rule init_declarator() -> InitDeclarator = d:init_declarator_declarator() i:init_declarator_init()?  {
    let loc = if let Some(i) = i {
        l_from(d.loc, i.loc)
    } else {
        d.loc
    };

    InitDeclarator {
        loc,
        declarator: d,
        initializer: i,
    }
}

rule init_declarator_declarator() -> Declarator =
    d:declarator() {
        env.handle_declarator(&d, false);
        d
    }

rule init_declarator_init() -> Initializer = [TokenKind::Eq] i:initializer() { i }

rule type_declarator() -> InitDeclarator = d:declarator() {
    env.handle_declarator(&d, true);
    InitDeclarator {
        loc: d.loc,
        declarator: d,
        initializer: None,
    }
}

////
// 6.7.1 Storage-class specifiers
////

rule storage_class_specifier() -> DeclarationSpecifier =
    pos:position!() [TokenKind::Extern] {
        DeclarationSpecifier {
            kind: DeclarationSpecifierKind::Extern,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [TokenKind::Static] {
        DeclarationSpecifier {
            kind: DeclarationSpecifierKind::Static,
            loc: env.locs[pos],
        }
    }

rule storage_class_typedef() -> DeclarationSpecifier =
    pos:position!() [TokenKind::Typedef] {
        DeclarationSpecifier {
            kind: DeclarationSpecifierKind::Typedef,
            loc: env.locs[pos],
        }
    }

////
// 6.7.2 Type specifiers
////

rule type_specifier_unique() -> TypeSpecifier =
    pos:position!() [TokenKind::Void] {
        TypeSpecifier {
            kind: TypeSpecifierKind::Void,
            loc: env.locs[pos],
        }
    }

rule type_specifier_nonunique() -> TypeSpecifier =
    pos:position!() [TokenKind::Char] {
        TypeSpecifier {
            kind: TypeSpecifierKind::Char,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [TokenKind::Short] {
        TypeSpecifier {
            kind: TypeSpecifierKind::Short,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [TokenKind::Int] {
        TypeSpecifier {
            kind: TypeSpecifierKind::Int,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [TokenKind::Long] {
        TypeSpecifier {
            kind: TypeSpecifierKind::Long,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [TokenKind::Float] {
        TypeSpecifier {
            kind: TypeSpecifierKind::Float,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [TokenKind::Double] {
        TypeSpecifier {
            kind: TypeSpecifierKind::Double,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [TokenKind::Signed] {
        TypeSpecifier {
            kind: TypeSpecifierKind::Signed,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [TokenKind::Unsigned] {
        TypeSpecifier {
            kind: TypeSpecifierKind::Unsigned,
            loc: env.locs[pos],
        }
    }

rule specifier_qualifiers() -> (Vec<SpecifierQualifier>, CodeLoc) =
    list_eq1_n(<specifier_qualifier_unique_type0()>, <specifier_qualifier_qualifier0()>) /
    list_ge1_n(<specifier_qualifier_nonunique_type0()>, <specifier_qualifier_qualifier0()>)

rule specifier_qualifier_unique_type0() -> SpecifierQualifier = s:type_specifier_unique() {
    SpecifierQualifier {
        kind: SpecifierQualifierKind::TypeSpecifier(s),
        loc: s.loc,
    }
}

rule specifier_qualifier_nonunique_type0() -> SpecifierQualifier = s:type_specifier_nonunique() {
    SpecifierQualifier {
        kind: SpecifierQualifierKind::TypeSpecifier(s),
        loc: s.loc,
    }
}

rule specifier_qualifier_qualifier0() -> SpecifierQualifier = q:type_qualifier() {
    SpecifierQualifier {
        kind: SpecifierQualifierKind::TypeQualifier(q),
        loc: q.loc,
    }
}

rule type_qualifier() -> TypeQualifier =
    pos:position!() [TokenKind::Volatile] {
        TypeQualifier {
            kind: TypeQualifierKind::Volatile,
            loc: env.locs[pos],
        }
    }

rule declarator() -> Declarator
    = pointer:list0(<pointer()>) decl:direct_declarator() derived:list0(<derived_declarator()>)
{
    let (mut pointer, mut begin_loc) = pointer;
    if begin_loc == NO_FILE {
        begin_loc = decl.loc;
    }

    let (derived, mut end_loc) = derived;
    if end_loc == NO_FILE {
        end_loc = decl.loc;
    }

    let mut decl = decl;
    let loc = l_from(begin_loc, end_loc);

    let mut pointer = pointer;
    pointer.extend(derived);
    decl.derived = env.buckets.add_array(pointer);
    decl.loc = loc;
    decl
}

rule direct_declarator() -> Declarator =
    pos:position!() i:ident() {
        Declarator {
            kind: DeclaratorKind::Identifier(i.0),
            derived: &[],
            loc: i.1,
        }
}

rule derived_declarator() -> DerivedDeclarator  =
    pos:position!() [TokenKind::LBracket] a:array_declarator() pos2:position!() [TokenKind::RBracket] {
        DerivedDeclarator {
            kind: DerivedDeclaratorKind::Array(a),
            loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    } /
    pos:position!() [TokenKind::LParen] f:scoped(<function_declarator()>) pos2:position!() [TokenKind::RParen] {
        DerivedDeclarator {
            kind: DerivedDeclaratorKind::Function(f),
            loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    }

rule array_declarator() -> ArrayDeclarator =
    q:list0(<type_qualifier()>) {
        let (q, loc) = q;
        ArrayDeclarator {
            qualifiers: env.buckets.add_array(q),
            size: ArraySize {
                kind: ArraySizeKind::Unknown,
                loc,
            },
            loc,
        }
    } /
    q:list0(<type_qualifier()>) e:assignment_expression() {
        let (q, mut begin_loc) = q;
        if begin_loc == NO_FILE {
            begin_loc = e.loc;
        }

        ArrayDeclarator {
            qualifiers: env.buckets.add_array(q),
            size: ArraySize{
               loc: e.loc,
               kind: ArraySizeKind::VariableExpression(env.buckets.add(e)),
            },
            loc: l_from(begin_loc, e.loc),
        }
    }

rule function_declarator() -> FunctionDeclarator =
    params:cs1(<parameter_declaration()>) pos:position!() varargs:([TokenKind::Comma] [TokenKind::DotDotDot])?
{
    let (params, mut loc) = params;
    let varargs = varargs.is_some();
    if varargs {
        loc = l_from(loc, env.locs[pos + 1]);
    }

    FunctionDeclarator {
        parameters: env.buckets.add_array(params),
        varargs,
        loc,
    }
}


rule pointer() -> DerivedDeclarator = pos:position!() [TokenKind::Star] q:list0(<type_qualifier()>) {
    let (q, mut end_loc) = q;
    let loc = env.locs[pos];
    if end_loc == NO_FILE {
        end_loc = loc;
    }

    let loc = l_from(loc, end_loc);
    DerivedDeclarator {
        kind: DerivedDeclaratorKind::Pointer(env.buckets.add_array(q)),
        loc,
    }
}

rule parameter_declaration() -> ParameterDeclaration = s:decl_specs() d:parameter_declarator()
{
    let (specs, mut loc) = s;
    if let Some(decl) = d {
        loc = l_from(loc, decl.loc);
    }

    ParameterDeclaration {
        specifiers: env.buckets.add_array(specs),
        declarator: d,
        loc,
    }
}


rule parameter_declarator() -> Option<Declarator> =
    d:declarator() {
        env.handle_declarator(&d, false);
        Some(d)
    } /
    d:abstract_declarator() { Some(d) } /
    { None }


rule type_name() -> TypeName = s:specifier_qualifiers() d:abstract_declarator()? {
    let (sqs, mut loc) = s;

    if let Some(d) = d {
        loc = l_from(loc, d.loc);
    }

    TypeName {
        specifiers: env.buckets.add_array(sqs),
        declarator: d,
        loc,
    }
}

// rule function_specifiers

rule abstract_declarator() -> Declarator =
    p:list0(<pointer()>) k:direct_abstract_declarator() d:list0(<derived_abstract_declarator()>) {
        let (mut p, begin_loc) = p;
        let (d, end_loc) = d;
        let loc = l_from(begin_loc, k.loc);
        let loc = l_from(loc, end_loc);

        let mut declarator = k;
        p.extend(d);
        declarator.loc = loc;
        declarator.derived = env.buckets.add_array(p);
        declarator
    } /
    p:list0(<pointer()>) d:list1(<derived_abstract_declarator()>) {
        let (p, begin_loc) = p;
        let (d, end_loc) = d;
        let loc = l_from(begin_loc, end_loc);

        Declarator {
            kind: DeclaratorKind::Abstract,
            derived: env.buckets.add_array(concat(p, d)),
            loc,
        }
    } /
    p:list1(<pointer()>) {
        let (p, loc) = p;

        Declarator {
            kind: DeclaratorKind::Abstract,
            derived: env.buckets.add_array(p),
            loc,
        }
    }

rule direct_abstract_declarator() -> Declarator =
    pos:position!() [TokenKind::LParen] d:abstract_declarator() pos2:position!() [TokenKind::RParen]
{
    Declarator {
        kind: DeclaratorKind::Declarator(env.buckets.add(d)),
        derived: &[],
        loc: l_from(env.locs[pos], env.locs[pos2]),
    }
}

rule derived_abstract_declarator() -> DerivedDeclarator =
    pos:position!() [TokenKind::LBracket] a:abstract_array_declarator() pos2:position!() [TokenKind::RBracket] {
        DerivedDeclarator {
            kind: DerivedDeclaratorKind::Array(a),
            loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    }

rule abstract_array_declarator() -> ArrayDeclarator =
    q:list0(<type_qualifier()>) {
        let (q, loc) = q;
        ArrayDeclarator {
            qualifiers: env.buckets.add_array(q),
            size: ArraySize {
                kind: ArraySizeKind::Unknown,
                loc,
            },
            loc,
        }
    } /
    q:list0(<type_qualifier()>) e:assignment_expression() {
        let (q, loc) = q;

        ArrayDeclarator {
            qualifiers: env.buckets.add_array(q),
            size: ArraySize {
                kind: ArraySizeKind::VariableExpression(env.buckets.add(e)),
                loc: e.loc,
            },
            loc: l_from(loc, e.loc),
        }
    }

rule typedef_name() -> (u32, CodeLoc) = quiet! { typedef_name0() } / expected!("<typedef_name>")

rule typedef_name0() -> (u32, CodeLoc) = i:ident() {?
    if env.is_typename(i.0) {
        Ok(i)
    } else {
        Err("<unused>")
    }
}

rule initializer() -> Initializer =
    e:assignment_expression() {
        Initializer {
            kind: InitializerKind::Expr(env.buckets.add(e)),
            loc: e.loc,
        }
    } /
    pos:position!() [TokenKind::LBrace] i:cs1(<initializer_list_item()>)
    [TokenKind::Comma]? pos2:position!() [TokenKind::RBrace]
    {
        Initializer {
            kind: InitializerKind::List(env.buckets.add_array(i.0)),
            loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    }

rule initializer_list_item() -> Expr = expr()

pub rule statement() -> Statement =
    labeled_statement() /
    b:scoped(<compound_statement()>) {
        Statement {
            kind: StatementKind::Block(b),
            loc: b.loc,
        }
    } /
    expression_statement() /
    scoped(<selection_statement()>) /
    scoped(<iteration_statement()>) /
    jump_statement()

////
// 6.8.1 Labeled statements
////

rule labeled_statement() -> Statement =
    i:ident() [TokenKind::Colon] s:statement() {
        let (i, loc) = i;
        Statement {
            loc: l_from(loc, s.loc),
            kind: StatementKind::Labeled {
                label: i,
                label_loc: loc,
                stmt: env.buckets.add(s),
            }
        }
    } /
    pos:position!() [TokenKind::Case] i:constant_expr() s:statement() {
        Statement {
            loc: l_from(env.locs[pos], s.loc),
            kind: StatementKind::CaseLabeled {
                case_value: i,
                stmt: env.buckets.add(s),
            }
        }
    } /
    pos:position!() [TokenKind::Default] [TokenKind::Colon] s:statement() {
        Statement {
            loc: l_from(env.locs[pos], s.loc),
            kind: StatementKind::DefaultCaseLabeled(env.buckets.add(s))
        }
    }

////
// 6.8.2 Compound statement
////

rule compound_statement() -> Block =
    pos:position!() [TokenKind::LBrace] b:list0(<block_item()>) pos2:position!() [TokenKind::RBrace]
{
    let (block, loc) = b;

    Block{
        stmts: env.buckets.add(block),
        loc: l_from(env.locs[pos], env.locs[pos2]),
    }
}

rule block_item() -> BlockItem =
    d:declaration() {
        BlockItem {
            kind: BlockItemKind::Declaration(d),
            loc: d.loc,
        }
    } /
    s:statement() {
        BlockItem {
            kind: BlockItemKind::Statement(s),
            loc: s.loc,
        }
    }

////
// 6.8.3 Expression and null statements
////

rule expression_statement() -> Statement = e:expr() [TokenKind::Semicolon] {
    Statement {
        loc: e.loc,
        kind: StatementKind::Expr(e),
    }
}

////
// 6.8.4 Selection statement
////

rule selection_statement() -> Statement =
    pos:position!() [TokenKind::If] [TokenKind::LParen] e:expr()
    [TokenKind::RParen] a:statement() b:else_statement()?
    {
        let mut loc = l_from(env.locs[pos], a.loc);
        if let Some(else_stmt) = b {
            loc = l_from(loc, else_stmt.loc);
        }

        Statement {
            kind: StatementKind::Branch {
                if_cond: e,
                if_body: env.buckets.add(a),
                else_body: env.buckets.add(b).as_ref(),
            },
            loc,
        }
    } /
    pos:position!() [TokenKind::Switch] [TokenKind::LParen] e:expr() [TokenKind::RParen] a:statement() {
        let mut loc = l_from(env.locs[pos], a.loc);

        Statement {
            kind: StatementKind::Switch {
                expr: e,
                body: env.buckets.add(a),
            },
            loc,
        }
    }

rule else_statement() -> Statement = [TokenKind::Else] s:statement() { s }

////
// 6.8.5 Iteration statement
////

rule iteration_statement() -> Statement =
    s:while_statement() { s } /
    s:do_while_statement() { s } /
    s:for_statement() { s }

rule while_statement() -> Statement =
    pos:position!() [TokenKind::While] [TokenKind::LParen] e:expr() [TokenKind::RParen] s:statement() {
        let loc = l_from(env.locs[pos], s.loc);

        Statement {
            kind: StatementKind::While {
                condition: e,
                body: env.buckets.add(s),
            },
            loc,
        }
    }

rule do_while_statement() -> Statement =
    pos:position!() [TokenKind::Do] s:statement() [TokenKind::While] [TokenKind::LParen]
    e:expr() [TokenKind::RParen] pos2:position!() [TokenKind::Semicolon] {
        let loc = l_from(env.locs[pos], env.locs[pos2]);

        Statement {
            kind: StatementKind::DoWhile {
                condition: e,
                body: env.buckets.add(s),
            },
            loc,
        }
    }

rule for_statement() -> Statement =
    pos:position!() [TokenKind::For] [TokenKind::LParen] a:expr()? [TokenKind::Semicolon] b:expr()?
    [TokenKind::Semicolon] c:expr()? [TokenKind::RParen] s:statement() {
        let loc = l_from(env.locs[pos], s.loc);

        Statement {
            kind: StatementKind::For {
                at_start: a,
                condition: b,
                post_expr: c,
                body: env.buckets.add(s),
            },
            loc,
        }
    } /
    pos:position!() [TokenKind::For] [TokenKind::LParen] a:declaration() b:expr()?
    [TokenKind::Semicolon] c:expr()? [TokenKind::RParen] s:statement() {
        let loc = l_from(env.locs[pos], s.loc);

        Statement {
            kind: StatementKind::ForDecl {
                decl: a,
                condition: b,
                post_expr: c,
                body: env.buckets.add(s),
            },
            loc,
        }
    }


////
// 6.8.6 Jump statements
////

rule jump_statement() -> Statement =
    pos:position!() [TokenKind::Goto] i:ident() pos2:position!() [TokenKind::Semicolon] {
        let (i, label_loc) = i;
        let loc = l_from(env.locs[pos], env.locs[pos2]);
        Statement {
            kind: StatementKind::Goto {
                label: i,
                label_loc,
            },
            loc
        }
    } /
    pos:position!() [TokenKind::Continue] pos2:position!() [TokenKind::Semicolon] {
        let loc = l_from(env.locs[pos], env.locs[pos2]);

        Statement {
            kind: StatementKind::Continue,
            loc
        }
    } /
    pos:position!() [TokenKind::Break] pos2:position!() [TokenKind::Semicolon] {
        let loc = l_from(env.locs[pos], env.locs[pos2]);

        Statement {
            kind: StatementKind::Break,
            loc
        }
    } /
    pos:position!() [TokenKind::Return] pos2:position!() [TokenKind::Semicolon] {
        let loc = l_from(env.locs[pos], env.locs[pos2]);

        Statement {
            kind: StatementKind::Ret,
            loc
        }
    } /
    pos:position!() [TokenKind::Return] e:expr() pos2:position!() [TokenKind::Semicolon] {
        let loc = l_from(env.locs[pos], env.locs[pos2]);

        Statement {
            kind: StatementKind::RetVal(e),
            loc
        }
    }

////
// 6.9 External definitions
////

pub rule translation_unit() -> Vec<GlobalStatement> = d:external_declaration()*

rule external_declaration() -> GlobalStatement =
    d:declaration() {
        GlobalStatement {
            loc: d.loc,
            kind: GlobalStatementKind::Declaration(d),
        }
    } /
    d:scoped(<function_definition()>) {
        GlobalStatement {
            loc: d.loc,
            kind: GlobalStatementKind::FunctionDefinition(d),
        }
    }

rule function_definition() -> FunctionDefinition =
    a:decl_specs() b:declarator() c:list0(<declaration()>) d:compound_statement() {
        let (a, begin_loc) = a;
        let (c, _) = c;

        let loc = l_from(begin_loc, d.loc);
        FunctionDefinition {
            specifiers: env.buckets.add_array(a),
            declarator: env.buckets.add(b),
            declarations: env.buckets.add_array(c),
            statements: d,
            loc,
        }
    }

}
}
