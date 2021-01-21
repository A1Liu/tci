use crate::ast::*;
use crate::buckets::*;
use crate::lexer::*;
use crate::util::*;
use std::cell::RefCell;

pub enum LiteralType {
    Int,
    Unsigned,
    Long,
    LongLong,
    UnsignedLong,
    UnsignedLongLong,
}

pub struct ParseEnv {
    pub file: u32,
    pub symbol_is_type: RefCell<Vec<HashMap<u32, bool>>>, // true is type
    pub locs: Vec<CodeLoc>,
    pub buckets: BucketListFactory,
    pub tree: Vec<GlobalStatement>,
}

impl Drop for ParseEnv {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

impl ParseEnv {
    pub fn new(file: u32, locs: Vec<CodeLoc>) -> Self {
        Self {
            file,
            // TODO This is a hack to work around stuff in rust-peg
            symbol_is_type: RefCell::new(vec![HashMap::new()]),
            locs,
            tree: Vec::new(),
            buckets: BucketListFactory::new(),
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
        // TODO handle redeclaration of type as identifier
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

    pub fn add_symbol(&self, sym: u32, is_type: bool) -> Option<bool> {
        let mut raii_borrow = self.symbol_is_type.borrow_mut();
        let scope_o = raii_borrow.last_mut();
        let scope = scope_o.expect("at least one scope should be always present");
        return scope.insert(sym, is_type);
    }
}

#[inline]
pub fn concat<E>(mut a: Vec<E>, b: Vec<E>) -> Vec<E> {
    a.extend(b);
    return a;
}

pub fn parse(file: u32, toks: Vec<TokenKind>, locs: Vec<CodeLoc>) -> Result<ParseEnv, Error> {
    let mut parser = ParseEnv::new(file, locs);
    match c_parser::translation_unit(&toks, &mut parser) {
        Ok(tree) => {
            parser.tree = tree;
        }
        Err(err) => {
            return Err(error!(
                &format!("expected set: {}", err.expected),
                parser.locs[err.location],
                format!("unexpected token '{:?}' found here", toks[err.location])
            ));
        }
    }

    return Ok(parser);
}

peg::parser! {

// Translated from https://github.com/vickenty/lang-c/blob/master/grammar.rustpeg
pub grammar c_parser(env: &ParseEnv) for [TokenKind] {

use TokenKind::*;
use NumChar::*;

rule w() = [Whitespace]* {}

rule list0<E>(x: rule<E>) -> (Vec<E>, CodeLoc) = pos:position!() v:(x() ** w()) pos2:position!() {
    if pos == pos2 {
        (v, NO_FILE)
    } else {
        (v, l_from(env.locs[pos], env.locs[pos2 - 1]))
    }
}

rule list1<E>(x: rule<E>) -> (Vec<E>, CodeLoc) = pos:position!() v:(x() ++ w()) pos2:position!() {
    (v, l_from(env.locs[pos], env.locs[pos2 - 1]))
}

rule cs0<E>(x: rule<E>) -> (Vec<E>, CodeLoc) =
    pos:position!() v:(x() ** (w() [Comma] w())) pos2:position!() {
    if pos == pos2 {
        (v, NO_FILE)
    } else {
        (v, l_from(env.locs[pos], env.locs[pos2 - 1]))
    }
}

rule cs1<E>(x: rule<E>) -> (Vec<E>, CodeLoc) =
    pos:position!() v:(x() ++ (w() [Comma] w())) pos2:position!() {
    (v, l_from(env.locs[pos], env.locs[pos2 - 1]))
}

rule list_010<E>(b: rule<E>, s: rule<E>, a: rule<E>) -> (Vec<E>, CodeLoc) =
    before:list0(<b()>) w() pos:position!() single:s() w() after:list0(<a()>)
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

rule pragma() -> (&'static str, CodeLoc) = pos:position!() n:$[Pragma(_)] {
    match n[0] {
        Pragma(n) => (n, env.locs[pos]),
        _ => unreachable!(),
    }
}

rule raw_ident() -> (u32, CodeLoc) = pos:position!() n:$[Ident(_)] {
    match n[0] {
        Ident(n) => (n, env.locs[pos]),
        _ => unreachable!(),
    }
}

rule ident() -> (u32, CodeLoc) = i:raw_ident() {?
    if !env.is_typename(i.0) {
        Ok(i)
    } else {
        Err("<ident>")
    }
}

rule number_lit() -> u8 =
    [IntChar(_0)] { b'0' } /
    [IntChar(_1)] { b'1' } /
    [IntChar(_2)] { b'2' } /
    [IntChar(_3)] { b'3' } /
    [IntChar(_4)] { b'4' } /
    [IntChar(_5)] { b'5' } /
    [IntChar(_6)] { b'6' } /
    [IntChar(_7)] { b'7' } /
    [IntChar(_8)] { b'8' } /
    [IntChar(_9)] { b'9' }

rule hex_number_lit() -> u8 =
    [IntChar(_0)] { b'0' } /
    [IntChar(_1)] { b'1' } /
    [IntChar(_2)] { b'2' } /
    [IntChar(_3)] { b'3' } /
    [IntChar(_4)] { b'4' } /
    [IntChar(_5)] { b'5' } /
    [IntChar(_6)] { b'6' } /
    [IntChar(_7)] { b'7' } /
    [IntChar(_8)] { b'8' } /
    [IntChar(_9)] { b'9' } /
    [IntChar(_A)] { b'A' } /
    [IntChar(_B)] { b'B' } /
    [IntChar(_C)] { b'C' } /
    [IntChar(_D)] { b'D' } /
    [IntChar(_E)] { b'E' } /
    [IntChar(_F)] { b'F' }

rule number_lit_seq() -> String =
    nums:number_lit()+ {
        unsafe { String::from_utf8_unchecked(nums) }
    } /
    [IntChar(_0)] {
        "0".to_string()
    }

rule hex_number_lit_seq() -> String =
    nums:hex_number_lit()+ {
        unsafe { String::from_utf8_unchecked(nums) }
    } /
    [IntChar(_0)] {
        "0".to_string()
    }

rule float_number_lit_seq_strict() -> String =
    n:number_lit_seq() [Dot] trailing:number_lit()+ {
        let mut n = n;
        n.push('.');
        n.push_str(&unsafe { String::from_utf8_unchecked(trailing) });
        n
    }

rule float_number_lit_seq() -> String =
    float_number_lit_seq_strict() /
    number_lit_seq()


rule dec_number_type_part() -> u8 =
    [IntChar(_L)] { b'l' } /
    [IntChar(_U)] { b'u' }

rule dec_number_type() -> LiteralType = parts:dec_number_type_part()* {?
    let mut unsigned = 0;
    let mut long = 0;

    for part in parts {
        if part == b'l' {
            long += 1;
        } else if part == b'u' {
            unsigned += 1;
        }
    }

    if unsigned == 0 && long == 0 {
        Ok(LiteralType::Int)
    } else if unsigned == 0 && long == 1 {
        Ok(LiteralType::Long)
    } else if unsigned == 0 && long == 2 {
        Ok(LiteralType::LongLong)
    } else if unsigned == 1 && long == 0 {
        Ok(LiteralType::Unsigned)
    } else if unsigned == 1 && long == 1 {
        Ok(LiteralType::UnsignedLong)
    } else if unsigned == 1 && long == 2 {
        Ok(LiteralType::UnsignedLongLong)
    } else {
        Err("integer suffix")
    }
}


rule float_number() -> Expr =
    pos:position!() bef:float_number_lit_seq() [IntChar(_E)]
    dash:[Dash]? aft:float_number_lit_seq() [IntChar(_F)] pos2:position!() {?
        let loc = l_from(env.locs[pos], env.locs[pos2 - 1]);

        let mult = if dash.is_some() { -1f32 } else { 1f32 };
        let opt = str::parse::<f32>(&bef).ok().zip(str::parse::<f32>(&aft).ok());
        opt.map(|(bef, aft)| bef.powf(mult * aft)).ok_or("exponential").map(|float| {
            Expr {
                kind: ExprKind::FloatLit(float),
                loc,
            }
        })
    } /
    pos:position!() bef:float_number_lit_seq() [IntChar(_E)]
    dash:[Dash]? aft:float_number_lit_seq() pos2:position!() {?
        let loc = l_from(env.locs[pos], env.locs[pos2 - 1]);

        let mult = if dash.is_some() { -1f64 } else { 1f64 };
        let opt = str::parse::<f64>(&bef).ok().zip(str::parse::<f64>(&aft).ok());
        opt.map(|(bef, aft)| bef.powf(mult * aft)).ok_or("exponential").map(|double| {
            Expr {
                kind: ExprKind::DoubleLit(double),
                loc,
            }
        })
    } /
    pos:position!() n:float_number_lit_seq() [IntChar(_F)] pos2:position!() {?
        let loc = l_from(env.locs[pos], env.locs[pos2 - 1]);

        str::parse::<f32>(&n).map_err(|e| "float constant").map(|float| {
            Expr {
                kind: ExprKind::FloatLit(float),
                loc,
            }
        })
    } /
    pos:position!() n:float_number_lit_seq_strict() pos2:position!() {?
        let loc = l_from(env.locs[pos], env.locs[pos2 - 1]);

        str::parse::<f64>(&n).map_err(|e| "double constant").map(|double| {
            Expr {
                kind: ExprKind::DoubleLit(double),
                loc,
            }
        })
    }

rule dec_number() -> Expr =
    pos:position!() [IntChar(_0)] [IntChar(_X)] n:hex_number_lit_seq()
    ty:dec_number_type() pos2:position!() {?
        let loc = l_from(env.locs[pos], env.locs[pos2 - 1]);

        let kind = match ty {
            LiteralType::Int => i32::from_str_radix(&n ,16).map(|n| ExprKind::IntLit(n)),
            LiteralType::Long => i64::from_str_radix(&n ,16).map(|n| ExprKind::LongLit(n)),
            LiteralType::LongLong => i64::from_str_radix(&n ,16).map(|n| ExprKind::LongLit(n)),
            LiteralType::Unsigned => u32::from_str_radix(&n ,16).map(|n| ExprKind::ULit(n)),
            LiteralType::UnsignedLong => u64::from_str_radix(&n ,16).map(|n| ExprKind::ULongLit(n)),
            LiteralType::UnsignedLongLong => u64::from_str_radix(&n ,16).map(|n| ExprKind::ULongLit(n)),
        };

        kind.map_err(|e| "hex integer constant").map(|kind| Expr { kind, loc })
    } /
    pos:position!() n:number_lit_seq() ty:dec_number_type() pos2:position!() {?
        let loc = l_from(env.locs[pos], env.locs[pos2 - 1]);

        let kind = match ty {
            LiteralType::Int => i32::from_str_radix(&n ,10).map(|n| ExprKind::IntLit(n)),
            LiteralType::Long => i64::from_str_radix(&n ,10).map(|n| ExprKind::LongLit(n)),
            LiteralType::LongLong => i64::from_str_radix(&n ,10).map(|n| ExprKind::LongLit(n)),
            LiteralType::Unsigned => u32::from_str_radix(&n ,10).map(|n| ExprKind::ULit(n)),
            LiteralType::UnsignedLong => u64::from_str_radix(&n ,10).map(|n| ExprKind::ULongLit(n)),
            LiteralType::UnsignedLongLong => u64::from_str_radix(&n ,10).map(|n| ExprKind::ULongLit(n)),
        };

        kind.map_err(|e| "integer constant").map(|kind| Expr { kind, loc })
    }

rule char() -> (i8, CodeLoc) = pos:position!() n:$[CharLit(_)] {
    match n[0] {
        CharLit(n) => (n, env.locs[pos]),
        _ => unreachable!(),
    }
}

rule string() -> (&'static str, CodeLoc) = pos:position!() n:$([StringLit(_)] ++ w()) pos2:position!() {
    let mut string = String::new();
    let loc = l_from(env.locs[pos], env.locs[pos2 - 1]);
    for token in n {
        let s = match token {
            StringLit(s) => s,
            _ => unreachable!(),
        };

        string.push_str(s);
    }

    (env.buckets.add_str(&string), loc)
}

rule constant_expr() -> Expr =
    float_number() /
    dec_number() /
    n:string() {
        let (n, loc) = n;

        Expr {
            kind: ExprKind::StringLit(n),
            loc,
        }
    } /
    n:char() {
        let (n, loc) = n;

        Expr {
            kind: ExprKind::CharLit(n),
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
    pos:position!() [LParen] w() e:expr() w() pos2:position!() [RParen] {
        Expr {
            kind: e.kind,
            loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    } /
    pos:position!() [Sizeof] w() [LParen]
    w() t:type_name() w() pos2:position!() [RParen] {
        Expr { loc: l_from(env.locs[pos], env.locs[pos2]), kind: ExprKind::SizeofTy(t)  }
    }


rule assignment_expr() -> Expr = precedence! {
    x:@ w() [Eq] w() y:(@) {
        let (x, y) = env.buckets.add((x, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Assign {op: AssignOp::Assign, to: x, val: y }
        }
    }

    x:@ w() [PlusEq] w() y:(@) {
        let (x, y) = env.buckets.add((x, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Assign {op: AssignOp::MutAssign(BinOp::Add), to: x, val: y }
        }
    }
    x:@ w() [DashEq] w() y:(@) {
        let (x, y) = env.buckets.add((x, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Assign {op: AssignOp::MutAssign(BinOp::Sub), to: x, val: y }
        }
    }
    x:@ w() [StarEq] w() y:(@) {
        let (x, y) = env.buckets.add((x, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Assign {op: AssignOp::MutAssign(BinOp::Mul), to: x, val: y }
        }
    }
    x:@ w() [SlashEq] w() y:(@) {
        let (x, y) = env.buckets.add((x, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Assign {op: AssignOp::MutAssign(BinOp::Div), to: x, val: y }
        }
    }
    x:@ w() [PercentEq] w() y:(@) {
        let (x, y) = env.buckets.add((x, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Assign {op: AssignOp::MutAssign(BinOp::Mod), to: x, val: y }
        }
    }
    x:@ w() [LtLtEq] w() y:(@) {
        let (x, y) = env.buckets.add((x, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Assign {op: AssignOp::MutAssign(BinOp::LShift), to: x, val: y }
        }
    }
    x:@ w() [GtGtEq] w() y:(@) {
        let (x, y) = env.buckets.add((x, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Assign {op: AssignOp::MutAssign(BinOp::RShift), to: x, val: y }
        }
    }
    x:@ w() [AmpEq] w() y:(@) {
        let (x, y) = env.buckets.add((x, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Assign {op: AssignOp::MutAssign(BinOp::BitAnd), to: x, val: y }
        }
    }
    x:@ w() [CaretEq] w() y:(@) {
        let (x, y) = env.buckets.add((x, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Assign {op: AssignOp::MutAssign(BinOp::BitXor), to: x, val: y }
        }
    }
    x:@ w() [LineEq] w() y:(@) {
        let (x, y) = env.buckets.add((x, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Assign {op: AssignOp::MutAssign(BinOp::BitOr), to: x, val: y }
        }
    }

    --
    x:@ w() [Question] w() e:expr() w() [Colon] w() y:(@) {
        let (x, e, y) = env.buckets.add((x, e, y));
        Expr {
            loc: l_from(x.loc, y.loc),
            kind: ExprKind::Ternary { condition: x, if_true: e, if_false: y }
        }
    }

    --
    x:(@) w() [LineLine] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::BoolOr, x, y) }
    }

    --
    x:(@) w() [AmpAmp] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::BoolAnd, x, y) }
    }

    --
    x:(@) w() [Line] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::BitOr, x, y) }
    }

    --
    x:(@) w() [Caret] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::BitXor, x, y) }
    }

    --
    x:(@) w() [Amp] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::BitAnd, x, y) }
    }

    --
    x:(@) w() [EqEq] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Eq, x, y) }
    }
    x:(@) w() [Neq] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Neq, x, y) }
    }

    --
    x:(@) w() [Gt] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Gt, x, y) }
    }
    x:(@) w() [Geq] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Geq, x, y) }
    }
    x:(@) w() [Lt] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Lt, x, y) }
    }
    x:(@) w() [Leq] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Leq, x, y) }
    }

    --
    x:(@) w() [LtLt] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::LShift, x, y) }
    }
    x:(@) w() [GtGt] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::RShift, x, y) }
    }

    --
    x:(@) w() [Plus] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Add, x, y) }
    }
    x:(@) w() [Dash] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Sub, x, y) }
    }

    --
    x:(@) w() [Slash] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Div, x, y) }
    }
    x:(@) w() [Star] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Mul, x, y) }
    }
    x:(@) w() [Percent] w() y:@ {
        let (x, y) = env.buckets.add((x, y));
        Expr { loc: l_from(x.loc, y.loc), kind: ExprKind::BinOp(BinOp::Mod, x, y) }
    }

    --
    n:prefix_expr() { n }
    n:cast_expr() { n }
}

rule cast_expr() -> Expr =
    pos:position!() [LParen] w() t:type_name() w() [RParen] w() x:cast_expr() {
        let x = env.buckets.add(x);
        Expr { loc: l_from(env.locs[pos], x.loc), kind: ExprKind::Cast { to: t, from: x } }
    } /
    prefix_expr()

rule prefix_expr() -> Expr =
    pos:position!() [Amp] w() x:cast_expr() {
        let x = env.buckets.add(x);
        Expr { loc: l_from(env.locs[pos], x.loc), kind: ExprKind::UnaryOp(UnaryOp::Ref, x)  }
    } /
    pos:position!() [Star] w() x:cast_expr() {
        let x = env.buckets.add(x);
        Expr { loc: l_from(env.locs[pos], x.loc), kind: ExprKind::UnaryOp(UnaryOp::Deref, x)  }
    } /
    pos:position!() [Sizeof] w() x:prefix_expr() {
        let x = env.buckets.add(x);
        Expr { loc: l_from(env.locs[pos], x.loc), kind: ExprKind::SizeofExpr(x)  }
    } /
    pos:position!() [Bang] w() x:cast_expr() {
        let x = env.buckets.add(x);
        Expr { loc: l_from(env.locs[pos], x.loc), kind: ExprKind::UnaryOp(UnaryOp::BoolNot, x)  }
    } /
    pos:position!() [Tilde] w() x:cast_expr() {
        let x = env.buckets.add(x);
        Expr { loc: l_from(env.locs[pos], x.loc), kind: ExprKind::UnaryOp(UnaryOp::BitNot, x)  }
    } /
    pos:position!() [Dash] w() x:cast_expr() {
        let x = env.buckets.add(x);
        Expr { loc: l_from(env.locs[pos], x.loc), kind: ExprKind::UnaryOp(UnaryOp::Neg, x)  }
    } /
    pos:position!() [DashDash] w() x:cast_expr() {
        let x = env.buckets.add(x);
        Expr { loc: l_from(env.locs[pos], x.loc), kind: ExprKind::UnaryOp(UnaryOp::PreDecr, x)  }
    } /
    pos:position!() [PlusPlus] w() x:cast_expr() {
        let x = env.buckets.add(x);
        Expr { loc: l_from(env.locs[pos], x.loc), kind: ExprKind::UnaryOp(UnaryOp::PreIncr, x)  }
    } /
    postfix_expr()

rule postfix_expr() -> Expr = precedence! {
    // Postfix
    x:(@) w() [LParen] w() c:cs0(<assignment_expr()>) w() pos:position!() [RParen] {
        let (c, _) = c;
        let loc = l_from(x.loc, env.locs[pos]);
        let function = env.buckets.add(x);
        let params = env.buckets.add_array(c);
        Expr { loc, kind: ExprKind::Call { function, params } }
    }
    x:(@) w() pos:position!() [DashDash] {
        let loc = l_from(x.loc, env.locs[pos]);
        Expr { loc, kind: ExprKind::UnaryOp(UnaryOp::PostDecr, env.buckets.add(x)) }
    }
    x:(@) w() pos:position!() [PlusPlus] {
        let loc = l_from(x.loc, env.locs[pos]);
        Expr { loc, kind: ExprKind::UnaryOp(UnaryOp::PostIncr, env.buckets.add(x)) }
    }
    x:(@) w() [LBracket] w() y:expr() w() pos:position!() [RBracket] {
        let loc = l_from(x.loc, env.locs[pos]);
        let (x, y) = env.buckets.add((x, y));
        Expr { loc, kind: ExprKind::BinOp(BinOp::Index, x, y) }
    }
    x:(@) w() [Arrow] w() id:raw_ident() {
        let (id, loc) = id;
        let loc = l_from(x.loc, loc);
        let x = env.buckets.add(x);
        Expr { loc, kind: ExprKind::PtrMember { member: id, base: x } }
    }
    x:(@) w() [Dot] w() id:raw_ident() {
        let (id, loc) = id;
        let loc = l_from(x.loc, loc);
        let x = env.buckets.add(x);
        Expr { loc, kind: ExprKind::Member { member: id, base: x } }
    }

    --
    n:atom() { n }
}

rule expr() -> Expr = list:cs1(<assignment_expr()>) { // TODO alllocations!
    let (list, loc) = list;
    if list.len() == 1 {
        list[0]
    }else {
        Expr {
            kind: ExprKind::ParenList(env.buckets.add_array(list)),
            loc,
        }
    }
}

pub rule declaration() -> Declaration = d:declaration1() w() [Semicolon] {
    Declaration {
        loc: d.2,
        specifiers: env.buckets.add_array(d.0),
        declarators: env.buckets.add_array(d.1),
    }
}

rule declaration_seq<E, T>(h: rule<(Vec<E>, CodeLoc)>, t: rule<(Vec<E>, Vec<T>, CodeLoc)>)
    -> (Vec<E>, Vec<T>, CodeLoc) = head:h() w() tail:t()
{
    (concat(head.0, tail.0), tail.1, l_from(head.1, tail.2))
}

rule declaration1() -> (Vec<DeclarationSpecifier>, Vec<InitDeclarator>, CodeLoc) =
    declaration_seq(<decl_specs_unique()>, <declaration2()>)

rule declaration2() -> (Vec<DeclarationSpecifier>, Vec<InitDeclarator>, CodeLoc) =
    declaration_seq(<declaration_typedef()>, <declaration_typedef_tail()>) /
    declaration_seq(<declaration_unique_type()>, <declaration_tail(<decl_specs_unique()>)>) /
    declaration_seq(<declaration_nonunique_type()>, <declaration_tail(<decl_specs_nonunique()>)>)


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
    = s:s() w() d:declaration_type_declarators() { (s.0, d.0, l_from(s.1, d.1)) }

rule declaration_unique_type() -> (Vec<DeclarationSpecifier>, CodeLoc) =
    n:decl_spec_unique_type0() { (vec![ n ], n.loc) }

rule declaration_nonunique_type() -> (Vec<DeclarationSpecifier>, CodeLoc) =
    n:decl_spec_nonunique_type0() { (vec![ n ], n.loc) }

rule decl_specs() -> (Vec<DeclarationSpecifier>, CodeLoc) =
    s:decl_specs_unique() w() t:decl_specs_tail() { (concat(s.0, t.0), l_from(s.1, t.1)) }

rule decl_specs_tail() -> (Vec<DeclarationSpecifier>, CodeLoc) =
    t:declaration_unique_type() w() s:decl_specs_unique() { (concat(t.0, s.0), l_from(t.1, s.1)) } /
    t:declaration_nonunique_type() w() s:decl_specs_nonunique() { (concat(t.0, s.0), l_from(t.1, s.1)) }

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
    } /
    s:function_specifier() { s }

rule declaration_typedef() -> (Vec<DeclarationSpecifier>, CodeLoc) =
    s:declaration_typedef0() { (vec![ s ], s.loc) }

rule declaration_typedef0() -> DeclarationSpecifier =
    s:storage_class_typedef() { s }

rule decl_spec_unique_type0() -> DeclarationSpecifier =
    pos:position!() s:type_specifier_unique() pos2:position!()
{
    DeclarationSpecifier {
        loc: l_from(env.locs[pos], env.locs[pos2 - 1]),
        kind: DeclarationSpecifierKind::TypeSpecifier(s)
    }
}

rule decl_spec_nonunique_type0() -> DeclarationSpecifier = pos:position!() s:type_specifier_nonunique() {
    DeclarationSpecifier { loc: env.locs[pos], kind: DeclarationSpecifierKind::TypeSpecifier(s) }
}

rule declaration_init_declarators() -> (Vec<InitDeclarator>, CodeLoc) = cs0(<init_declarator()>)

rule declaration_type_declarators() -> (Vec<InitDeclarator>, CodeLoc) = cs0(<type_declarator()>)

rule init_declarator() -> InitDeclarator = d:init_declarator_declarator() w() i:init_declarator_init()?  {
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

rule init_declarator_init() -> Initializer = [Eq] w() i:initializer() { i }

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
    pos:position!() [Register] {
        DeclarationSpecifier {
            kind: DeclarationSpecifierKind::Register,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [Extern] {
        DeclarationSpecifier {
            kind: DeclarationSpecifierKind::Extern,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [Static] {
        DeclarationSpecifier {
            kind: DeclarationSpecifierKind::Static,
            loc: env.locs[pos],
        }
    }

rule storage_class_typedef() -> DeclarationSpecifier =
    pos:position!() [Typedef] {
        DeclarationSpecifier {
            kind: DeclarationSpecifierKind::Typedef,
            loc: env.locs[pos],
        }
    }

////
// 6.7.2 Type specifiers
////

rule type_specifier_unique() -> TypeSpecifier =
    [Void] { TypeSpecifier::Void } /
    pos:position!() [Struct] w() id:raw_ident()? w() declarations:struct_body() {
        let (declarations, loc) = declarations;

        if let Some((ident, _)) = id {
            TypeSpecifier::Struct(StructType {
                kind: StructTypeKind::NamedDecl {
                    ident,
                    declarations,
                },
                loc: l_from(env.locs[pos], loc),
            })
        } else {
            TypeSpecifier::Struct(StructType {
                kind: StructTypeKind::UnnamedDecl {
                    declarations,
                },
                loc: l_from(env.locs[pos], loc),
            })
        }
    } /
    pos:position!() [Union] w() id:raw_ident()? w() declarations:struct_body() {
        let (declarations, loc) = declarations;

        if let Some((ident, _)) = id {
            TypeSpecifier::Union(StructType {
                kind: StructTypeKind::NamedDecl {
                    ident,
                    declarations,
                },
                loc: l_from(env.locs[pos], loc),
            })
        } else {
            TypeSpecifier::Union(StructType {
                kind: StructTypeKind::UnnamedDecl {
                    declarations,
                },
                loc: l_from(env.locs[pos], loc),
            })
        }
    } /
    pos:position!() [Struct] w() id:raw_ident() {
        let (id, loc) = id;

        TypeSpecifier::Struct(StructType {
            kind: StructTypeKind::Named(id),
            loc: l_from(env.locs[pos], loc),
        })
    } /
    pos:position!() [Union] w() id:raw_ident() {
        let (id, loc) = id;

        TypeSpecifier::Union(StructType {
            kind: StructTypeKind::Named(id),
            loc: l_from(env.locs[pos], loc),
        })
    } /
    t:typedef_name() {
        let (t, loc) = t;
        TypeSpecifier::Ident(t)
    }


rule struct_body() -> (&'static [StructField], CodeLoc) =
    pos:position!() [LBrace] w() d:list0(<struct_field()>) w()
    pos2:position!() [RBrace] {
        let (d, _) = d;
        let d = env.buckets.add_array(d);

        (d, l_from(env.locs[pos], env.locs[pos2]))
    }

rule struct_field() -> StructField =
    s:specifier_qualifiers() w() d:cs0(<struct_declarator()>)
    pos2:position!() [Semicolon] {
        let (s, loc) = s;
        let (d, _) = d;
        StructField {
            specifiers: env.buckets.add_array(s),
            declarators: env.buckets.add_array(d),
            loc: l_from(loc, env.locs[pos2]),
        }
    }

rule struct_declarator() -> StructDeclarator =
    d:declarator() {
        StructDeclarator {
            declarator: d,
            loc: d.loc,
        }
    }

rule type_specifier_nonunique() -> TypeSpecifier =
    pos:position!() [Char] { TypeSpecifier::Char } /
    pos:position!() [Short] { TypeSpecifier::Short } /
    pos:position!() [Int] { TypeSpecifier::Int } /
    pos:position!() [Long] { TypeSpecifier::Long } /
    pos:position!() [Float] { TypeSpecifier::Float } /
    pos:position!() [Double] { TypeSpecifier::Double } /
    pos:position!() [Signed] { TypeSpecifier::Signed } /
    pos:position!() [Unsigned] { TypeSpecifier::Unsigned }

rule specifier_qualifiers() -> (Vec<SpecifierQualifier>, CodeLoc) =
    list_eq1_n(<specifier_qualifier_unique_type0()>, <specifier_qualifier_qualifier0()>) /
    list_ge1_n(<specifier_qualifier_nonunique_type0()>, <specifier_qualifier_qualifier0()>)

rule specifier_qualifier_unique_type0() -> SpecifierQualifier =
    pos:position!() s:type_specifier_unique() pos2:position!()
{
    SpecifierQualifier {
        kind: SpecifierQualifierKind::TypeSpecifier(s),
        loc: l_from(env.locs[pos], env.locs[pos2 - 1]),
    }
}

rule specifier_qualifier_nonunique_type0() -> SpecifierQualifier = pos:position!() s:type_specifier_nonunique()
{
    SpecifierQualifier {
        kind: SpecifierQualifierKind::TypeSpecifier(s),
        loc: env.locs[pos],
    }
}

rule specifier_qualifier_qualifier0() -> SpecifierQualifier = q:type_qualifier() {
    SpecifierQualifier {
        kind: SpecifierQualifierKind::TypeQualifier(q),
        loc: q.loc,
    }
}

rule type_qualifier() -> TypeQualifier =
    pos:position!() [Restrict] {
        TypeQualifier {
            kind: TypeQualifierKind::Restrict,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [Const] {
        TypeQualifier {
            kind: TypeQualifierKind::Const,
            loc: env.locs[pos],
        }
    } /
    pos:position!() [Volatile] {
        TypeQualifier {
            kind: TypeQualifierKind::Volatile,
            loc: env.locs[pos],
        }
    }

rule declarator() -> Declarator
    = pointer:list0(<pointer()>) w() decl:direct_declarator() w() derived:list0(<derived_declarator()>)
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

    decl.derived = env.buckets.add_array(concat(derived, pointer));
    decl.loc = loc;
    decl
}

rule direct_declarator() -> Declarator =
    pos:position!() i:raw_ident() {
        Declarator {
            kind: DeclaratorKind::Identifier(i.0),
            derived: &[],
            loc: i.1,
        }
    } /
    pos:position!() [LParen] w() d:declarator() w() pos2:position!() [RParen] {
        Declarator {
           kind: DeclaratorKind::Declarator(env.buckets.add(d)),
           derived: &[],
           loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    }

rule derived_declarator() -> DerivedDeclarator  =
    pos:position!() [LBracket] w() a:array_declarator() w() pos2:position!() [RBracket] {
        DerivedDeclarator {
            kind: DerivedDeclaratorKind::Array(a),
            loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    } /
    f:scoped(<function_declarator()>) {
        DerivedDeclarator {
            kind: DerivedDeclaratorKind::Function(f),
            loc: f.loc,
        }
    } /
    pos:position!() [LParen] w() pos2:position!() [RParen] {
        DerivedDeclarator {
            kind: DerivedDeclaratorKind::EmptyFunction,
            loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    }

rule array_declarator() -> ArrayDeclarator =
    q:list0(<type_qualifier()>) w() e:constant_expr() {
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
    } /
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
    }

rule function_declarator() -> FunctionDeclarator =
    pos:position!() [LParen] w() params:cs1(<parameter_declaration()>) w()
    varargs:([Comma] w() [DotDotDot] w())? pos2:position!() [RParen]
    {
        let (params, mut loc) = params;
        let varargs = varargs.is_some();
        loc = l_from(env.locs[pos], env.locs[pos2]);

        FunctionDeclarator {
            parameters: env.buckets.add_array(params),
            varargs,
            loc,
        }
    }

rule pointer() -> DerivedDeclarator = pos:position!() [Star] w() q:list0(<type_qualifier()>) {
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

rule pointer_quals() -> PointerQuals = pos:position!() [Star] w() q:list0(<type_qualifier()>) {
    let (q, mut end_loc) = q;
    let loc = env.locs[pos];
    if end_loc == NO_FILE {
        end_loc = loc;
    }

    let loc = l_from(loc, end_loc);
    PointerQuals {
        quals: env.buckets.add_array(q),
        loc,
    }
}


rule parameter_declaration() -> ParameterDeclaration = s:decl_specs() w() d:parameter_declarator()
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


rule type_name() -> TypeName = s:specifier_qualifiers() w() d:abstract_declarator()? {
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

rule function_specifier() -> DeclarationSpecifier = pos:position!() [Inline] {
    DeclarationSpecifier {
        kind: DeclarationSpecifierKind::Inline,
        loc: env.locs[pos],
    }
}

rule abstract_declarator() -> Declarator =
    p:list0(<pointer()>) w() k:direct_abstract_declarator() w() d:list0(<derived_abstract_declarator()>) {
        let (mut p, begin_loc) = p;
        let (d, end_loc) = d;
        let loc = l_from(begin_loc, k.loc);
        let loc = l_from(loc, end_loc);

        let mut declarator = k;
        declarator.loc = loc;
        declarator.derived = env.buckets.add_array(concat(d, p));
        declarator
    } /
    p:list0(<pointer()>) w() d:list1(<derived_abstract_declarator()>) {
        let (p, begin_loc) = p;
        let (d, end_loc) = d;
        let loc = l_from(begin_loc, end_loc);

        Declarator {
            kind: DeclaratorKind::Abstract,
            derived: env.buckets.add_array(concat(d, p)),
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
    pos:position!() [LParen] w() d:abstract_declarator() w()
    pos2:position!() [RParen]
{
    Declarator {
        kind: DeclaratorKind::Declarator(env.buckets.add(d)),
        derived: &[],
        loc: l_from(env.locs[pos], env.locs[pos2]),
    }
}

rule derived_abstract_declarator() -> DerivedDeclarator =
    pos:position!() [LBracket] w() a:abstract_array_declarator() w() pos2:position!() [RBracket] {
        DerivedDeclarator {
            kind: DerivedDeclaratorKind::Array(a),
            loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    } /
    pos:position!() [LParen] w() a:abstract_function_declarator() w() pos2:position!() [RParen] {
        DerivedDeclarator {
            kind: DerivedDeclaratorKind::Function(a),
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
    q:list0(<type_qualifier()>) w() e:assignment_expr() {
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

rule abstract_function_declarator() -> FunctionDeclarator =
    p:cs1(<parameter_declaration()>) w() pos:position!() varargs:([Comma] w() [DotDotDot])? {
        let (p, mut loc) = p;
        let varargs = varargs.is_some();
        if varargs {
            loc = l_from(loc, env.locs[pos + 1]);
        }

        FunctionDeclarator {
            parameters: env.buckets.add_array(p),
            varargs,
            loc,
        }
    } /
    pos:position!() {
        FunctionDeclarator {
            parameters: &[],
            varargs: false,
            loc: env.locs[pos],
        }
    }

rule typedef_name() -> (u32, CodeLoc) = quiet! { typedef_name0() } / expected!("<typedef_name>")

rule typedef_name0() -> (u32, CodeLoc) = i:raw_ident() {?
    if env.is_typename(i.0) {
        Ok(i)
    } else {
        Err("<unused>")
    }
}

rule initializer() -> Initializer =
    e:assignment_expr() {
        Initializer {
            kind: InitializerKind::Expr(env.buckets.add(e)),
            loc: e.loc,
        }
    } /
    pos:position!() [LBrace] w() i:cs1(<initializer_list_item()>) w()
    [Comma]? w() pos2:position!() [RBrace]
    {
        Initializer {
            kind: InitializerKind::List(env.buckets.add_array(i.0)),
            loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    }

rule initializer_list_item() -> Expr = assignment_expr()

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
    jump_statement() /
    pos:position!() [Semicolon] {
        let loc = env.locs[pos];
        Statement {
            kind: StatementKind::Block(Block { stmts: &[], loc }),
            loc,
        }
    }

////
// 6.8.1 Labeled statements
////

rule labeled_statement() -> Statement =
    i:raw_ident() w() [Colon] w() s:statement() {
        let (i, loc) = i;
        Statement {
            loc: l_from(loc, s.loc),
            kind: StatementKind::Labeled {
                label: i,
                label_loc: loc,
                labeled: env.buckets.add(s),
            }
        }
    } /
    pos:position!() [Case] w() i:assignment_expr() w() [Colon] w() s:statement() {
        Statement {
            loc: l_from(env.locs[pos], s.loc),
            kind: StatementKind::CaseLabeled {
                case_value: i,
                labeled: env.buckets.add(s),
            }
        }
    } /
    pos:position!() w() [Default] w() [Colon] w() s:statement() {
        Statement {
            loc: l_from(env.locs[pos], s.loc),
            kind: StatementKind::DefaultCaseLabeled(env.buckets.add(s))
        }
    }

////
// 6.8.2 Compound statement
////

rule compound_statement() -> Block =
    pos:position!() [LBrace] w() b:list0(<block_item()>) w() pos2:position!() [RBrace]
{
    let (block, loc) = b;

    Block{
        stmts: env.buckets.add_array(block),
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

rule expression_statement() -> Statement = e:expr() w() [Semicolon] {
    Statement {
        loc: e.loc,
        kind: StatementKind::Expr(e),
    }
}

////
// 6.8.4 Selection statement
////

rule selection_statement() -> Statement =
    pos:position!() [If] w() [LParen] w() e:expr()
    w() [RParen] w() a:statement() w() b:else_statement()?
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
    pos:position!() [Switch] w() [LParen] w() e:expr() w()
    [RParen] w() a:statement() {
        let mut loc = l_from(env.locs[pos], a.loc);

        Statement {
            kind: StatementKind::Switch {
                expr: e,
                body: env.buckets.add(a),
            },
            loc,
        }
    }

rule else_statement() -> Statement = [Else] w() s:statement() { s }

////
// 6.8.5 Iteration statement
////

rule iteration_statement() -> Statement =
    s:while_statement() { s } /
    s:do_while_statement() { s } /
    s:for_statement() { s }

rule while_statement() -> Statement =
    pos:position!() [While] w() [LParen] w() e:expr() w()
    [RParen] w() s:statement() {
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
    pos:position!() [Do] w() s:statement() w() [While] w()
    [LParen] w() e:expr() w() [RParen] w()
    pos2:position!() [Semicolon] {
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
    pos:position!() [For] w() [LParen] w() a:expr()? w()
    [Semicolon] w() b:expr()? w() [Semicolon] w() e:expr()?
    w() [RParen] w() s:statement() {
        let loc = l_from(env.locs[pos], s.loc);

        Statement {
            kind: StatementKind::For {
                at_start: a,
                condition: b,
                post_expr: e,
                body: env.buckets.add(s),
            },
            loc,
        }
    } /
    pos:position!() [For] w() [LParen] w() a:declaration()
    w() b:expr()? w() [Semicolon] w() c:expr()? w() [RParen]
    w() s:statement() {
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
    pos:position!() [Goto] w() i:raw_ident() w() pos2:position!() [Semicolon] {
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
    pos:position!() [Continue] w() pos2:position!() [Semicolon] {
        let loc = l_from(env.locs[pos], env.locs[pos2]);

        Statement {
            kind: StatementKind::Continue,
            loc
        }
    } /
    pos:position!() [Break] w() pos2:position!() [Semicolon] {
        let loc = l_from(env.locs[pos], env.locs[pos2]);

        Statement {
            kind: StatementKind::Break,
            loc
        }
    } /
    pos:position!() [Return] w() pos2:position!() [Semicolon] {
        let loc = l_from(env.locs[pos], env.locs[pos2]);

        Statement {
            kind: StatementKind::Ret,
            loc
        }
    } /
    pos:position!() [Return] w() e:expr() w() pos2:position!() [Semicolon] {
        let loc = l_from(env.locs[pos], env.locs[pos2]);

        Statement {
            kind: StatementKind::RetVal(e),
            loc
        }
    }

////
// 6.9 External definitions
////

pub rule translation_unit() -> Vec<GlobalStatement> = w() tu:(external_declaration() ** w()) w() {
    tu
}

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
    } /
    p:pragma() {
        let (pragma, loc) = p;

        GlobalStatement {
            kind: GlobalStatementKind::Pragma(pragma),
            loc,
        }
    }

rule function_definition() -> FunctionDefinition =
    a:decl_specs() w() pointer:list0(<pointer_quals()>) w() id:ident()
    w() f:function_declarator() w() d:compound_statement() {
        let (a, begin_loc) = a;
        let (ident, _) = id;
        let (pointer, _) = pointer;

        let loc = l_from(begin_loc, d.loc);
        FunctionDefinition {
            specifiers: env.buckets.add_array(a),
            pointer: env.buckets.add_array(pointer),
            ident,
            params: Some(f),
            statements: d,
            loc,
        }
    } /
    a:decl_specs() w() pointer:list0(<pointer_quals()>) w() id:ident() w()
    [LParen] w() [RParen] w() d:compound_statement() {
        let (a, begin_loc) = a;
        let (ident, _) = id;
        let (pointer, _) = pointer;

        let loc = l_from(begin_loc, d.loc);
        FunctionDefinition {
            specifiers: env.buckets.add_array(a),
            pointer: env.buckets.add_array(pointer),
            ident,
            params: None,
            statements: d,
            loc,
        }
    }

}
}
