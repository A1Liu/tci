use crate::buckets::*;
use crate::lexer::*;
use crate::new_ast::*;
use crate::util::*;
use std::collections::HashMap;

pub struct ParseEnv {
    pub symbol_is_type: Vec<HashMap<u32, bool>>, // true is type
    pub locs: Vec<CodeLoc>,
    pub buckets_begin: BucketListRef<'static>,
    pub buckets: BucketListRef<'static>,
}

impl ParseEnv {
    pub fn new(locs: &[Token]) -> Self {
        let buckets = BucketList::new();

        Self {
            symbol_is_type: vec![HashMap::new()],
            locs: locs.iter().map(|tok| tok.loc).collect(),
            buckets_begin: buckets,
            buckets,
        }
    }

    pub fn is_typename(&self, ident: u32) -> bool {
        for scope in self.symbol_is_type.iter().rev() {
            if let Some(symbol) = scope.get(&ident) {
                return *symbol;
            }
        }
        false
    }

    pub fn handle_declarator(&mut self, mut declarator: &Declarator, is_type: bool) {
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

    pub fn add_symbol(&mut self, sym: u32, is_type: bool) {
        let scope_o = self.symbol_is_type.last_mut();
        let scope = scope_o.expect("at least one scope should be always present");
        scope.insert(sym, is_type);
    }
}

peg::parser! {

// Translated from https://github.com/vickenty/lang-c/blob/master/grammar.rustpeg
pub grammar c_parser(env: &ParseEnv) for [TokenKind<'static>] {

rule list0<E>(x: rule<E>) -> Vec<E> = v:(x()*) {
    v
}

rule list1<E>(x: rule<E>) -> Vec<E> = v:(x()+) {
    v
}

rule cs0<E>(x: rule<E>) -> Vec<E> = v:(x() ** [TokenKind::Comma]) {
    v
}

rule cs1<E>(x: rule<E>) -> Vec<E> = v:(x() ** [TokenKind::Comma]) [TokenKind::Comma] last:x() {
    let mut v = v;
    v.push(last);
    v
}

rule list_010<E>(b: rule<E>, s: rule<E>, a: rule<E>) -> Vec<E> =
    before:list0(<b()>) single:s() after:list0(<a()>)
{
    let mut before = before;
    before.push(single);
    before.extend(after);
    before
}

// A list containing *exactly* one element of a, and any of b.
rule list_eq1_n<E>(a: rule<E>, b: rule<E>) -> Vec<E> = v:list_010(<b()>,<a()>, <b()>)

// A list containing *at least* one element of a, and any of b.
rule list_ge1_n<E>(a: rule<E>, b: rule<E>) -> Vec<E> =  v:list_010(<b()>, <a()>, <a() / b()>)

rule ident() -> u32 = n:$[TokenKind::Ident(_)] {
    match n[0] {
        TokenKind::Ident(n) => n,
        _ => unreachable!(),
    }
}

rule int() -> i32 = n:$[TokenKind::IntLiteral(_)] {
    match n[0] {
        TokenKind::IntLiteral(n) => n,
        _ => unreachable!(),
    }
}

rule string() -> &'static str = n:$([TokenKind::StringLiteral(_)]+) {
    let mut string = String::new();
    for token in n {
        let s = match token {
            TokenKind::StringLiteral(s) => s,
            _ => unreachable!(),
        };

        string.push_str(s);
    }

    env.buckets.add_str(&string)
}

rule atom() -> Expr<'static> =
    pos:position!() n:int() {
        Expr {
            kind: ExprKind::IntLiteral(n),
            loc: env.locs[pos],
        }
    } /
    pos:position!() n:ident() {
        Expr {
            kind: ExprKind::Ident(n),
            loc: env.locs[pos],
        }
    } /
    pos:position!() n:string() {
        Expr {
            kind: ExprKind::StringLiteral(n),
            loc: env.locs[pos],
        }
    } /
    pos:position!() [TokenKind::LParen] v:(expr() ** [TokenKind::Comma]) pos2:position!() [TokenKind::RParen] {
        Expr {
            kind: ExprKind::ParenList(env.buckets.add_array(v)),
            loc: l_from(env.locs[pos], env.locs[pos2]),
        }
    }

rule expr() -> Expr<'static> = precedence! {
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

}
}
