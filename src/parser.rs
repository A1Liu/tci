/*!
Parser for the C programming language.

I think the plan is to make it very very flexible,
so that basically anything that could be considered C code
will work (except for those goddamn K&R decls). Then,
most of the constraints placed upon the AST will be validated
in compiler passes.

Doing it this way is maybe a little bit bonkers, but I have no
experience writing compiler passes with this AST structure,
and at the very least I need the practice.
*/

use crate::api::*;
use std::cell::Cell;

struct ParserTracker {
    depth: Cell<u16>,
    pre_order: Cell<u32>,
}

struct Parser<'a> {
    // type_names: HashMap<Symbol, u32>,
    // TODO: use a global symbol hashmap for each type name
    // and then in NodeTracker, add stuff to count which types
    // are shadowed
    tracker: &'a ParserTracker,

    tokens: TokenSlice<'a>,
    index: usize,

    ast: AstNodeVec,
}

/// An RAII type to track Node depth in the tree
/// during parsing.
///
/// It can be "dereferenced" to get the depth value
/// in the current parsing function.
struct NodeTracker<'a> {
    start: u32,
    depth: u16,
    reserved_pre_order: Option<u32>,
    tracker: &'a ParserTracker,
}

impl<'a> Drop for NodeTracker<'a> {
    fn drop(&mut self) {
        self.tracker.depth.set(self.depth);
    }
}

impl<'a> NodeTracker<'a> {
    fn create_node(&mut self, kind: AstNodeKind) -> AstNode {
        let node = self;

        // You cannot call this more than once; maybe we should enforce that statically
        // by taking in self instead of &self?
        let pre_order = node.reserved_pre_order.take().unwrap();

        return AstNode {
            kind,
            start: node.start,
            depth: node.depth,
            pre_order,
            data: 0,
        };
    }
}

impl<'a> Parser<'a> {
    fn track_node(&mut self) -> NodeTracker<'a> {
        let depth = self.tracker.depth.get();
        self.tracker.depth.set(depth + 1);

        let pre_order = self.tracker.pre_order.get();
        self.tracker.pre_order.set(pre_order + 1);

        let start = self.tokens.start[self.index];

        return NodeTracker {
            start,
            depth,
            reserved_pre_order: Some(pre_order),
            tracker: self.tracker,
        };
    }

    fn peek_kind(&self) -> TokenKind {
        if self.index >= self.tokens.len() {
            return TokenKind::EOF;
        }

        return self.tokens.kind[self.index];
    }

    fn consume<T: Into<AstNodeKind>>(&mut self, node: &mut NodeTracker, kind: T) {
        self.index += 1;

        self.push(node, kind);
    }

    fn push<T: Into<AstNodeKind>>(&mut self, node: &mut NodeTracker, kind: T) {
        let ast_node = node.create_node(kind.into());
        self.ast.push(ast_node);
    }
}

pub fn parse(tokens: &TokenVec) -> Result<AstNodeVec, Error> {
    let tracker = ParserTracker {
        depth: Cell::new(0),
        pre_order: Cell::new(0),
    };
    let mut parser = Parser {
        tracker: &tracker,

        tokens: tokens.as_slice(),
        index: 0,

        ast: AstNodeVec::new(),
    };

    while parser.index < parser.tokens.len() {
        parse_global(&mut parser)?;
    }

    let ast = parser.ast;

    // TODO: Sort the AST

    return Ok(ast);
}

fn parse_global(p: &mut Parser) -> Result<(), Error> {
    if !parse_declaration(p, DeclarationKind::Variable)? {
        unimplemented!("a global that's not a declaration");
    };

    return Ok(());
}

enum DeclarationKind {
    Param,
    Variable,
}

fn parse_declaration(p: &mut Parser, kind: DeclarationKind) -> Result<bool, Error> {
    let node = &mut p.track_node();

    if !parse_declaration_specifier(p) {
        return Ok(false);
    };

    while parse_declaration_specifier(p) {}

    parse_declarator(p)?;

    if let DeclarationKind::Param = kind {
        p.push(node, AstDeclaration);
        return Ok(true);
    }

    if parse_block(p)? {
        p.push(node, AstFunctionDefinition);

        return Ok(true);
    }

    match p.peek_kind() {
        TokenKind::Comma | TokenKind::Semicolon => p.push(node, AstDeclaration),

        _ => panic!("bad character after declartor"),
    }

    while p.peek_kind() == TokenKind::Comma {
        p.index += 1;

        parse_declarator(p)?;
    }

    expect_semicolon(p)?;

    p.push(node, AstDeclaration);

    return Ok(true);
}

fn parse_declaration_specifier(p: &mut Parser) -> bool {
    let node = &mut p.track_node();

    let specifier = match p.peek_kind() {
        TokenKind::Char => ast::AstSpecifier::Char,
        TokenKind::Short => ast::AstSpecifier::Short,
        TokenKind::Int => ast::AstSpecifier::Int,
        TokenKind::Long => ast::AstSpecifier::Long,

        _ => return false,
    };

    p.consume(node, specifier);

    return true;
}

fn parse_declarator(p: &mut Parser) -> Result<(), Error> {
    let node = &mut p.track_node();

    while parse_star_declarator(p)? {}

    match p.peek_kind() {
        TokenKind::LParen if parse_func_declarator(p)? => p.push(node, AstDeclarator::Abstract),
        TokenKind::LParen => {
            p.consume(node, AstDeclarator::NestedWithChild);

            parse_declarator(p)?;

            if p.peek_kind() != TokenKind::RParen {
                panic!("nested declarator didn't have closing paren");
            }

            p.index += 1;
        }

        TokenKind::Ident => {
            println!("ident at depth: {}", node.depth);
            p.consume(node, AstDeclarator::Ident);
        }

        _ => p.push(node, AstDeclarator::Abstract),
    }

    loop {
        let found_array = parse_array_declarator(p)?;
        let found_func = parse_func_declarator(p)?;

        if !found_array && !found_func {
            break;
        }
    }

    return Ok(());
}

fn parse_star_declarator(p: &mut Parser) -> Result<bool, Error> {
    let node = &mut p.track_node();

    if p.peek_kind() != TokenKind::Star {
        return Ok(false);
    }

    p.consume(node, AstDerivedDeclarator::Pointer);

    while parse_declaration_specifier(p) {}

    return Ok(true);
}

fn parse_array_declarator(p: &mut Parser) -> Result<bool, Error> {
    let node = &mut p.track_node();

    return Ok(false);
}

fn parse_func_declarator(p: &mut Parser) -> Result<bool, Error> {
    let node = &mut p.track_node();

    if p.peek_kind() != TokenKind::LParen {
        return Ok(false);
    }

    // We honestly don't know yet whether or not this is a
    // func declarator; later on we need to then we might need to
    // backtrack a bit.
    p.index += 1;

    // We know it's a function declarator because `()` is illegal as an
    // abstract declarator
    if p.peek_kind() == TokenKind::RParen {
        p.consume(node, AstDerivedDeclarator::Function);
        return Ok(true);
    }

    if !parse_declaration(p, DeclarationKind::Param)? {
        // We didn't find a declaration; this means we need to backtrack to before
        // the parenthesis, and parse the next guy as an abstract declarator.
        p.index -= 1;

        return Ok(false);
    }

    while p.peek_kind() == TokenKind::Comma {
        p.index += 1;

        if !parse_declaration(p, DeclarationKind::Param)? {
            panic!("why not");
        }
    }

    if p.peek_kind() == TokenKind::DotDotDot {
        p.consume(node, AstDerivedDeclarator::FunctionElipsis);
    } else {
        p.push(node, AstDerivedDeclarator::Function);
    }

    if p.peek_kind() != TokenKind::RParen {
        println!("before: {:?}", p.tokens.kind[p.index - 1]);
        println!("after: {:?}", p.tokens.kind[p.index + 1]);
        panic!(
            "missing closing paren for func declarator (got {:?})",
            p.peek_kind()
        );
    }
    p.index += 1;

    return Ok(true);
}

fn parse_statement(p: &mut Parser) -> Result<(), Error> {
    match () {
        _ if parse_declaration(p, DeclarationKind::Variable)? => {}
        _ if parse_block(p)? => {}
        _ if parse_return(p)? => {}
        _ => parse_expr(p)?,
    }

    expect_semicolon(p)?;

    return Ok(());
}

fn parse_block(p: &mut Parser) -> Result<bool, Error> {
    let node = &mut p.track_node();

    if p.peek_kind() != TokenKind::LBrace {
        return Ok(false);
    }

    p.consume(node, AstStatement::Block);

    while p.peek_kind() == TokenKind::Semicolon {
        p.index += 1;
    }

    while p.peek_kind() != TokenKind::RBrace {
        parse_statement(p)?;
    }

    p.index += 1;

    return Ok(true);
}

fn parse_return(p: &mut Parser) -> Result<bool, Error> {
    let node = &mut p.track_node();

    match p.peek_kind() {
        TokenKind::Return => p.consume(node, AstStatement::Ret),
        _ => return Ok(false),
    }

    if p.peek_kind() != TokenKind::Semicolon {
        parse_expr(p)?;
    }

    return Ok(true);
}

fn parse_expr(p: &mut Parser) -> Result<(), Error> {
    return parse_precedence_climbing_expr(p, 0);
}

/// Handles commas, assignments, ternary, and binary expressions
fn parse_precedence_climbing_expr(p: &mut Parser, min_precedence: u8) -> Result<(), Error> {
    let node = &mut p.track_node();

    parse_prefix_expr(p)?;

    while let Some(info) = get_precedence(p.peek_kind()) {
        if info.precedence < min_precedence {
            break;
        }

        p.consume(node, info.op);

        if let AstExpr::Ternary = info.op {
            unimplemented!("TODO: special logic for middle part of ternary")
        }

        let mut next_min_precedence = info.precedence;
        if info.left_associative {
            next_min_precedence += 1;
        }

        parse_precedence_climbing_expr(p, next_min_precedence)?;
    }

    return Ok(());
}

struct PrecedenceInfo {
    left_associative: bool,
    precedence: u8,
    op: AstExpr,
}

fn get_precedence(t: TokenKind) -> Option<PrecedenceInfo> {
    use ast::{AstExpr::*, BinOp::*};
    let (left_associative, precedence, op) = match t {
        TokenKind::Star => (true, 100, BinOp(Mul)),
        TokenKind::Percent => (true, 100, BinOp(Mod)),
        TokenKind::Slash => (true, 100, BinOp(Div)),

        TokenKind::Plus => (true, 90, BinOp(Add)),
        TokenKind::Dash => (true, 90, BinOp(Sub)),

        TokenKind::LtLt => (true, 80, BinOp(LShift)),
        TokenKind::GtGt => (true, 80, BinOp(RShift)),

        TokenKind::Lt => (true, 70, BinOp(Lt)),
        TokenKind::Leq => (true, 70, BinOp(Leq)),
        TokenKind::Gt => (true, 70, BinOp(Gt)),
        TokenKind::Geq => (true, 70, BinOp(Geq)),

        TokenKind::EqEq => (true, 70, BinOp(Eq)),
        TokenKind::Neq => (true, 70, BinOp(Neq)),

        TokenKind::Amp => (true, 66, BinOp(BitAnd)),
        TokenKind::Caret => (true, 63, BinOp(BitXor)),
        TokenKind::Line => (true, 60, BinOp(BitOr)),

        TokenKind::AmpAmp => (true, 56, BinOp(BoolAnd)),
        TokenKind::LineLine => (true, 50, BinOp(BoolOr)),

        TokenKind::Question => (true, 50, Ternary),

        TokenKind::Eq => (true, 40, Assign),
        TokenKind::PlusEq => (true, 40, BinOpAssign(Add)),
        TokenKind::DashEq => (true, 40, BinOpAssign(Sub)),
        TokenKind::StarEq => (true, 40, BinOpAssign(Mul)),
        TokenKind::PercentEq => (true, 40, BinOpAssign(Mod)),
        TokenKind::SlashEq => (true, 40, BinOpAssign(Div)),
        TokenKind::LtLtEq => (true, 40, BinOpAssign(LShift)),
        TokenKind::GtGtEq => (true, 40, BinOpAssign(RShift)),
        TokenKind::AmpEq => (true, 40, BinOpAssign(BitAnd)),
        TokenKind::CaretEq => (true, 40, BinOpAssign(BitXor)),
        TokenKind::LineEq => (true, 40, BinOpAssign(BitOr)),

        TokenKind::Comma => (true, 40, BinOp(Comma)),

        _ => return None,
    };

    return Some(PrecedenceInfo {
        left_associative,
        precedence,
        op,
    });
}

fn parse_prefix_expr(p: &mut Parser) -> Result<(), Error> {
    let node = &mut p.track_node();

    return parse_postfix_expr(p);
}

fn parse_postfix_expr(p: &mut Parser) -> Result<(), Error> {
    let node = &mut p.track_node();

    return parse_atom_expr(p);
}

fn parse_atom_expr(p: &mut Parser) -> Result<(), Error> {
    let node = &mut p.track_node();

    let expr = match p.peek_kind() {
        TokenKind::Ident => {
            // TODO: Set data field

            AstExpr::Ident
        }

        // TODO: Remove this and replace with actually reasonable logic
        TokenKind::PreprocessingNum => AstExpr::IntLit,

        TokenKind::StringLit => AstExpr::StringLit,

        _ => panic!("OOOOOPS"),
    };

    p.consume(node, expr);

    return Ok(());
}

fn expect_semicolon(p: &mut Parser) -> Result<(), Error> {
    if p.peek_kind() != TokenKind::Semicolon {
        panic!("expected a semicolon");
    }

    while p.peek_kind() == TokenKind::Semicolon {
        p.index += 1;
    }

    return Ok(());
}
