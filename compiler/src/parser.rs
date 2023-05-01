/*!
Parser for the C programming language.

Outputs nodes in Post-fix order. Outputted parent indices
refer to the post-fix order index.

The output isn't fully validated, so various strange combinations are
possible, like `static typedef *a() {}`. The goal here is to be
as flexible and simple as possible, and then add validation passes
later.
*/

// I think the plan is to make it very very flexible,
// so that basically anything that could be considered C code
// will work (except for those goddamn K&R decls). Then,
// most of the constraints placed upon the AST will be validated
// in compiler passes.
//
// Doing it this way is maybe a little bit bonkers, but I have no
// experience writing compiler passes with this AST structure,
// and at the very least I need the practice.

use crate::api::*;
use std::cell::Cell;

struct ParserTracker {
    ast: AstNodeVec,
}

struct Parser<'a> {
    // type_names: HashMap<Symbol, u32>,
    // TODO: use a global symbol hashmap for each type name
    // and then in NodeTracker, add stuff to count which types
    // are shadowed
    tracker: &'a Cell<ParserTracker>,

    tokens: TokenSlice<'a>,
    index: usize,
}

/// An RAII type to track Node depth in the tree
/// during parsing.
///
/// It can be "dereferenced" to get the depth value
/// in the current parsing function.
struct NodeTracker {
    used: bool,
    children: Vec<u32>,
    start: u32,
    height: u16,
}

#[derive(Clone, Copy)]
struct NodeResult {
    post_order: u32,
    height: u16,
}

impl NodeTracker {
    fn child(&mut self, res: NodeResult) {
        self.height = core::cmp::max(self.height, res.height + 1);
        self.children.push(res.post_order);
    }

    fn child_opt(&mut self, res: Option<NodeResult>) -> bool {
        if let Some(res) = res {
            self.child(res);
            return true;
        }

        return false;
    }
}

impl<'a> Parser<'a> {
    fn track_node(&mut self) -> NodeTracker {
        let start = self.tokens.start[self.index];

        return NodeTracker {
            used: false,
            start,
            children: Vec::new(),
            height: 0,
        };
    }

    fn track_node_from(&mut self, start: u32) -> NodeTracker {
        return NodeTracker {
            used: false,
            start,
            children: Vec::new(),
            height: 0,
        };
    }

    fn peek_kind(&self) -> TokenKind {
        if self.index >= self.tokens.len() {
            return TokenKind::EOF;
        }

        return self.tokens.kind[self.index];
    }

    fn push<T: Into<AstNodeKind>>(&mut self, node: &mut NodeTracker, kind: T) -> NodeResult {
        if node.used {
            panic!("used a node twice");
        }

        node.used = true;

        let mut tracker = self.tracker.replace(ParserTracker {
            ast: AstNodeVec::new(),
        });

        let post_order = tracker.ast.len() as u32;

        let ast_node = AstNode {
            kind: kind.into(),
            start: node.start,
            height: node.height,

            // Root nodes have themselves as their own parent.
            parent: post_order,

            post_order,
            data: 0,
        };

        tracker.ast.push(ast_node);

        // Fix-up the parent fields of the children of this node.
        for id in &node.children {
            tracker.ast.parent[*id as usize] = post_order;
        }

        self.tracker.replace(tracker);

        return NodeResult {
            post_order,
            height: node.height,
        };
    }
}

pub fn parse(tokens: &TokenVec) -> Result<AstNodeVec, Error> {
    let tracker = Cell::new(ParserTracker {
        ast: AstNodeVec::new(),
    });

    let mut parser = Parser {
        tracker: &tracker,

        tokens: tokens.as_slice(),
        index: 0,
    };

    while parser.index < parser.tokens.len() {
        parse_global(&mut parser)?;
    }

    return Ok(tracker.into_inner().ast);
}

fn parse_global(p: &mut Parser) -> Result<(), Error> {
    if let Some(res) = parse_declaration(p, DeclarationKind::Variable)? {
        return Ok(());
    };

    unimplemented!("a global that's not a declaration");
}

enum DeclarationKind {
    Param,
    Variable,
}

fn parse_declaration(p: &mut Parser, kind: DeclarationKind) -> Result<Option<NodeResult>, Error> {
    let node = &mut p.track_node();

    if !node.child_opt(parse_declaration_specifier(p)) {
        return Ok(None);
    };

    while node.child_opt(parse_declaration_specifier(p)) {}

    node.child(parse_declarator(p)?);

    if let DeclarationKind::Param = kind {
        return Ok(Some(p.push(node, AstDeclaration)));
    }

    if node.child_opt(parse_block(p)?) {
        return Ok(Some(p.push(node, AstFunctionDefinition)));
    }

    match p.peek_kind() {
        TokenKind::Comma | TokenKind::Semicolon => {}

        x => return Err(Error::Todo("bad character after declartor")),
    }

    while p.peek_kind() == TokenKind::Comma {
        p.index += 1;

        node.child(parse_declarator(p)?);
    }

    expect_semicolon(p)?;

    return Ok(Some(p.push(node, AstDeclaration)));
}

fn parse_declaration_specifier(p: &mut Parser) -> Option<NodeResult> {
    let node = &mut p.track_node();

    let specifier = match p.peek_kind() {
        TokenKind::Char => ast::AstSpecifier::Char,
        TokenKind::Short => ast::AstSpecifier::Short,
        TokenKind::Int => ast::AstSpecifier::Int,
        TokenKind::Long => ast::AstSpecifier::Long,

        _ => return None,
    };

    p.index += 1;
    return Some(p.push(node, specifier));
}

fn parse_declarator(p: &mut Parser) -> Result<NodeResult, Error> {
    let node = &mut p.track_node();

    while node.child_opt(parse_star_declarator(p)?) {}

    let kind = match p.peek_kind() {
        TokenKind::LParen if node.child_opt(parse_func_declarator(p)?) => AstDeclarator::Abstract,
        TokenKind::LParen => {
            p.index += 1;

            node.child(parse_declarator(p)?);

            if p.peek_kind() != TokenKind::RParen {
                return Err(Error::Todo("nested declarator didn't have closing paren"));
            }

            p.index += 1;

            AstDeclarator::NestedWithChild
        }

        TokenKind::Ident => {
            p.index += 1;
            AstDeclarator::Ident
        }

        _ => AstDeclarator::Abstract,
    };

    loop {
        let found_array = node.child_opt(parse_array_declarator(p)?);
        let found_func = node.child_opt(parse_func_declarator(p)?);

        if !found_array && !found_func {
            break;
        }
    }

    return Ok(p.push(node, kind));
}

fn parse_star_declarator(p: &mut Parser) -> Result<Option<NodeResult>, Error> {
    let node = &mut p.track_node();

    if p.peek_kind() != TokenKind::Star {
        return Ok(None);
    }

    p.index += 1;

    while node.child_opt(parse_declaration_specifier(p)) {}

    return Ok(Some(p.push(node, AstDerivedDeclarator::Pointer)));
}

fn parse_array_declarator(p: &mut Parser) -> Result<Option<NodeResult>, Error> {
    let node = &mut p.track_node();

    return Ok(None);
}

fn parse_func_declarator(p: &mut Parser) -> Result<Option<NodeResult>, Error> {
    let node = &mut p.track_node();

    if p.peek_kind() != TokenKind::LParen {
        return Ok(None);
    }

    // We honestly don't know yet whether or not this is a
    // func declarator; later on we need to then we might need to
    // backtrack a bit.
    p.index += 1;

    // We know it's a function declarator because `()` is illegal as an
    // abstract declarator
    if p.peek_kind() == TokenKind::RParen {
        p.index += 1;
        return Ok(Some(p.push(node, AstDerivedDeclarator::Function)));
    }

    if !node.child_opt(parse_declaration(p, DeclarationKind::Param)?) {
        // We didn't find a declaration; this means we need to backtrack to before
        // the parenthesis, and parse the next guy as an abstract declarator.
        p.index -= 1;

        return Ok(None);
    }

    while p.peek_kind() == TokenKind::Comma {
        p.index += 1;

        if !node.child_opt(parse_declaration(p, DeclarationKind::Param)?) {
            return Err(Error::Todo("parameter list has extra comma at the end"));
        }
    }

    let kind = match p.peek_kind() {
        TokenKind::DotDotDot => {
            p.index += 1;
            AstDerivedDeclarator::FunctionElipsis
        }
        _ => AstDerivedDeclarator::Function,
    };

    if p.peek_kind() != TokenKind::RParen {
        return Err(Error::Todo("missing closing paren for func declarator"));
    }

    p.index += 1;

    return Ok(Some(p.push(node, kind)));
}

fn parse_statement(p: &mut Parser) -> Result<NodeResult, Error> {
    let node = &mut p.track_node();

    let res = 'stmt: {
        if let Some(res) = parse_declaration(p, DeclarationKind::Variable)? {
            break 'stmt res;
        }

        if let Some(res) = parse_block(p)? {
            break 'stmt res;
        }

        if let Some(res) = parse_return(p)? {
            break 'stmt res;
        }

        break 'stmt parse_expr(p)?;
    };

    expect_semicolon(p)?;

    return Ok(res);
}

fn parse_block(p: &mut Parser) -> Result<Option<NodeResult>, Error> {
    let node = &mut p.track_node();

    if p.peek_kind() != TokenKind::LBrace {
        return Ok(None);
    }

    p.index += 1;

    while p.peek_kind() == TokenKind::Semicolon {
        p.index += 1;
    }

    while p.peek_kind() != TokenKind::RBrace {
        node.child(parse_statement(p)?);
    }

    p.index += 1;
    return Ok(Some(p.push(node, AstStatement::Block)));
}

fn parse_return(p: &mut Parser) -> Result<Option<NodeResult>, Error> {
    let node = &mut p.track_node();

    match p.peek_kind() {
        TokenKind::Return => p.index += 1,
        _ => return Ok(None),
    }

    if p.peek_kind() != TokenKind::Semicolon {
        node.child(parse_expr(p)?);
    }

    return Ok(Some(p.push(node, AstStatement::Ret)));
}

fn parse_expr(p: &mut Parser) -> Result<NodeResult, Error> {
    return parse_precedence_climbing_expr(p, 0);
}

/// Handles commas, assignments, ternary, and binary expressions
fn parse_precedence_climbing_expr(p: &mut Parser, min_precedence: u8) -> Result<NodeResult, Error> {
    let node = &mut p.track_node();
    let mut res = parse_prefix_expr(p)?;

    node.child(res);

    while let Some(info) = get_precedence(p.peek_kind()) {
        if info.precedence < min_precedence {
            break;
        }

        p.index += 1;

        if let AstExpr::Ternary = info.op {
            unimplemented!("TODO: special logic for middle part of ternary")
        }

        let mut next_min_precedence = info.precedence;
        if info.left_associative {
            next_min_precedence += 1;
        }

        node.child(parse_precedence_climbing_expr(p, next_min_precedence)?);

        res = p.push(node, info.op);
        *node = p.track_node_from(node.start);
        node.child(res);
    }

    return Ok(res);
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

fn parse_prefix_expr(p: &mut Parser) -> Result<NodeResult, Error> {
    let node = &mut p.track_node();

    return parse_postfix_expr(p);
}

fn parse_postfix_expr(p: &mut Parser) -> Result<NodeResult, Error> {
    let node = &mut p.track_node();

    return parse_atom_expr(p);
}

fn parse_atom_expr(p: &mut Parser) -> Result<NodeResult, Error> {
    let node = &mut p.track_node();

    let expr = match p.peek_kind() {
        TokenKind::Ident => {
            // TODO: Set data field

            AstExpr::Ident
        }

        // TODO: Remove this and replace with actually reasonable logic
        TokenKind::PreprocessingNum => AstExpr::IntLit,

        TokenKind::StringLit => AstExpr::StringLit,

        _ => return Err(Error::Todo("unrecognized atom token")),
    };

    p.index += 1;
    return Ok(p.push(node, expr));
}

fn expect_semicolon(p: &mut Parser) -> Result<(), Error> {
    if p.peek_kind() != TokenKind::Semicolon {
        return Err(Error::Todo("expected a semicolon"));
    }

    while p.peek_kind() == TokenKind::Semicolon {
        p.index += 1;
    }

    return Ok(());
}
