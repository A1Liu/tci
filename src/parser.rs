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

    return Ok(parser.ast);
}

fn parse_global(p: &mut Parser) -> Result<(), Error> {
    let node = p.track_node();

    if !parse_declaration(p)? {
        unimplemented!("a global that's not a declaration");
    };

    return Ok(());
}

fn parse_declaration(p: &mut Parser) -> Result<bool, Error> {
    let node = &mut p.track_node();

    if !parse_declaration_specifier(p) {
        return Ok(false);
    };

    while parse_declaration_specifier(p) {}

    parse_declarator(p)?;

    if parse_block(p)? {
        p.consume(node, AstFunctionDefinition);

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

    unimplemented!()
}

fn parse_statement(p: &mut Parser) -> Result<(), Error> {
    match () {
        _ if parse_declaration(p)? => {}
        _ if parse_block(p)? => {}
        _ if parse_return(p)? => {}
        _ => parse_expr(p)?,
    }

    return Ok(());
}

fn parse_block(p: &mut Parser) -> Result<bool, Error> {
    let node = &mut p.track_node();

    unimplemented!()
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
    return parse_comma_expr(p);
}

fn parse_comma_expr(p: &mut Parser) -> Result<(), Error> {
    let node = &mut p.track_node();

    return parse_assign_expr(p);
}

fn parse_assign_expr(p: &mut Parser) -> Result<(), Error> {
    let node = &mut p.track_node();

    return parse_ternary_expr(p);
}

fn parse_ternary_expr(p: &mut Parser) -> Result<(), Error> {
    let node = &mut p.track_node();

    return parse_bin_expr(p);
}

fn parse_bin_expr(p: &mut Parser) -> Result<(), Error> {
    let node = &mut p.track_node();

    return parse_prefix_expr(p);
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

        _ => panic!("OOOOOPS"),
    };

    p.consume(node, expr);

    return Ok(());
}
