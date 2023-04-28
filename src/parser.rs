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

struct Parser<'a> {
    // type_names: HashMap<Symbol, u32>,
    depth_tracker: &'a Cell<u16>,
    pre_order_tracker: u32,

    tokens: TokenSlice<'a>,
    index: usize,

    ast: AstNodeVec,
}

/// An RAII type to track Node depth in the tree
/// during parsing.
///
/// It can be "dereferenced" to get the depth value
/// in the current parsing function.
struct Depth<'a> {
    depth: u16,
    tracker: &'a Cell<u16>,
}

impl<'a> Drop for Depth<'a> {
    fn drop(&mut self) {
        self.tracker.set(self.depth);
    }
}

impl<'a> std::ops::Deref for Depth<'a> {
    type Target = u16;

    fn deref(&self) -> &Self::Target {
        return &self.depth;
    }
}

impl<'a> Parser<'a> {
    fn track_depth(&self) -> Depth<'a> {
        let depth = self.depth_tracker.get();
        self.depth_tracker.set(depth + 1);

        return Depth {
            depth,
            tracker: self.depth_tracker,
        };
    }

    fn peek_kind(&self) -> TokenKind {
        return self.tokens.kind[self.index];
    }

    fn pop_to_node<T: Into<AstNodeKind>>(&mut self, kind: T) -> AstNode {
        let start = self.tokens.start[self.index];
        self.index += 1;

        let pre_order = self.pre_order_tracker;
        self.pre_order_tracker += 1;

        return AstNode {
            kind: kind.into(),
            id: pre_order,
            start,
            depth: self.depth_tracker.get() - 1,
            pre_order,
            data: 0,

            // This isn't used yet
            parent: 0,
        };
    }
}

pub fn parse(tokens: &TokenVec) -> Result<AstNodeVec, Error> {
    let depth = Cell::new(0);
    let mut parser = Parser {
        // type_names: HashMap::new(),
        depth_tracker: &depth,
        pre_order_tracker: 0,

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
    let depth = p.track_depth();

    let specs = match parse_declaration_specifiers(p) {
        Some(specs) => specs,
        None => unimplemented!("a global that's not a declaration"),
    };

    parse_declarator(p)?;

    unimplemented!();
}

fn parse_declaration_specifiers(p: &mut Parser) -> Option<Vec<AstNode>> {
    let depth = p.track_depth();

    let mut specifiers: Vec<AstNode> = Vec::new();
    // DeclarationSpecifiers::new();

    loop {
        // TODO: handle long int or etc
        match p.peek_kind() {
            TokenKind::Int => specifiers.push(p.pop_to_node(ast::AstSpecifier::Int)),

            _ => {
                if specifiers.len() == 0 {
                    return None;
                }

                return Some(specifiers);
            }
        }

        p.index += 1;
    }
}

fn parse_declarator(p: &mut Parser) -> Result<(), Error> {
    let depth = p.track_depth();

    unimplemented!()
}
