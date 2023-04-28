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
    type_names: HashMap<Symbol, u32>,

    depth_tracker: &'a Cell<u32>,

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
    depth: u32,
    tracker: &'a Cell<u32>,
}

impl<'a> Drop for Depth<'a> {
    fn drop(&mut self) {
        self.tracker.set(self.depth);
    }
}

impl<'a> std::ops::Deref for Depth<'a> {
    type Target = u32;

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
}

pub fn parse(tokens: &TokenVec) -> Result<AstNodeVec, Error> {
    let depth = Cell::new(0);
    let mut parser = Parser {
        type_names: HashMap::new(),
        depth_tracker: &depth,
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

    if !parse_declaration_specifiers(p)? {
        unimplemented!("a global that's not a declaration");
    }

    parse_declarator(p)?;

    unimplemented!();
}

// false means that no parsing actually happened; the caller needs to determine
// if this means that an error has occured or we can just continue to the next
// rule.
fn parse_declaration_specifiers(p: &mut Parser) -> Result<bool, Error> {
    let depth = p.track_depth();
    unimplemented!()
}

fn parse_declarator(p: &mut Parser) -> Result<(), Error> {
    let depth = p.track_depth();
    unimplemented!()
}
