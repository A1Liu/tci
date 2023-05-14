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

use crate::{
    api::*,
    ast::{AstBlock, AstIdentExpr, AstParamDecl},
};
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

    scope_builder: ScopeBuilder,
    tokens: TokenSlice<'a>,
    index: usize,
}

/// An RAII type to track Node depth in the tree
/// during parsing.
///
/// It can be "dereferenced" to get the depth value
/// in the current parsing function.
struct NodeTracker {
    used: Option<()>,
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
            used: Some(()),
            start,
            children: Vec::new(),
            height: 0,
        };
    }

    fn track_node_from(&mut self, start: u32) -> NodeTracker {
        return NodeTracker {
            used: Some(()),
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

    fn start_index(&self) -> u32 {
        if self.index >= self.tokens.len() {
            return self.tokens.last().map(|f| *f.start).unwrap_or(0);
        }

        return *self.tokens.index(self.index).start;
    }

    fn push_data<T: Into<AstNodeKind> + AstInterpretData>(
        &mut self,
        node: &mut NodeTracker,
        kind: T,
        data: T::AstData,
    ) -> NodeResult {
        return self.push_data_raw(node, kind, data.into());
    }

    fn push<T: Into<AstNodeKind>>(&mut self, node: &mut NodeTracker, kind: T) -> NodeResult {
        return self.push_data_raw(node, kind, 0);
    }

    fn push_data_raw<T: Into<AstNodeKind>>(
        &mut self,
        node: &mut NodeTracker,
        kind: T,
        data: u64,
    ) -> NodeResult {
        if node.used.take().is_none() {
            panic!("used a node twice");
        }

        let mut tracker = self.tracker.replace(ParserTracker {
            ast: AstNodeVec::new(),
        });

        let post_order = tracker.ast.len() as u32;

        let ast_node = AstNode {
            id: post_order,
            kind: kind.into(),
            start: node.start,
            height: node.height,

            ty_id: TyId::Untyped,

            // Root nodes have themselves as their own parent.
            parent: post_order,

            post_order,
            data,
        };

        tracker.ast.push(ast_node);

        // Fix-up the parent fields of the children of this node.
        for &id in &node.children {
            tracker.ast.parent[id as usize] = post_order;
        }

        self.tracker.replace(tracker);

        return NodeResult {
            post_order,
            height: node.height,
        };
    }
}

pub fn parse(tokens: &TokenVec) -> Result<(AstNodeVec, Scopes), Error> {
    let tracker = Cell::new(ParserTracker {
        ast: AstNodeVec::new(),
    });

    let mut parser = Parser {
        tracker: &tracker,

        scope_builder: ScopeBuilder::new(),
        tokens: tokens.as_slice(),
        index: 0,
    };

    while parser.index < parser.tokens.len() {
        parse_global(&mut parser)?;
    }

    let scopes = parser.scope_builder.finish();

    return Ok((tracker.into_inner().ast, scopes));
}

fn parse_global(p: &mut Parser) -> Result<(), Error> {
    if let Some(res) = parse_declaration(p, DeclarationKind::Variable)? {
        return Ok(());
    };

    throw!(
        NotImplemented,
        "a global that's not a declaration",
        p.start_index()
    );
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
        return Ok(Some(p.push(node, AstParamDecl)));
    }

    if node.child_opt(parse_block(p)?) {
        return Ok(Some(p.push(node, AstFunctionDefinition)));
    }

    match p.peek_kind() {
        TokenKind::Comma | TokenKind::Semicolon => {}

        x => throw!(todo, "bad character after declarator", p.start_index()),
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
        TokenKind::Void => ast::AstSpecifier::Void,

        TokenKind::Char => ast::AstSpecifier::Char,
        TokenKind::Short => ast::AstSpecifier::Short,
        TokenKind::Int => ast::AstSpecifier::Int,
        TokenKind::Long => ast::AstSpecifier::Long,

        TokenKind::Unsigned => ast::AstSpecifier::Unsigned,
        TokenKind::Signed => ast::AstSpecifier::Signed,

        TokenKind::Float => ast::AstSpecifier::Float,
        TokenKind::Double => ast::AstSpecifier::Double,

        TokenKind::Static => ast::AstSpecifier::Static,
        TokenKind::Extern => ast::AstSpecifier::Extern,
        TokenKind::Register => ast::AstSpecifier::Register,
        TokenKind::Inline => ast::AstSpecifier::Inline,

        TokenKind::Volatile => ast::AstSpecifier::Volatile,
        TokenKind::Const => ast::AstSpecifier::Const,
        TokenKind::Restrict => ast::AstSpecifier::Restrict,
        TokenKind::Typedef => ast::AstSpecifier::Typedef,

        _ => return None,
    };

    p.index += 1;
    return Some(p.push(node, specifier));
}

fn parse_declarator(p: &mut Parser) -> Result<NodeResult, Error> {
    let node = &mut p.track_node();

    // Make declarators recursive
    if p.peek_kind() == TokenKind::Star {
        p.index += 1;

        while node.child_opt(parse_declaration_specifier(p)) {}

        node.child(parse_declarator(p)?);

        return Ok(p.push(node, AstDerivedDeclarator::Pointer));
    }

    return parse_postfix_declarator(p);
}

fn parse_postfix_declarator(p: &mut Parser) -> Result<NodeResult, Error> {
    let mut child = 'child: {
        if let Some(child) = parse_func_declarator(p, FuncDeclKind::CreateAbstract)? {
            break 'child child;
        }

        let node = &mut p.track_node();
        match p.peek_kind() {
            TokenKind::LParen => {
                p.index += 1;

                let child = parse_declarator(p)?;

                if p.peek_kind() != TokenKind::RParen {
                    throw!(
                        todo,
                        "nested declarator didn't have closing paren",
                        p.start_index()
                    );
                }

                p.index += 1;

                child
            }

            TokenKind::Ident => {
                let data: Symbol = p.tokens.data[p.index].into();
                p.index += 1;

                let res = p.push_data(node, AstDeclarator::Ident, data);
                p.scope_builder.add_name(res.post_order, data);

                res
            }

            _ => p.push_data(node, AstDeclarator::Abstract, Symbol::NullSymbol),
        }
    };

    loop {
        if let Some(node) = parse_func_declarator(p, FuncDeclKind::Child(child))? {
            child = node;
            continue;
        }

        if let Some(node) = parse_array_declarator(p, child)? {
            child = node;
            continue;
        }

        break;
    }

    return Ok(child);
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

fn parse_array_declarator(p: &mut Parser, child: NodeResult) -> Result<Option<NodeResult>, Error> {
    let node = &mut p.track_node();

    match p.peek_kind() {
        TokenKind::LBracket => p.index += 1,
        _ => return Ok(None),
    }

    while node.child_opt(parse_declaration_specifier(p)) {}
    node.child(child);

    let kind = match p.peek_kind() {
        TokenKind::RBracket => {
            p.index += 1;
            AstDerivedDeclarator::ArrayUnknown
        }
        _ => throw!(
            NotImplemented,
            "haven't implemented array declarators fully yet",
            node.start
        ),
    };

    return Ok(Some(p.push(node, kind)));
}

enum FuncDeclKind {
    Child(NodeResult),
    CreateAbstract,
}

fn parse_func_declarator(p: &mut Parser, kind: FuncDeclKind) -> Result<Option<NodeResult>, Error> {
    let node = &mut p.track_node();
    let abstract_decl_node = &mut p.track_node();

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

        let child = match kind {
            FuncDeclKind::Child(c) => c,
            FuncDeclKind::CreateAbstract => p.push_data(
                abstract_decl_node,
                AstDeclarator::Abstract,
                Symbol::NullSymbol,
            ),
        };
        node.child(child);

        return Ok(Some(p.push(node, AstDerivedDeclarator::Function)));
    }

    if !node.child_opt(parse_declaration(p, DeclarationKind::Param)?) {
        // We didn't find a declaration; this means we need to backtrack to before
        // the parenthesis, and parse the next guy as an abstract declarator.
        p.index -= 1;

        return Ok(None);
    }

    let child = match kind {
        FuncDeclKind::Child(c) => c,
        FuncDeclKind::CreateAbstract => p.push_data(
            abstract_decl_node,
            AstDeclarator::Abstract,
            Symbol::NullSymbol,
        ),
    };
    node.child(child);

    while p.peek_kind() == TokenKind::Comma {
        p.index += 1;

        if !node.child_opt(parse_declaration(p, DeclarationKind::Param)?) {
            p.index -= 1;
            throw!(
                todo,
                "parameter list has extra comma at the end",
                p.start_index()
            );
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
        throw!(
            todo,
            "missing closing paren for func declarator",
            p.start_index()
        );
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

    let scope = p.scope_builder.add_scope();

    p.index += 1;

    while p.peek_kind() == TokenKind::Semicolon {
        p.index += 1;
    }

    while p.peek_kind() != TokenKind::RBrace {
        node.child(parse_statement(p)?);
    }

    p.index += 1;
    let res = p.push_data(node, AstBlock, scope);
    p.scope_builder.end_scope(res);

    return Ok(Some(res));
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

    let kind = p.peek_kind();
    let data = &p.tokens.data[p.index];
    p.index += 1;

    let expr = match kind {
        TokenKind::Ident => {
            return Ok(p.push_data(node, AstIdentExpr, (*data).into()));
        }

        // TODO: Remove this and replace with actually reasonable logic
        TokenKind::PreprocessingNum => {
            return Ok(p.push_data(node, ast::AstIntLit::U64, *data));
        }

        TokenKind::StringLit => AstExpr::StringLit,

        _ => throw!(todo, "unrecognized atom token", node.start),
    };

    return Ok(p.push(node, expr));
}

fn expect_semicolon(p: &mut Parser) -> Result<(), Error> {
    if p.peek_kind() != TokenKind::Semicolon {
        throw!(todo, "expected a semicolon", p.start_index());
    }

    while p.peek_kind() == TokenKind::Semicolon {
        p.index += 1;
    }

    return Ok(());
}

#[derive(Clone, Default)]
pub struct Scopes {
    // This data structure maybe doesn't need to be a btree? I don't like that
    // It can't be deallocated in one shot when compilation ends, but there is
    // some argument you could make that its unnecessary to get that behavior.
    // I think the size of this structure might not be so big, so maybe its fine.
    pub scopes: BTreeMap<Scope, ScopeInfo>,
    pub declarators: BTreeMap<u32, Scope>,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Scope(u32);

impl Into<u64> for Scope {
    fn into(self) -> u64 {
        self.0 as u64
    }
}

impl From<u64> for Scope {
    fn from(value: u64) -> Self {
        Self(value as u32)
    }
}

#[derive(Clone)]
pub struct ScopeInfo {
    pub node_id: u32,

    // values reference declarators, which are stored in the declarators field of `Scopes`
    pub symbols: BTreeMap<Symbol, u32>,
    pub duplicates: Vec<(u32, Symbol)>,
    pub parent: Scope,
}

struct ScopeBuilder {
    scopes: Scopes,
    current_scope: Scope,
    next_scope_id: u32,
}

const GLOBAL_SCOPE: Scope = Scope(!0);

impl ScopeBuilder {
    fn new() -> Self {
        let mut scopes = Scopes::default();
        scopes.scopes.insert(
            GLOBAL_SCOPE,
            ScopeInfo {
                node_id: !0, // What's supposed to be here?
                symbols: BTreeMap::new(),
                duplicates: Vec::new(),
                parent: GLOBAL_SCOPE,
            },
        );

        return Self {
            scopes,
            next_scope_id: 0,
            current_scope: GLOBAL_SCOPE,
        };
    }

    fn add_scope(&mut self) -> Scope {
        let scope = Scope(self.next_scope_id);
        self.next_scope_id += 1;

        let prev = self.scopes.scopes.insert(
            scope,
            ScopeInfo {
                node_id: !0,
                symbols: BTreeMap::new(),
                duplicates: Vec::new(),
                parent: self.current_scope,
            },
        );

        debug_assert!(prev.is_none(), "found duplicate scope");

        return scope;
    }

    fn end_scope(&mut self, res: NodeResult) {
        let info = self.scopes.scopes.get_mut(&self.current_scope).unwrap();
        info.node_id = res.post_order;
        self.current_scope = info.parent;
    }

    fn finish(self) -> Scopes {
        return self.scopes;
    }

    fn add_name(&mut self, id: u32, symbol: Symbol) {
        let info = self.scopes.scopes.get_mut(&self.current_scope).unwrap();
        if let Some(prev) = info.symbols.insert(symbol, id) {
            // This is probably invalid code, because a symbol was re-declared in the same scope.
            // However, we don't *know* that for certain yet. We also don't really care yet.
            info.duplicates.push((prev, symbol));
        }
    }
}
