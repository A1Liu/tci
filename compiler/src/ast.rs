/*!
This module describes the AST created by the parser.
*/

use crate::api::*;

pub trait AstInterpretData {
    type AstData: From<u64> + Into<u64>;
}

#[derive(Debug, Clone, Copy, StructOfArray, serde::Serialize, serde::Deserialize)]
pub struct AstNode {
    pub id: u32,
    pub kind: AstNodeKind,
    pub height: u16,

    #[serde(skip)]
    pub start: u32,

    #[serde(skip)]
    pub ty_id: TyId,

    pub data: u64,

    /// refers to the post_order index of the node that's the parent
    /// of this one.
    pub parent: u32,

    /// The post-order index of this node. The parser returns nodes in post-order,
    /// so these will also be the index in the AST after parsing.
    pub post_order: u32,
}

impl PartialEq for AstNode {
    fn eq(&self, other: &Self) -> bool {
        return self.kind == other.kind
            && self.height == other.height
            && self.data == other.data
            && self.parent == other.parent
            && self.post_order == other.post_order;
    }
}

macro_attr! {
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, EnumFromInner!)]
#[serde(tag = "kind", content = "data")]
pub enum AstNodeKind {
    Expr(AstExpr),
    Statement(AstStatement),
    DerivedDeclarator(AstDerivedDeclarator),
    Declarator(AstDeclarator),
    Specifier(AstSpecifier),
    Declaration(AstDeclaration),
    ParamDecl(AstParamDecl),
    FunctionDefinition(AstFunctionDefinition),

    // TODO: maybe we wanna delete stuff from the AST later
    // Nop(()),
}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AstExpr {
    IntLit(AstIntLit),   // data: i32
    FloatLit,            // data: f32
    DoubleLit,           // data: f64
    CharLit,             // data: i8
    StringLit,           // data: end of string text
    Ident(AstIdentExpr), // data: Symbol
    Assign,              // children: expression being assigned to, expression being assigned
    SizeofExpr,          // children: expression that's being queried
    SizeofTy,            // children: type that's being queried
    Cast,                // children: expression being cased and type being cast to
    Member,              // data: field name ; children: base of expression
    PtrMember,           // data: field name ; children: base of expression
    Call,                // data: id of function parameter, children: function and children
    Ternary, // data: id of condition parameter, children: condition, if_true, and if_false

    UnaryOp(UnaryOp),   // children: expression that's operated on
    BinOp(BinOp),       // children: operands
    BinOpAssign(BinOp), // children: expression being assigned to, expression being assigned
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AstIntLit {
    S8,
    S16,
    S32,
    S64,
    U8,
    U16,
    U32,
    U64,
}

impl AstInterpretData for AstIntLit {
    type AstData = u64;
}

impl Into<AstNodeKind> for AstIntLit {
    fn into(self) -> AstNodeKind {
        return AstNodeKind::Expr(AstExpr::IntLit(self));
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct AstIdentExpr;

impl AstInterpretData for AstIdentExpr {
    type AstData = Symbol;
}

impl Into<AstNodeKind> for AstIdentExpr {
    fn into(self) -> AstNodeKind {
        return AstNodeKind::Expr(AstExpr::Ident(self));
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Index,
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
    LShift,
    RShift,
    BitAnd,
    BitXor,
    BitOr,
    BoolAnd,
    BoolOr,

    Comma,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy, PartialOrd, Ord, Serialize, Deserialize)]
pub enum UnaryOp {
    Neg,
    BoolNot,
    BitNot,
    PostIncr,
    PostDecr,
    PreIncr,
    PreDecr,
    Deref,
    Ref,
}

/// Handles struct and union declarations:
///
/// ```text
/// struct a { int b; }
/// ```
///
/// In the above, it would have children for each field
/// declaration, and a child for the identifier as well.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum StructDeclaration {
    Struct,
    Union,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AstStatement {
    Labeled,            // data: label ; children: statement that is being labelled
    CaseLabeled,        // children: case value expression, statement that is being labelled
    DefaultCaseLabeled, // children: statement that is being labelled
    Goto,               // data: label ; children: statement that is being labelled
    Expr,               // children: expression
    Branch,             // children: condition, if_true body, if_false body
    Block,              // children: statements; maybe this is unnecessary
    For,                // children: start expression, condition, post expression, body
    ForDecl,            // children: declaration, condition, post expression, body
    While,              // children: condition expression, body
    DoWhile,            // children: body, condition expression
    Switch,             // children: switch expression, body
    Ret,                // children: optional expression to return
    Break,
    Continue,
}

/// A derived declarator. This is the `*const` part of
/// `int *const a`, or the `[3]` part of `int b[3]`
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AstDerivedDeclarator {
    Pointer,

    /// []
    ArrayUnknown,
    /// `[*]`
    ArrayVariableUnknown,
    /// `[10]`
    /// Children: size expression
    ArrayVariableExpression,
    /// `[static 10]`
    /// Children: size expression
    ArrayStaticExpression,

    /// x(int param1, char, long param3)
    /// children: AstParameterDeclaration*
    Function,

    /// x(int param1, char, long param3, ...)
    /// children: AstParameterDeclarators
    FunctionElipsis,
}

/// children: a AstDerivedDeclarator for each derived declarator
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AstDeclarator {
    Abstract,
    /// data: Symbol
    Ident,
}

// NOTE: This should probably not be a node, and instead should be a data field.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AstSpecifier {
    Extern,
    Static,
    Typedef,
    Register,
    Inline,
    Noreturn,

    Const,
    Volatile,
    Restrict,
    Atomic,

    Void,
    Char,
    Short,
    Int,
    Long,

    Unsigned,
    Signed,

    Float,
    Double,

    Struct(StructDeclaration), // children: ident declaration of struct, field declarations of struct
    Ident,                     // data: Symbol
}

/// A parameter declaration
///
/// Children:
/// - AstSpecifier+, one for each specifier
/// - AstDerivedDeclarator | AstDeclarator
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct AstParamDecl;

/// A typical declaration; this is a stand-in for
/// `int *i[1] = {NULL};` or something similar
///
/// Children: AstSpecifier for each specifier, AstStructDeclaration if necessary,
/// an AstInitDeclarator for each declared variable
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct AstDeclaration;

/// A function definition
///
/// Data: DeclarationSpecifiers
/// Children: AstSpecifier for each specifier, san AstDeclarator, and all the
/// statements associated with the function
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct AstFunctionDefinition;

impl AstInterpretData for AstDeclarator {
    type AstData = Symbol;
}

impl<'a> AstNodeRef<'a> {
    pub fn read_data<T: AstInterpretData>(&self, kind: &T) -> T::AstData {
        return T::AstData::from(*self.data);
    }
}
impl<'a> AstNodeRefMut<'a> {
    pub fn read_data<T: AstInterpretData>(&self, kind: &T) -> T::AstData {
        return T::AstData::from(*self.data);
    }

    pub fn write_data<T: AstInterpretData>(&mut self, kind: &T, data: T::AstData) {
        *self.data = data.into();
    }
}

impl AstNode {
    pub fn read_data<T: AstInterpretData>(&self, kind: &T) -> T::AstData {
        return T::AstData::from(self.data);
    }

    pub fn write_data<T: AstInterpretData>(&mut self, kind: &T, data: T::AstData) {
        self.data = data.into();
    }
}

impl AstNodeVec {
    pub fn rebuild_ids(&mut self) {
        let mut ids = vec![0u32; self.len()];

        for (index, id) in self.id.iter_mut().enumerate() {
            ids[*id as usize] = index as u32;
            *id = index as u32;
        }

        for parent in &mut self.parent {
            *parent = ids[*parent as usize];
        }
    }

    pub fn collect_to_parents<T: Send>(
        &self,
        range: core::ops::Range<usize>,
        extract: impl for<'a> Fn(AstNodeRef<'a>) -> Option<(u32, T)> + Sync,
    ) -> HashMap<u32, Vec<T>> {
        return range
            .into_par_iter()
            .with_min_len(128)
            .fold(
                || HashMap::<u32, Vec<T>>::new(),
                |mut map, index| {
                    let node = self.index(index);

                    if let Some((parent, s)) = extract(node) {
                        let value = map.entry(parent).or_default();
                        value.push(s);
                    }

                    return map;
                },
            )
            .reduce(
                || HashMap::new(),
                |left, right| {
                    let (mut large, small) = if left.len() < right.len() {
                        (right, left)
                    } else {
                        (left, right)
                    };

                    use std::collections::hash_map::Entry;
                    for (k, mut v) in small {
                        match large.entry(k) {
                            Entry::Occupied(mut l) => {
                                let left = l.get_mut();
                                if left.len() >= v.len() {
                                    left.extend(v);
                                } else {
                                    v.append(left);
                                    *left = v;
                                }
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(v);
                            }
                        }
                    }

                    return large;
                },
            );
    }
}
