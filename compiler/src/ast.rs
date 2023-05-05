/*!
This module describes the AST created by the parser.
*/

use crate::api::*;

pub trait AstInterpretData {
    type AstData: From<u64> + Into<u64>;

    fn read(field: &u64) -> Self::AstData {
        return Self::AstData::from(*field);
    }

    fn write(field: &mut u64, value: Self::AstData) {
        *field = value.into();
    }
}

#[derive(Debug, Clone, Copy, StructOfArray)]
pub struct AstNode {
    pub kind: AstNodeKind,
    pub start: u32,
    pub height: u16,
    pub data: u64,

    /// refers to the post_order index of the node that's the parent
    /// of this one.
    pub parent: u32,

    /// The post-order index of this node. The parser returns nodes in post-order,
    /// so these will also be the index in the AST after parsing.
    pub post_order: u32,
}

macro_attr! {
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, EnumFromInner!)]
#[serde(tag = "kind", content = "data")]
pub enum AstNodeKind {
    Expr(AstExpr),
    Statement(AstStatement),
    DerivedDeclarator(AstDerivedDeclarator),
    Declarator(AstDeclarator),
    Specifier(AstSpecifier),
    Declaration(AstDeclaration),
    FunctionDefinition(AstFunctionDefinition),

    // TODO: maybe we wanna delete stuff from the AST later
    // Nop(()),
}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Serialize, Deserialize)]
pub enum AstExpr {
    IntLit,     // data: i32
    LongLit,    // data: i64
    ULit,       // data: u32
    ULongLit,   // data: u64
    FloatLit,   // data: f32
    DoubleLit,  // data: f64
    CharLit,    // data: i8
    StringLit,  // data: end of string text
    Ident,      // data: Symbol
    Assign,     // children: expression being assigned to, expression being assigned
    SizeofExpr, // children: expression that's being queried
    SizeofTy,   // children: type that's being queried
    Cast,       // children: expression being cased and type being cast to
    Member,     // data: field name ; children: base of expression
    PtrMember,  // data: field name ; children: base of expression
    Call,       // data: id of function parameter, children: function and children
    Ternary,    // data: id of condition parameter, children: condition, if_true, and if_false

    UnaryOp(UnaryOp),   // children: expression that's operated on
    BinOp(BinOp),       // children: operands
    BinOpAssign(BinOp), // children: expression being assigned to, expression being assigned
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, PartialOrd, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Hash, Eq, Copy, PartialOrd, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Serialize, Deserialize)]
pub enum StructDeclaration {
    Struct,
    Union,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Serialize, Deserialize)]
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
///
/// Children: AstSpecifer for each type qualifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Serialize, Deserialize)]
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
    /// children: AstParameterDeclarators
    Function,

    /// x(int param1, char, long param3, ...)
    /// children: AstParameterDeclarators
    FunctionElipsis,
}

/// children: a AstDerivedDeclarator for each derived declarator
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Serialize, Deserialize)]
pub enum AstDeclarator {
    Abstract,
    /// data: Symbol
    Ident,
    /// The declarator forwards to another declarator using parentheses
    NestedWithChild,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Serialize, Deserialize)]
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

/// A typical declaration; this is a stand-in for
/// `int *i[1] = {NULL};` or something similar
///
/// Children: AstSpecifier for each specifier, AstStructDeclaration if necessary,
/// an AstInitDeclarator for each declared variable
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Serialize, Deserialize)]
pub struct AstDeclaration;

/// A function definition
///
/// Data: DeclarationSpecifiers
/// Children: AstSpecifier for each specifier, san AstDeclarator, and all the
/// statements associated with the function
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Serialize, Deserialize)]
pub struct AstFunctionDefinition;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Serialize, Deserialize)]
pub enum TypeSpecifier {
    Void = 0,

    Char = 1,
    Short = 2,
    Int = 3,
    Long = 4,

    UChar = 5,
    UShort = 6,
    UInt = 7,
    ULong = 8,

    Float = 9,
    Double = 10,

    Struct = 11,
    Union = 12,
    Ident = 13,

    Invalid = 14,
}

impl Into<u64> for TypeSpecifier {
    fn into(self) -> u64 {
        return self as u64;
    }
}

impl From<u64> for TypeSpecifier {
    fn from(value: u64) -> Self {
        match value {
            0 => Self::Void,
            1 => Self::Char,
            2 => Self::Short,
            3 => Self::Int,
            4 => Self::Long,

            5 => Self::UChar,
            6 => Self::UShort,
            7 => Self::UInt,
            8 => Self::ULong,

            9 => Self::Float,
            10 => Self::Double,

            11 => Self::Struct,
            12 => Self::Union,
            13 => Self::Ident,

            _ => Self::Invalid,
        }
    }
}

#[bitfield(u64)]
pub struct DeclSpecifiers {
    // Inline doesn't make sense for non-definitions
    pub static_: bool,
    pub extern_: bool,
    pub noreturn_: bool,
    pub register_: bool,
    pub typedef_: bool,

    pub const_: bool,
    pub volatile_: bool,
    pub restrict_: bool,
    pub atomic_: bool,

    #[bits(15)]
    _asdf2: u16,

    #[bits(8)]
    pub specifier: TypeSpecifier,

    #[bits(32)]
    pub symbol: Symbol,
}

impl AstInterpretData for AstDeclaration {
    type AstData = DeclSpecifiers;
}

#[bitfield(u64)]
pub struct FuncDefSpecifiers {
    pub static_: bool,
    pub extern_: bool,
    pub inline_: bool,
    pub noreturn_: bool,

    pub const_: bool,
    pub volatile_: bool,
    pub restrict_: bool,
    pub atomic_: bool,

    _asdf2: u16,

    #[bits(8)]
    pub specifier: TypeSpecifier,

    #[bits(32)]
    pub symbol: Symbol,
}

impl AstInterpretData for AstFunctionDefinition {
    type AstData = FuncDefSpecifiers;
}

/// Prints the tree in a text format, so that it's a lil easier to read.
/// Output right now looks like this:
///
/// ```text
/// FunctionDefinition(AstFunctionDefinition)                                  
/// └ Specifier(Int)                                                               
/// └ Declarator(Ident)                                                            
/// | └ DerivedDeclarator(Function)                                                
/// | | └ Declaration(AstDeclaration)                                              
/// | | | └ Specifier(Int)                                                         
/// | | | └ Declarator(Ident)                                                      
/// | | └ Declaration(AstDeclaration)                                              
/// | | | └ Specifier(Char)                                                        
/// | | | └ Declarator(Ident)                                                      
/// | | | | └ DerivedDeclarator(Pointer)                                           
/// | | | | └ DerivedDeclarator(Pointer)                                           
/// └ Statement(Block)                                                             
/// | └ Statement(Ret)                                                             
/// | | └ Expr(StringLit)
/// ```
pub fn display_tree(ast: &AstNodeVec) -> String {
    let mut children = Vec::<Vec<usize>>::with_capacity(ast.len());
    children.resize_with(ast.len(), || Vec::new());

    let mut roots = Vec::new();

    for node in ast.as_slice().into_iter() {
        if *node.post_order != *node.parent {
            children[*node.parent as usize].push(*node.post_order as usize);
        } else {
            roots.push(*node.post_order);
        }
    }

    roots.reverse();

    let mut parent_stack = Vec::with_capacity(roots.len());
    for root in roots.iter().rev() {
        parent_stack.push((0u32, *root as usize));
    }

    let mut out = String::new();
    while let Some((depth, node_id)) = parent_stack.pop() {
        if depth > 0 {
            for _ in 0..(depth - 1) {
                out += "| ";
            }

            out += "└ ";
        }

        out += &format!("{:?}\n", ast.as_slice().index(node_id).kind);

        for id in children[node_id].iter().rev() {
            parent_stack.push((depth + 1, *id));
        }
    }

    return out;
}
