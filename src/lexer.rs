use std::fs::File;

use crate::api::*;

#[derive(Debug, PartialEq, Clone, Copy, Hash)]
pub enum TokenKind {
    PreprocessingNum,

    IntNum,
    FloatNum,
    StringLit,
    CharLit,

    EOF,
    Newline,

    Hashtag,
    Dot,
    DotDotDot,
    Arrow,
    Bang,
    Question,
    Tilde,
    Star,
    Slash,
    Plus,
    Dash,
    Percent,
    PlusPlus,
    DashDash,

    Eq,
    EqEq,
    Neq,
    Leq,
    Lt,
    LtLt, // <<
    Geq,
    Gt,
    GtGt, // >>
    Amp,
    AmpAmp,
    Line,     // |
    LineLine, // ||
    Caret,
    AmpEq,
    LineEq,
    CaretEq,
    PlusEq,
    DashEq,
    SlashEq,
    StarEq,
    PercentEq,
    LtLtEq,
    GtGtEq,

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,

    Semicolon,
    Colon,
    Comma,

    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Unsigned,
    Signed,
    Struct,
    Union,
    Enum,
    Sizeof,

    Typedef,
    Extern,
    Static,
    Register,

    Const,
    Volatile,
    Inline,
    Restrict,

    If,
    Else,
    Do,
    While,
    For,
    Break,
    Continue,
    Case,
    Default,
    Switch,
    Return,
    Goto,

    Ident,
}

#[derive(Clone, Copy, StructOfArray)]
pub struct Token {
    pub kind: TokenKind,
    pub start: u32,
    pub symbol: Symbol,
}

lazy_static! {
    pub static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenKind> = {
        let mut set = HashMap::new();
        // set.insert("auto", TokenKind::Unimplemented);
        set.insert("break", TokenKind::Break);
        set.insert("case", TokenKind::Case);
        set.insert("char", TokenKind::Char);
        set.insert("const", TokenKind::Const);
        set.insert("continue", TokenKind::Continue);
        set.insert("default", TokenKind::Default);
        set.insert("do", TokenKind::Do);
        set.insert("double", TokenKind::Double);
        set.insert("else", TokenKind::Else);
        set.insert("enum", TokenKind::Enum);
        set.insert("extern", TokenKind::Extern);
        set.insert("float", TokenKind::Float);
        set.insert("for", TokenKind::For);
        set.insert("goto", TokenKind::Goto);
        set.insert("if", TokenKind::If);
        set.insert("inline", TokenKind::Inline);
        set.insert("int", TokenKind::Int);
        set.insert("long", TokenKind::Long);
        set.insert("register", TokenKind::Register);
        set.insert("restrict", TokenKind::Restrict);
        set.insert("return", TokenKind::Return);
        set.insert("short", TokenKind::Short);
        set.insert("signed", TokenKind::Signed);
        set.insert("sizeof", TokenKind::Sizeof);
        set.insert("static", TokenKind::Static);
        set.insert("struct", TokenKind::Struct);
        set.insert("switch", TokenKind::Switch);
        set.insert("typedef", TokenKind::Typedef);
        set.insert("union", TokenKind::Union);
        set.insert("unsigned", TokenKind::Unsigned);
        set.insert("void", TokenKind::Void);
        // set.insert("volatile", TokenKind::Unimplemented);
        set.insert("while", TokenKind::While);
        // set.insert("_Alignas", TokenKind::Unimplemented);
        // set.insert("_Alignof", TokenKind::Unimplemented);
        // set.insert("_Atomic", TokenKind::Unimplemented);
        // set.insert("_Bool", TokenKind::Unimplemented);
        // set.insert("_Complex", TokenKind::Unimplemented);
        // set.insert("_Generic", TokenKind::Unimplemented);
        // set.insert("_Imaginary", TokenKind::Unimplemented);
        // set.insert("_Noreturn", TokenKind::Unimplemented);
        // set.insert("_Static_assert", TokenKind::Unimplemented);
        // set.insert("_Thread_local", TokenKind::Unimplemented);
        // set.insert("_Float16", TokenKind::Unimplemented);
        // set.insert("_Float16x", TokenKind::Unimplemented);
        // set.insert("_Float32", TokenKind::Unimplemented);
        // set.insert("_Float32x", TokenKind::Unimplemented);
        // set.insert("_Float64", TokenKind::Unimplemented);
        // set.insert("_Float64x", TokenKind::Unimplemented);
        // set.insert("_Float128", TokenKind::Unimplemented);
        // set.insert("_Float128x", TokenKind::Unimplemented);
        // set.insert("_Decimal32", TokenKind::Unimplemented);
        // set.insert("_Decimal32x", TokenKind::Unimplemented);
        // set.insert("_Decimal64", TokenKind::Unimplemented);
        // set.insert("_Decimal64x", TokenKind::Unimplemented);
        // set.insert("_Decimal128", TokenKind::Unimplemented);
        // set.insert("_Decimal128x", TokenKind::Unimplemented);

        set
    };
}

// Book-keeping to track which ranges belong to which file, so that we can
// compute file and line number from `start`
#[derive(Clone, Copy)]
struct FileStarts {
    index: u32,
    file: Symbol,
    file_index: usize,
}

struct IncludeEntry {
    symbol: Symbol,
    contents: Vec<u8>,
    file_index: usize,
}

// Processes tokens and also expands #include
struct Lexer {
    index: u32,
    input_symbol: Symbol,
    input: Vec<u8>,
    input_index: usize,
    include_stack: Vec<IncludeEntry>,

    result: LexResult,
}

pub struct LexResult {
    file_starts: Vec<FileStarts>,
    symbols: SymbolTable,
    tokens: TokenVec,
}

pub fn lex(file: &str, text: &str) -> Result<LexResult, String> {
    let mut lexer = Lexer::new(file, text);

    loop {
        while lexer.input_index < lexer.input.len() {
            let data = &lexer.input[lexer.input_index..];
            let res = lex_tok_from_bytes(data)?;
            let kind = res.kind;

            let start = lexer.index;
            lexer.index += res.consumed as u32;
            lexer.input_index += res.consumed;

            if res.kind == TokenKind::Ident {
                let s = unsafe { core::str::from_utf8_unchecked(&data[..res.consumed]) };
                let symbol = lexer.result.symbols.add_str(s);

                lexer.result.tokens.push(Token {
                    start,
                    kind,
                    symbol,
                });
                continue;
            }

            lexer.result.tokens.push(Token {
                start,
                kind,
                symbol: Symbol::NullSymbol,
            });
        }

        if lexer.include_stack.len() == 0 {
            break;
        }

        let entry = match lexer.include_stack.pop() {
            Some(f) => f,
            None => break,
        };

        lexer.input = entry.contents;
        lexer.input_index = entry.file_index;
        lexer.input_symbol = entry.symbol;
        lexer.result.file_starts.push(FileStarts {
            file: entry.symbol,
            index: lexer.index,
            file_index: lexer.input_index,
        });
    }

    return Ok(lexer.result);
}

impl Lexer {
    fn new(file: &str, text: &str) -> Self {
        let mut symbols = SymbolTable::new();
        let file = symbols.add_str(file);
        return Self {
            result: LexResult {
                file_starts: vec![FileStarts {
                    index: 0,
                    file,
                    file_index: 0,
                }],
                symbols,
                tokens: TokenVec::new(),
            },

            index: 0,
            input: text.as_bytes().to_owned(),
            input_symbol: file,
            input_index: 0,
            include_stack: Vec::new(),
        };
    }
}

struct LexedTok {
    consumed: usize,
    kind: TokenKind,
}

fn lex_tok_from_bytes<'a>(data: &'a [u8]) -> Result<LexedTok, String> {
    let mut index: usize = 0;

    // Skip whitespace
    while index < data.len() && (data[index] == b' ' || data[index] == b'\t') {
        index += 1;
    }

    if index >= data.len() {
        return Ok(LexedTok {
            consumed: 0,
            kind: TokenKind::EOF,
        });
    }

    let first = data[index];
    index += 1;

    'simple_syntax: {
        let kind = match first {
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'[' => TokenKind::LBracket,
            b']' => TokenKind::RBracket,
            b'~' => TokenKind::Tilde,
            b';' => TokenKind::Semicolon,
            b':' => TokenKind::Colon,
            b',' => TokenKind::Comma,
            b'?' => TokenKind::Question,
            b'#' => TokenKind::Hashtag,

            // NOTE: This will spit out 2 newlines for CRLF, maybe that's bad, but i think its probably fine
            b'\r' | b'\n' => TokenKind::Newline,

            _ => break 'simple_syntax,
        };

        return Ok(LexedTok {
            consumed: index,
            kind,
        });
    }

    'operator: {
        let (increment, kind) = match (first, data.get(index).map(|i| *i)) {
            (b'+', Some(b'+')) => (1 as usize, TokenKind::PlusPlus),
            (b'+', Some(b'=')) => (1, TokenKind::PlusEq),
            (b'+', _) => (0, TokenKind::Plus),

            (b'-', Some(b'-')) => (1 as usize, TokenKind::DashDash),
            (b'-', Some(b'>')) => (1 as usize, TokenKind::Arrow),
            (b'-', Some(b'=')) => (1, TokenKind::DashEq),
            (b'-', _) => (0, TokenKind::Dash),

            (b'/', Some(b'=')) => (1, TokenKind::SlashEq),
            (b'/', _) => (0, TokenKind::Slash),

            (b'*', Some(b'=')) => (1, TokenKind::StarEq),
            (b'*', _) => (0, TokenKind::Star),

            (b'%', Some(b'=')) => (1, TokenKind::PercentEq),
            (b'%', _) => (0, TokenKind::Percent),

            (b'>', Some(b'=')) => (1, TokenKind::Geq),
            (b'>', Some(b'>')) => match data.get(index + 1).map(|i| *i) {
                Some(b'=') => (2, TokenKind::GtGtEq),
                _ => (1, TokenKind::GtGt),
            },
            (b'>', _) => (0, TokenKind::Gt),

            (b'<', Some(b'=')) => (1, TokenKind::Leq),
            (b'<', Some(b'<')) => match data.get(index + 1).map(|i| *i) {
                Some(b'=') => (2, TokenKind::LtLtEq),
                _ => (1, TokenKind::LtLt),
            },
            (b'<', _) => (0, TokenKind::Lt),

            (b'!', Some(b'=')) => (1, TokenKind::Neq),
            (b'!', _) => (0, TokenKind::Bang),

            (b'=', Some(b'=')) => (1, TokenKind::EqEq),
            (b'=', _) => (0, TokenKind::Eq),

            (b'|', Some(b'=')) => (1, TokenKind::LineEq),
            (b'|', Some(b'|')) => (1, TokenKind::LineLine),
            (b'|', _) => (0, TokenKind::Line),

            (b'&', Some(b'=')) => (1, TokenKind::AmpEq),
            (b'&', Some(b'&')) => (1, TokenKind::AmpAmp),
            (b'&', _) => (0, TokenKind::Amp),

            (b'^', Some(b'=')) => (1, TokenKind::CaretEq),
            (b'^', _) => (0, TokenKind::Caret),

            _ => break 'operator,
        };

        index += increment;

        return Ok(LexedTok {
            consumed: index,
            kind,
        });
    }

    match first {
        x if (x >= b'A' && x <= b'Z') || (x >= b'a' && x <= b'z') || x == b'_' => {
            for &c in &data[index..] {
                if !is_ident_char(c) {
                    break;
                }

                index += 1;
            }

            let word = unsafe { core::str::from_utf8_unchecked(&data[..index]) };
            if let Some(&kind) = RESERVED_KEYWORDS.get(word) {
                return Ok(LexedTok {
                    consumed: index,
                    kind,
                });
            }

            return Ok(LexedTok {
                consumed: index,
                kind: TokenKind::Ident,
            });
        }

        x if x >= b'0' && x <= b'9' => {
            unimplemented!()
        }

        b'\"' => return lex_character(TokenKind::StringLit, b'\"', data),
        b'\'' => return lex_character(TokenKind::CharLit, b'\'', data),

        x => return Err("".to_string()),
    }
}

pub fn is_ident_char(cur: u8) -> bool {
    (cur >= b'a' && cur <= b'z')
        || (cur >= b'A' && cur <= b'Z')
        || cur == b'_'
        || (cur >= b'0' && cur <= b'9')
}

fn lex_character(kind: TokenKind, surround: u8, data: &[u8]) -> Result<LexedTok, String> {
    let mut index = 1;
    while index < data.len() {
        let cur = data[index];
        if !cur.is_ascii() {
            return Err("character is not valid ascii".to_owned());
        }

        if cur == surround {
            return Ok(LexedTok {
                consumed: index + 1,
                kind,
            });
        }

        // handle early newline
        if cur == b'\n' || cur == b'\r' {
            return Err("invalid character found when parsing string literal".to_string());
        }

        // handle escape cases
        if cur == b'\\' {
            index += 1;

            let mut iterations_left = 3;
            while iterations_left > 0 {
                match cur {
                    b'0'..=b'7' => iterations_left -= 1, // Octals
                    _ => iterations_left = 0,
                }
            }
        }

        index += 1;
    }

    return Err("File ended before the string was closed".to_owned());
}

// TODO: Add ability to skip escaped newlines
fn skip_line_ext(data: &[u8]) -> usize {
    let mut bytes: [u8; 3] = [0, 0, 0];

    let len = std::cmp::min(data.len(), bytes.len());
    for idx in 0..len {
        bytes[idx] = data[idx];
    }

    if &bytes == b"\\\r\n" || &bytes == b"\\\n\r" {
        return 3;
    }
    if &bytes[..2] == b"\\\n" || &bytes[..2] == b"\\\r" {
        return 2;
    }

    return 0;
}
