use crate::api::*;

#[derive(Debug, PartialEq, Clone, Copy, Hash)]
pub enum TokenKind {
    PreprocessingNum,

    IntNum,
    FloatNum,
    StringLit,
    CharLit,

    Whitespace,
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
    pub symbol: u32,
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
    file_index: u32,
}

struct IncludeEntry {
    contents: Vec<u8>,
    file_index: u32,
}

// Processes tokens and also expands #include
pub struct Lexer {
    file_starts: Vec<FileStarts>,
    symbols: SymbolTable,
    tokens: TokenVec,

    index: u32,
    input: Vec<u8>,
    input_index: u32,
    include_stack: Vec<IncludeEntry>,
}

impl Lexer {
    pub fn new(text: &str) -> Self {
        return Self {
            file_starts: Vec::new(),
            symbols: SymbolTable::new(),
            tokens: TokenVec::new(),

            index: 0,
            input: text.as_bytes().to_owned(),
            input_index: 0,
            include_stack: Vec::new(),
        };
    }

    pub fn peek(&self) -> Option<u8> {
        return self.input.get(self.input_index as usize).map(|i| *i);
    }

    pub fn pop(&mut self) -> Option<u8> {
        loop {
            if let Some(next) = self.input.get(self.input_index as usize).map(|i| *i) {
                self.input_index += 1;
                return Some(next);
            }

            let next_input = self.include_stack.pop()?;
            self.input = next_input.contents;
            self.input_index = next_input.file_index;
        }
    }
}

struct LexResult {
    consumed: usize,
    kind: TokenKind,
}

fn lex_from_bytes<'a>(data: &'a [u8]) -> Result<LexResult, String> {
    if data.len() == 0 {
        return Ok(LexResult {
            consumed: 0,
            kind: TokenKind::Whitespace,
        });
    }

    let mut index: usize = 1;
    let first = data[0];

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
                return Ok(LexResult {
                    consumed: index,
                    kind,
                });
            }

            return Ok(LexResult {
                consumed: index,
                kind: TokenKind::Ident,
            });
        }

        x if x >= b'0' && x <= b'9' => {
            unimplemented!()
        }

        b'\"' => {
            unimplemented!()
        }

        /*

                b'\'' => {
                    let byte = self.lex_character(b'\'', data)?;
                    if byte == CLOSING_CHAR {
                        return Err(error!("empty character literal", self.loc(), "found here"));
                    }

                    let closing = self.expect(data)?;
                    if closing != b'\'' {
                        return Err(error!(
                            "expected closing single quote",
                            self.loc(),
                            "this should be a closing single quote"
                        ));
                    }

                    ret!(TokenKind::CharLit(byte as i8));
                }

                b'{' => ret!(TokenKind::LBrace),
                b'}' => ret!(TokenKind::RBrace),
                b'(' => ret!(TokenKind::LParen),
                b')' => ret!(TokenKind::RParen),
                b'[' => ret!(TokenKind::LBracket),
                b']' => ret!(TokenKind::RBracket),
                b'~' => ret!(TokenKind::Tilde),
                b';' => ret!(TokenKind::Semicolon),
                b':' => ret!(TokenKind::Colon),
                b',' => ret!(TokenKind::Comma),
                b'?' => ret!(TokenKind::Question),
                b'#' => {
                    if self.at_line_begin {
                        self.at_line_begin = false;
                        return Ok(Some(self.lex_directive(buckets, symbols, files, data)?));
                    } else {
                        return Err(error!("unexpected token", self.loc(), "this token"));
                    }
                }

                b'.' => {
                    if self.peek_eq(data, b'.') {
                        self.current += 1;
                        if self.peek_eq(data, b'.') {
                            incr_ret!(TokenKind::DotDotDot);
                        }

                        return Err(invalid_token(self.begin, self.current, self.file));
                    }

                    ret!(TokenKind::Dot);
                }
                b'+' => {
                    if self.peek_eq(data, b'+') {
                        incr_ret!(TokenKind::PlusPlus);
                    } else if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::PlusEq);
                    } else {
                        ret!(TokenKind::Plus);
                    }
                }
                b'-' => {
                    if self.peek_eq(data, b'-') {
                        incr_ret!(TokenKind::DashDash);
                    } else if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::DashEq);
                    } else if self.peek_eq(data, b'>') {
                        incr_ret!(TokenKind::Arrow);
                    } else {
                        ret!(TokenKind::Dash);
                    }
                }
                b'/' => {
                    if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::SlashEq);
                    } else {
                        ret!(TokenKind::Slash);
                    }
                }
                b'*' => {
                    if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::StarEq);
                    } else {
                        ret!(TokenKind::Star);
                    }
                }
                b'%' => {
                    if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::PercentEq);
                    } else {
                        ret!(TokenKind::Percent);
                    }
                }
                b'>' => {
                    if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::Geq);
                    } else if self.peek_eq(data, b'>') {
                        self.current += 1;
                        if self.peek_eq(data, b'=') {
                            incr_ret!(TokenKind::GtGtEq);
                        }
                        ret!(TokenKind::GtGt);
                    } else {
                        ret!(TokenKind::Gt);
                    }
                }
                b'<' => {
                    if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::Leq);
                    } else if self.peek_eq(data, b'<') {
                        self.current += 1;
                        if self.peek_eq(data, b'=') {
                            incr_ret!(TokenKind::LtLtEq);
                        }
                        ret!(TokenKind::LtLt);
                    } else {
                        ret!(TokenKind::Lt);
                    }
                }
                b'!' => {
                    if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::Neq);
                    } else {
                        ret!(TokenKind::Bang);
                    }
                }
                b'=' => {
                    if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::EqEq);
                    } else {
                        ret!(TokenKind::Eq);
                    }
                }
                b'|' => {
                    if self.peek_eq(data, b'|') {
                        incr_ret!(TokenKind::LineLine);
                    } else if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::LineEq);
                    } else {
                        ret!(TokenKind::Line);
                    }
                }
                b'&' => {
                    if self.peek_eq(data, b'&') {
                        incr_ret!(TokenKind::AmpAmp);
                    } else if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::AmpEq);
                    } else {
                        ret!(TokenKind::Amp);
                    }
                }
                b'^' => {
                    if self.peek_eq(data, b'=') {
                        incr_ret!(TokenKind::CaretEq);
                    } else {
                        ret!(TokenKind::Caret);
                    }
                }
        */
        x => {
            return Err("".to_string());
        }
    }
}

pub fn is_ident_char(cur: u8) -> bool {
    (cur >= b'a' && cur <= b'z')
        || (cur >= b'A' && cur <= b'Z')
        || cur == b'_'
        || (cur >= b'0' && cur <= b'9')
}

fn lex_character(kind: TokenKind, surround: u8, data: &[u8]) -> Result<LexResult, String> {
    let mut escaped = false;
    for (index, &cur_b) in data.iter().enumerate().skip(1) {
        let cur: char = cur_b.into();
        if !cur.is_ascii() {
            return Err("character is not valid ascii".to_owned());
        }

        if cur_b == surround && !escaped {
            return Ok(LexResult {
                consumed: index + 1,
                kind,
            });
        }

        // handle backslash-enter

        // handle all the escape cases

        escaped = cur_b == b'\\';
    }

    return Err("File ended before the string was closed".to_owned());
}
