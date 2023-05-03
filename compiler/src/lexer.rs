use crate::{api::*, filedb::FileType};

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum TokenKind {
    PreprocessingNum,

    IntNum,
    FloatNum,
    StringLit,
    CharLit,

    Comment,
    Newline,
    EOF,

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
    GtGt,     // >>
    Amp,      // &
    AmpAmp,   // &&
    Line,     // |
    LineLine, // ||
    Caret,    // ^
    AmpEq,
    LineEq, // |=
    CaretEq,
    PlusEq,
    DashEq,
    SlashEq,
    StarEq,
    PercentEq,
    LtLtEq,
    GtGtEq,

    LBrace, // {
    RBrace,
    LParen,
    RParen,
    LBracket, // [
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

#[derive(Clone, Copy, StructOfArray, Serialize, Deserialize)]
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

#[derive(Clone, Copy)]
struct IncludeEntry<'a> {
    file_id: u32,
    contents: &'a [u8],
    index: usize,
}

pub struct LexResult {
    pub translation_unit: TranslationUnitDebugInfo,
    pub symbols: SymbolTable,
    pub tokens: TokenVec,
}

#[derive(Debug)]
pub struct LexError {
    pub translation_unit: TranslationUnitDebugInfo,
    pub error: Error,
}

pub fn lex(files: &FileDb, file: &File) -> Result<LexResult, LexError> {
    let mut result = LexResult {
        translation_unit: TranslationUnitDebugInfo {
            file_starts: vec![FileStarts {
                index: 0,
                file: file.id,
                file_index: 0,
            }],
        },
        symbols: SymbolTable::new(),
        tokens: TokenVec::new(),
    };
    let mut index = 0;
    let mut include_stack = vec![IncludeEntry {
        file_id: file.id,
        contents: &file.source.as_bytes(),
        index: 0,
    }];

    loop {
        let mut input = match include_stack.pop() {
            Some(f) => f,
            None => break,
        };

        result.translation_unit.file_starts.push(FileStarts {
            file: input.file_id,
            index: index,
            file_index: input.index,
        });

        let mut token_count = 0;
        let mut was_hashtag = false;
        while input.index < input.contents.len() {
            // Skip whitespace
            while input.index < input.contents.len() {
                let b = input.contents[input.index];
                if b != b' ' && b != b'\t' {
                    break;
                }

                input.index += 1;
                index += 1;
            }

            if input.index >= input.contents.len() {
                break;
            }

            let data = &input.contents[input.index..];
            let res = match lex_tok_from_bytes(data) {
                Ok(res) => res,
                Err(error) => {
                    return Err(LexError {
                        translation_unit: result.translation_unit,
                        error,
                    })
                }
            };

            let kind = res.kind;
            let start = index;

            index += res.consumed as u32;
            input.index += res.consumed;

            let symbol = match kind {
                TokenKind::Ident => {
                    let s = unsafe { core::str::from_utf8_unchecked(&data[..res.consumed]) };
                    let symbol = result.symbols.add_str(s);

                    symbol
                }

                _ => Symbol::NullSymbol,
            };

            if was_hashtag && symbol == Symbol::Include {
                // Get rid of the hashtag we got previously
                result.tokens.pop().unwrap();
                token_count -= 1;

                let res = match lex_include_line(&input.contents[input.index..]) {
                    Ok(res) => res,
                    Err(error) => {
                        return Err(LexError {
                            translation_unit: result.translation_unit,
                            error,
                        })
                    }
                };

                index += res.consumed as u32;
                input.index += res.consumed;

                let resolved_result = match res.file_type {
                    FileType::System => files.resolve_system_include(res.file, input.file_id),
                    FileType::User => files.resolve_include(res.file, input.file_id),
                };

                let resolved = match resolved_result {
                    Ok(res) => res,
                    Err(e) => {
                        return Err(LexError {
                            translation_unit: result.translation_unit,
                            error: Error::new(ErrorKind::Todo(e)),
                        })
                    }
                };

                include_stack.push(input);

                include_stack.push(IncludeEntry {
                    file_id: resolved.id,
                    contents: resolved.source.as_bytes(),
                    index: 0,
                });

                break;
            }

            result.tokens.push(Token {
                start,
                kind,
                symbol,
            });

            token_count += 1;
            was_hashtag = kind == TokenKind::Hashtag;
        }

        if !token_count > 0 {
            result.translation_unit.file_starts.pop().unwrap();
        }
    }

    return Ok(result);
}

struct LexedTok {
    consumed: usize,
    kind: TokenKind,
}

/// Lex a token from the bytes given. Assumes that we're not at EOF, and
/// theres no whitespace before the token.
fn lex_tok_from_bytes<'a>(data: &'a [u8]) -> Result<LexedTok, Error> {
    let mut index: usize = 0;

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
            (b'/', Some(b'/')) => {
                // we've consumed 1 extra character already from the second '/'
                // ALSO though, index is already pushed forwards by one
                // So this code leaves our index right before the newline we just found
                let mut i = 1;
                while let Some(&b) = data.get(index + i) {
                    // Consume until the newline
                    match b {
                        b'\n' | b'\r' => break,
                        _ => i += 1,
                    }
                }
                (i, TokenKind::Comment)
            }
            (b'/', Some(b'*')) => {
                let mut i = 1;
                let mut prev = 0u8;
                loop {
                    let b = *data.get(index + i).ok_or(Error::new(ErrorKind::Todo(
                        "EOF while inside a block comment",
                    )))?;
                    i += 1;

                    // Consume until we hit the suffix
                    match (prev, b) {
                        (b'*', b'/') => break,
                        _ => prev = b,
                    }
                }

                (i, TokenKind::Comment)
            }
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

        x if x >= b'0' && x <= b'9' => return lex_num(index, data),

        b'.' => {
            if let Some(true) = data.get(index).map(|c| *c >= b'0' && *c <= b'9') {
                index += 1;
                return lex_num(index, data);
            }

            if let Some(b'.') = data.get(index) {
                index += 1;
                if let Some(b'.') = data.get(index) {
                    index += 1;

                    return Ok(LexedTok {
                        consumed: index,
                        kind: TokenKind::DotDotDot,
                    });
                }

                throw!(Todo("'..' isn't valid"));
            }

            return Ok(LexedTok {
                consumed: index,
                kind: TokenKind::Dot,
            });
        }

        b'\"' => return lex_character(TokenKind::StringLit, b'\"', index, data),
        b'\'' => return lex_character(TokenKind::CharLit, b'\'', index, data),

        x => throw!(Todo("invalid character")),
    }
}

pub fn is_ident_char(cur: u8) -> bool {
    (cur >= b'a' && cur <= b'z')
        || (cur >= b'A' && cur <= b'Z')
        || cur == b'_'
        || (cur >= b'0' && cur <= b'9')
}

// NOTE: We assume at this point that we are in fact lexing a number.
fn lex_num(mut index: usize, data: &[u8]) -> Result<LexedTok, Error> {
    /*
    https://gcc.gnu.org/onlinedocs/cpp/Tokenization.html

    A preprocessing number has a rather bizarre definition. The category includes all the
    normal integer and floating point constants one expects of C, but also a number of other
    things one might not initially recognize as a number. Formally, preprocessing numbers
    begin with an optional period, a required decimal digit, and then continue with any sequence
    of letters, digits, underscores, periods, and exponents. Exponents are the two-character
    sequences ‘e+’, ‘e-’, ‘E+’, ‘E-’, ‘p+’, ‘p-’, ‘P+’, and ‘P-’. (The exponents that begin with
    ‘p’ or ‘P’ are used for hexadecimal floating-point constants.)
    */

    while index < data.len() {
        let lower = data[index].to_ascii_lowercase();

        match lower {
            b'a'..=b'z' => {}
            b'0'..=b'9' => {}
            b'.' => {}
            b'_' => {}
            x => break,
        }

        index += 1;

        // Match against exponent
        if lower == b'e' || lower == b'p' {
            match data.get(index) {
                Some(b'-') | Some(b'+') => index += 1,
                _ => {}
            }

            continue;
        }
    }

    return Ok(LexedTok {
        consumed: index,
        kind: TokenKind::PreprocessingNum,
    });
}

fn lex_character(
    kind: TokenKind,
    surround: u8,
    mut index: usize,
    data: &[u8],
) -> Result<LexedTok, Error> {
    while index < data.len() {
        let cur = data[index];
        if !cur.is_ascii() {
            throw!(Todo("character is not valid ascii"));
        }

        if cur == surround {
            return Ok(LexedTok {
                consumed: index + 1,
                kind,
            });
        }

        // handle early newline
        if cur == b'\n' || cur == b'\r' {
            throw!(Todo("invalid character found when parsing string literal",));
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

    throw!(Todo("File ended before ohe string was closed"));
}

struct IncludeResult<'a> {
    file: &'a str,
    file_type: FileType,
    consumed: usize,
}

fn lex_include_line(data: &[u8]) -> Result<IncludeResult, Error> {
    let mut index = 0;
    while index < data.len() {
        match data[index] {
            b' ' | b'\t' => index += 1,
            _ => break,
        }
    }

    let (end_quote, file_type) = match data[index] {
        b'"' => (b'"', FileType::User),
        b'<' => (b'>', FileType::System),
        _ => throw!(Todo("expected a file string")),
    };

    index += 1;

    let begin = index;

    loop {
        match data.get(index) {
            None => {
                throw!(Todo("file ended before include file string was done",))
            }
            Some(b'\n') | Some(b'\r') => {
                throw!(Todo("line ended before file string was done"))
            }

            Some(x) if *x == end_quote => break,
            _ => index += 1,
        }
    }

    let end = index;

    index += 1;

    while index < data.len() {
        match data[index] {
            b' ' | b'\t' => index += 1,
            _ => break,
        }
    }

    let increment = match (data[index], data.get(index + 1)) {
        (b'\n', Some(b'\r')) => 2,
        (b'\r', Some(b'\n')) => 2,

        (b'\n', _) => 1,
        (b'\r', _) => 1,

        _ => throw!(Todo("extra stuff after include file name")),
    };

    index += increment;

    return Ok(IncludeResult {
        file: unsafe { core::str::from_utf8_unchecked(&data[begin..end]) },
        file_type,
        consumed: index,
    });
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
