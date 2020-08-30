use crate::buckets::*;
use crate::*;

pub const CLOSING_CHAR: u8 = !0;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind<'a> {
    Ident(u32),
    TypeIdent(u32),
    IntLiteral(i32),
    StringLiteral(&'a str),
    CharLiteral(u8),

    Include(u32),
    IncludeSys(u32),

    Void,
    Char,
    Int,
    Struct,

    If,
    Else,
    Do,
    While,
    For,
    Break,
    Continue,
    Return,
    Typedef,

    Sizeof,
    Cast,

    Dot,
    DotDotDot,
    Arrow,
    Not,
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
    Geq,
    Gt,
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

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,

    Semicolon,
    Comma,

    Invalid,
    End,
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub range: Range,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, range: core::ops::Range<usize>) -> Self {
        Self {
            kind,
            range: r(range.start as u32, range.end as u32),
        }
    }

    pub fn newr<E>(kind: TokenKind<'a>, range: core::ops::Range<usize>) -> Result<Self, E> {
        Ok(Self {
            kind,
            range: r(range.start as u32, range.end as u32),
        })
    }
}

#[inline]
pub fn expect(data: &[u8], current: &mut usize) -> Result<u8, Error> {
    if *current == data.len() {
        return Err(error!("unexpected end of file"));
    }

    let cur = *current;
    *current += 1;
    return Ok(data[cur]);
}

#[inline]
pub fn peek_check(data: &[u8], current: &mut usize, checker: impl Fn(u8) -> bool) -> bool {
    if *current >= data.len() {
        return false;
    }

    return checker(data[*current]);
}

#[inline]
pub fn peek_eq(data: &[u8], current: &mut usize, byte: u8) -> bool {
    if *current >= data.len() {
        return false;
    }

    return data[*current] == byte;
}

pub fn peek_eq_series(data: &[u8], current: &mut usize, bytes: &[u8]) -> bool {
    let byte_len = bytes.len();
    if *current + bytes.len() >= data.len() {
        return false;
    }

    let eq_slice = &data[(*current)..(*current + byte_len)];
    return eq_slice == bytes;
}

#[inline]
pub fn peek_neq(data: &[u8], current: &mut usize, byte: u8) -> bool {
    if *current >= data.len() {
        return false;
    }

    return data[*current] != byte;
}

#[inline]
pub fn peek_neqs(data: &[u8], current: &mut usize, bytes: &[u8]) -> bool {
    if *current >= data.len() {
        return false;
    }

    for byte in bytes {
        if data[*current] == *byte {
            return false;
        }
    }

    return true;
}

#[inline]
pub fn peek_eqs(data: &[u8], current: &mut usize, bytes: &[u8]) -> bool {
    if *current >= data.len() {
        return false;
    }

    for byte in bytes {
        if data[*current] == *byte {
            return true;
        }
    }

    return false;
}

#[inline]
pub fn invalid_token(file: u32, begin: usize, end: usize) -> Error {
    return error!(
        "invalid token",
        r(begin as u32, end as u32),
        file,
        "token found here"
    );
}

pub fn lex_file<'a, 'b>(
    buckets: BucketListRef<'b>,
    symbols: &mut FileDb<'a>,
    file: u32,
    data: &'a str,
) -> Result<Vec<Token<'b>>, Error> {
    let mut toks = Vec::new();
    let bytes = data.as_bytes();
    let mut current = 0;
    let mut done = lex_token(buckets, symbols, file, bytes, &mut current, &mut toks)?;

    while !done {
        done = lex_token(buckets, symbols, file, bytes, &mut current, &mut toks)?;
    }

    return Ok(toks);
}

pub fn lex_token<'a, 'b>(
    buckets: BucketListRef<'b>,
    symbols: &mut FileDb<'a>,
    file: u32,
    data: &'a [u8],
    current: &mut usize,
    output: &mut Vec<Token<'b>>,
) -> Result<bool, Error> {
    let whitespace = [b' ', b'\t'];
    let crlf = [b'\r', b'\n'];

    loop {
        while peek_eqs(data, current, &whitespace) {
            *current += 1;
        }

        // parse macros
        if peek_eq(data, current, b'\n') || peek_eq_series(data, current, &crlf) {
            *current += 1;
        } else {
            break;
        }

        if peek_eq(data, current, b'#') {
            *current += 1;
        } else {
            continue;
        }

        // macros!
        let begin = *current;
        while peek_neqs(data, current, &whitespace) {
            *current += 1;
        }

        let directive = unsafe { std::str::from_utf8_unchecked(&data[begin..*current]) };
        match directive {
            "include" => {
                while peek_eqs(data, current, &whitespace) {
                    *current += 1;
                }

                if peek_eq(data, current, b'"') {
                    *current += 1;
                    let name_begin = *current;
                    while peek_neq(data, current, b'"') {
                        *current += 1;
                    }

                    let id = symbols.translate_add(name_begin..*current, file);
                    output.push(Token::new(TokenKind::Include(id), begin..*current));
                    if peek_eq(data, current, b'\n') || peek_eq_series(data, current, &crlf) {
                        return Ok(false);
                    } else {
                        return Err(expected_newline("include", begin, *current, file));
                    }
                } else if peek_eq(data, current, b'<') {
                    *current += 1;
                    let name_begin = *current;

                    while peek_neq(data, current, b'>') {
                        *current += 1;
                    }

                    let id = symbols.translate_add(name_begin..*current, file);
                    output.push(Token::new(TokenKind::Include(id), begin..*current));
                    if peek_eq(data, current, b'\n') || peek_eq_series(data, current, &crlf) {
                        return Ok(false);
                    } else {
                        return Err(expected_newline("include", begin, *current, file));
                    }
                }
            }
            _ => {
                return Err(error!(
                    "invalid compiler directive",
                    r(begin as u32, *current as u32),
                    file,
                    "directive found here"
                ));
            }
        }
    }

    if *current == data.len() {
        output.push(Token::new(TokenKind::End, *current..*current));
        return Ok(true);
    }

    let begin = *current;
    *current += 1;

    macro_rules! ret_tok {
        ($arg1:expr) => {{
            output.push(Token::new($arg1, begin..*current));
            return Ok(false);
        }};
    }

    let is_ident_char = |cur| {
        (cur >= b'a' && cur <= b'z')
            || (cur >= b'A' && cur <= b'Z')
            || cur == b'_'
            || (cur >= b'0' && cur <= b'9')
    };

    match data[begin] {
        x if (x >= b'a' && x <= b'z') || x == b'_' => {
            while peek_check(data, current, is_ident_char) {
                *current += 1;
            }

            let word = unsafe { std::str::from_utf8_unchecked(&data[begin..*current]) };
            match word {
                "if" => ret_tok!(TokenKind::If),
                "else" => ret_tok!(TokenKind::Else),
                "do" => ret_tok!(TokenKind::Do),
                "while" => ret_tok!(TokenKind::While),
                "for" => ret_tok!(TokenKind::For),
                "break" => ret_tok!(TokenKind::Break),
                "continue" => ret_tok!(TokenKind::Continue),
                "return" => ret_tok!(TokenKind::Return),
                "struct" => ret_tok!(TokenKind::Struct),
                "typedef" => ret_tok!(TokenKind::Typedef),
                "void" => ret_tok!(TokenKind::Void),
                "int" => ret_tok!(TokenKind::Int),
                "char" => ret_tok!(TokenKind::Char),
                word => {
                    let id = symbols.translate_add(begin..*current, file);
                    if word.ends_with("_t") || word == "va_list" {
                        ret_tok!(TokenKind::TypeIdent(id));
                    } else {
                        ret_tok!(TokenKind::Ident(id));
                    }
                }
            }
        }

        x if (x >= b'A' && x <= b'Z') => {
            while peek_check(data, current, is_ident_char) {
                *current += 1;
            }

            let id = symbols.translate_add(begin..*current, file);
            ret_tok!(TokenKind::TypeIdent(id));
        }

        x if (x >= b'0' && x <= b'9') => {
            let mut int_value: i32 = (x - b'0') as i32;
            while peek_check(data, current, |b| b >= b'0' && b <= b'9') {
                int_value *= 10;
                int_value += (data[*current] - b'0') as i32;
                *current += 1;
            }

            ret_tok!(TokenKind::IntLiteral(int_value));
        }

        b'\"' => {
            let mut cur = lex_character(b'\"', file, data, current)?;
            let mut chars = Vec::new();
            while cur != CLOSING_CHAR {
                chars.push(cur);
                cur = lex_character(b'\"', file, data, current)?;
            }

            let string = unsafe { std::str::from_utf8_unchecked(&chars) };
            let string = buckets.add_str(string);
            ret_tok!(TokenKind::StringLiteral(string));
        }

        b'\'' => {
            let byte = lex_character(b'\'', file, data, current)?;
            if byte == CLOSING_CHAR {
                return Err(error!(
                    "empty character literal",
                    r(begin as u32, *current as u32),
                    file,
                    "found here"
                ));
            }

            let closing = expect(data, current)?;
            if closing != b'\'' {
                return Err(error!(
                    "expected closing single quote",
                    r(begin as u32, *current as u32),
                    file,
                    "this should be a closing single quote"
                ));
            }

            ret_tok!(TokenKind::CharLiteral(byte));
        }

        b'{' => ret_tok!(TokenKind::LBrace),
        b'}' => ret_tok!(TokenKind::RBrace),
        b'(' => ret_tok!(TokenKind::LParen),
        b')' => ret_tok!(TokenKind::RParen),
        b'[' => ret_tok!(TokenKind::LBracket),
        b']' => ret_tok!(TokenKind::RBracket),
        b'~' => ret_tok!(TokenKind::Tilde),
        b';' => ret_tok!(TokenKind::Semicolon),
        b',' => ret_tok!(TokenKind::Comma),

        b'.' => {
            if peek_eq(data, current, b'.') {
                *current += 1;
                if peek_eq(data, current, b'.') {
                    *current += 1;
                    ret_tok!(TokenKind::DotDotDot);
                }

                return Err(invalid_token(file, begin, *current));
            }

            ret_tok!(TokenKind::Dot);
        }
        b'+' => {
            if peek_eq(data, current, b'+') {
                *current += 1;
                ret_tok!(TokenKind::PlusPlus);
            } else if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::PlusEq);
            } else {
                ret_tok!(TokenKind::Plus);
            }
        }
        b'-' => {
            if peek_eq(data, current, b'-') {
                *current += 1;
                ret_tok!(TokenKind::DashDash);
            } else if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::DashEq);
            } else if peek_eq(data, current, b'>') {
                *current += 1;
                ret_tok!(TokenKind::Arrow);
            } else {
                ret_tok!(TokenKind::Dash);
            }
        }
        b'/' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::SlashEq);
            } else {
                ret_tok!(TokenKind::Slash);
            }
        }
        b'*' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::StarEq);
            } else {
                ret_tok!(TokenKind::Star);
            }
        }
        b'%' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::PercentEq);
            } else {
                ret_tok!(TokenKind::Percent);
            }
        }
        b'>' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::Geq);
            } else {
                ret_tok!(TokenKind::Gt);
            }
        }
        b'<' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::Leq);
            } else {
                ret_tok!(TokenKind::Gt);
            }
        }
        b'!' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::Neq);
            } else {
                ret_tok!(TokenKind::Not);
            }
        }
        b'=' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::EqEq);
            } else {
                ret_tok!(TokenKind::Eq);
            }
        }
        b'|' => {
            if peek_eq(data, current, b'|') {
                *current += 1;
                ret_tok!(TokenKind::LineLine);
            } else if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::LineEq);
            } else {
                ret_tok!(TokenKind::Line);
            }
        }
        b'&' => {
            if peek_eq(data, current, b'&') {
                *current += 1;
                ret_tok!(TokenKind::AmpAmp);
            } else if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::AmpEq);
            } else {
                ret_tok!(TokenKind::Amp);
            }
        }
        b'^' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                ret_tok!(TokenKind::CaretEq);
            } else {
                ret_tok!(TokenKind::Caret);
            }
        }

        _ => return Err(invalid_token(file, begin, *current)),
    }
}

pub fn lex_character(
    surround: u8,
    file: u32,
    data: &[u8],
    current: &mut usize,
) -> Result<u8, Error> {
    loop {
        let cur_b = expect(data, current)?;
        let cur: char = cur_b.into();

        if !cur.is_ascii() {
            return Err(error!(
                "character is not valid ascii",
                r(*current as u32 - 1, *current as u32),
                file,
                "invalid character literal here"
            ));
        }

        if cur_b == surround {
            return Ok(CLOSING_CHAR);
        }

        if cur_b == b'\n' || cur_b == b'\r' {
            if surround == b'\"' {
                return Err(error!(
                    "invalid character found when parsing string literal",
                    r(*current as u32 - 1, *current as u32),
                    file,
                    "invalid character here"
                ));
            } else {
                return Err(error!(
                    "invalid character found when parsing character literal",
                    r(*current as u32 - 1, *current as u32),
                    file,
                    "invalid character here"
                ));
            }
        }

        if cur_b != b'\\' {
            return Ok(cur_b);
        }

        match expect(data, current)? {
            b'n' => return Ok(b'\n'),
            b'\n' => continue,
            b'\'' => return Ok(b'\''),
            b'"' => return Ok(b'"'),
            _ => {
                return Err(error!(
                    "invalid escape sequence",
                    r(*current as u32 - 2, *current as u32),
                    file,
                    "invalid escape sequence here"
                ))
            }
        }
    }
}

#[inline]
pub fn expected_newline(directive_name: &'static str, begin: usize, current: usize, file: u32) -> Error {
    return error!(
        &format!("expected newline after {} directive", directive_name),
        r(begin as u32, current as u32),
        file,
        "directive here"
    );
}
