use crate::buckets::*;
use crate::*;
use std::collections::HashMap;

pub const CLOSING_CHAR: u8 = !0;
pub const INVALID_CHAR: u8 = (!0) - 1;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind<'a> {
    Ident(u32),
    TypeIdent(u32),
    IntLiteral(i32),
    StringLiteral(&'a str),
    CharLiteral(u8),

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

pub const MAIN_SYMBOL: u32 = 0;
pub const PRINTF_SYMBOL: u32 = 1;
pub const EXIT_SYMBOL: u32 = 2;

pub struct Symbols<'a> {
    pub translate: HashMap<&'a str, u32>,
    pub names: Vec<&'a str>,
}

impl<'a> Symbols<'a> {
    pub fn new() -> Self {
        let mut new_symbols = Self {
            names: Vec::new(),
            translate: HashMap::new(),
        };

        assert_eq!(MAIN_SYMBOL, new_symbols.translate_add("main"));
        assert_eq!(PRINTF_SYMBOL, new_symbols.translate_add("printf"));
        assert_eq!(EXIT_SYMBOL, new_symbols.translate_add("exit"));
        return new_symbols;
    }

    pub fn translate_add(&mut self, word: &'a str) -> u32 {
        if let Some(id) = self.translate.get(word) {
            return *id;
        } else {
            let idx = self.names.len() as u32;
            self.names.push(word);
            self.translate.insert(word, idx);
            return idx;
        }
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
    symbols: &mut Symbols<'a>,
    file: u32,
    data: &'a str,
) -> Result<Vec<Token<'b>>, Error> {
    let mut toks = Vec::new();
    let bytes = data.as_bytes();
    let mut current = 0;
    let mut tok = lex_token(buckets, symbols, file, bytes, &mut current)?;
    toks.push(tok);

    while tok.kind != TokenKind::End {
        tok = lex_token(buckets, symbols, file, bytes, &mut current)?;
        toks.push(tok);
    }

    return Ok(toks);
}

pub fn lex_token<'a, 'b>(
    buckets: BucketListRef<'b>,
    symbols: &mut Symbols<'a>,
    file: u32,
    data: &'a [u8],
    current: &mut usize,
) -> Result<Token<'b>, Error> {
    let whitespace = [b' ', b'\t', b'\n', b'\r'];
    while peek_eqs(data, current, &whitespace) {
        *current += 1;
    }

    if *current == data.len() {
        return Token::newr(TokenKind::End, *current..*current);
    }

    let begin = *current;
    *current += 1;

    match data[begin] {
        x if (x >= b'a' && x <= b'z') => {
            let mut cur = data[*current];

            while (cur >= b'a' && cur <= b'z')
                || (cur >= b'A' && cur <= b'Z')
                || cur == b'_'
                || (cur >= b'0' && cur <= b'9')
            {
                *current += 1;
                cur = data[*current];
            }

            let word = unsafe { std::str::from_utf8_unchecked(&data[begin..*current]) };
            match word {
                "if" => return Token::newr(TokenKind::If, begin..*current),
                "else" => return Token::newr(TokenKind::Else, begin..*current),
                "do" => return Token::newr(TokenKind::Do, begin..*current),
                "while" => return Token::newr(TokenKind::While, begin..*current),
                "for" => return Token::newr(TokenKind::For, begin..*current),
                "break" => return Token::newr(TokenKind::Break, begin..*current),
                "continue" => return Token::newr(TokenKind::Continue, begin..*current),
                "return" => return Token::newr(TokenKind::Return, begin..*current),
                "struct" => return Token::newr(TokenKind::Struct, begin..*current),
                "typedef" => return Token::newr(TokenKind::Typedef, begin..*current),
                "void" => return Token::newr(TokenKind::Void, begin..*current),
                "int" => return Token::newr(TokenKind::Int, begin..*current),
                "char" => return Token::newr(TokenKind::Char, begin..*current),
                word => {
                    let id = symbols.translate_add(word);
                    if word.ends_with("_t") || word == "va_list" {
                        return Token::newr(TokenKind::TypeIdent(id), begin..*current);
                    } else {
                        return Token::newr(TokenKind::Ident(id), begin..*current);
                    }
                }
            }
        }

        x if (x >= b'A' && x <= b'Z') => {
            let mut cur = data[*current];

            while (cur >= b'a' && cur <= b'z')
                || (cur >= b'A' && cur <= b'Z')
                || cur == b'_'
                || (cur >= b'0' && cur <= b'9')
            {
                *current += 1;
                cur = data[*current];
            }

            let word = unsafe { std::str::from_utf8_unchecked(&data[begin..*current]) };
            let id = symbols.translate_add(word);

            return Token::newr(TokenKind::TypeIdent(id), begin..*current);
        }

        x if (x >= b'0' && x <= b'9') => {
            let mut int_value: i32 = (x - b'0') as i32;
            while peek_check(data, current, |b| b >= b'0' && b <= b'9') {
                int_value *= 10;
                int_value += (data[*current] - b'0') as i32;
                *current += 1;
            }

            println!("{}", int_value);
            return Token::newr(TokenKind::IntLiteral(int_value), begin..*current);
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
            return Token::newr(TokenKind::StringLiteral(string), begin..*current);
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

            return Token::newr(TokenKind::CharLiteral(byte), begin..*current);
        }

        b'{' => return Token::newr(TokenKind::LBrace, begin..*current),
        b'}' => return Token::newr(TokenKind::RBrace, begin..*current),
        b'(' => return Token::newr(TokenKind::LParen, begin..*current),
        b')' => return Token::newr(TokenKind::RParen, begin..*current),
        b'[' => return Token::newr(TokenKind::LBracket, begin..*current),
        b']' => return Token::newr(TokenKind::RBracket, begin..*current),
        b'~' => return Token::newr(TokenKind::Tilde, begin..*current),
        b';' => return Token::newr(TokenKind::Semicolon, begin..*current),
        b',' => return Token::newr(TokenKind::Comma, begin..*current),

        b'.' => {
            if peek_eq(data, current, b'.') {
                *current += 1;
                if peek_eq(data, current, b'.') {
                    *current += 1;
                    return Token::newr(TokenKind::DotDotDot, begin..*current);
                }

                return Err(invalid_token(file, begin, *current));
            }

            return Token::newr(TokenKind::Dot, begin..*current);
        }
        b'+' => {
            if peek_eq(data, current, b'+') {
                *current += 1;
                return Token::newr(TokenKind::PlusPlus, begin..*current);
            } else if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::PlusEq, begin..*current);
            } else {
                return Token::newr(TokenKind::Plus, begin..*current);
            }
        }
        b'-' => {
            if peek_eq(data, current, b'-') {
                *current += 1;
                return Token::newr(TokenKind::DashDash, begin..*current);
            } else if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::DashEq, begin..*current);
            } else if peek_eq(data, current, b'>') {
                *current += 1;
                return Token::newr(TokenKind::Arrow, begin..*current);
            } else {
                return Token::newr(TokenKind::Dash, begin..*current);
            }
        }
        b'/' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::SlashEq, begin..*current);
            } else {
                return Token::newr(TokenKind::Slash, begin..*current);
            }
        }
        b'*' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::StarEq, begin..*current);
            } else {
                return Token::newr(TokenKind::Star, begin..*current);
            }
        }
        b'%' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::PercentEq, begin..*current);
            } else {
                return Token::newr(TokenKind::Percent, begin..*current);
            }
        }
        b'>' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::Geq, begin..*current);
            } else {
                return Token::newr(TokenKind::Gt, begin..*current);
            }
        }
        b'<' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::Leq, begin..*current);
            } else {
                return Token::newr(TokenKind::Gt, begin..*current);
            }
        }
        b'!' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::Neq, begin..*current);
            } else {
                return Token::newr(TokenKind::Not, begin..*current);
            }
        }
        b'=' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::EqEq, begin..*current);
            } else {
                return Token::newr(TokenKind::Eq, begin..*current);
            }
        }
        b'|' => {
            if peek_eq(data, current, b'|') {
                *current += 1;
                return Token::newr(TokenKind::LineLine, begin..*current);
            } else if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::LineEq, begin..*current);
            } else {
                return Token::newr(TokenKind::Line, begin..*current);
            }
        }
        b'&' => {
            if peek_eq(data, current, b'&') {
                *current += 1;
                return Token::newr(TokenKind::AmpAmp, begin..*current);
            } else if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::AmpEq, begin..*current);
            } else {
                return Token::newr(TokenKind::Amp, begin..*current);
            }
        }
        b'^' => {
            if peek_eq(data, current, b'=') {
                *current += 1;
                return Token::newr(TokenKind::CaretEq, begin..*current);
            } else {
                return Token::newr(TokenKind::Caret, begin..*current);
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
