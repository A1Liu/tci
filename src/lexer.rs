use crate::buckets::*;
use crate::filedb::*;
use crate::util::*;
use codespan_reporting::files::Files;
use std::collections::{HashMap, HashSet};

pub const CLOSING_CHAR: u8 = !0;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind<'a> {
    Ident(u32),
    TypeIdent(u32),
    IntLiteral(i32),
    StringLiteral(&'a str),
    CharLiteral(i8),

    Include(u32),
    IncludeSys(u32),
    MacroDef(u32),
    FuncMacroDef(u32),
    MacroDefEnd,

    Void,
    Char,
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

    If,
    Else,
    Do,
    While,
    For,
    Break,
    Continue,
    Return,

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
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub loc: CodeLoc,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, range: core::ops::Range<usize>, file: u32) -> Self {
        Self {
            kind,
            loc: l(range.start as u32, range.end as u32, file),
        }
    }
}

#[inline]
pub fn invalid_token(file: u32, begin: usize, end: usize) -> Error {
    return error!(
        "invalid token",
        l(begin as u32, end as u32, file),
        "token found here"
    );
}

pub type TokenDb<'a> = HashMap<u32, &'a [Token<'a>]>;

const WHITESPACE: [u8; 2] = [b' ', b'\t'];
const CRLF: [u8; 2] = [b'\r', b'\n'];

pub fn lex_file<'b>(
    buckets: BucketListRef<'b>,
    token_db: &mut TokenDb<'b>,
    symbols: &mut FileDb,
    file: u32,
) -> Result<&'b [Token<'b>], Error> {
    if let Some(toks) = token_db.get(&file) {
        return Ok(toks);
    }

    let mut incomplete = HashSet::new();
    let tokens = Lexer::new(file).lex_file(buckets, &mut incomplete, token_db, symbols)?;
    token_db.insert(file, tokens);
    return Ok(tokens);
}

pub struct Lexer<'a> {
    file: u32,
    current: usize,
    output: Vec<Token<'a>>,
}

impl<'b> Lexer<'b> {
    pub fn new(file: u32) -> Self {
        Self {
            file,
            current: 0,
            output: Vec::new(),
        }
    }

    pub fn lex_file(
        mut self,
        mut buckets: BucketListRef<'b>,
        incomplete: &mut HashSet<u32>,
        token_db: &mut TokenDb<'b>,
        symbols: &mut FileDb,
    ) -> Result<&'b [Token<'b>], Error> {
        let bytes = symbols.source(self.file).unwrap().as_bytes();

        self.lex_macro(buckets, incomplete, token_db, symbols, bytes)?;

        let mut done = self.lex_macro_or_token(buckets, incomplete, token_db, symbols, bytes)?;

        while !done {
            done = self.lex_macro_or_token(buckets, incomplete, token_db, symbols, bytes)?;

            while let Some(next) = buckets.next() {
                buckets = next;
            }
        }

        return Ok(buckets.add_array(self.output));
    }

    pub fn lex_macro_or_token(
        &mut self,
        buckets: BucketListRef<'b>,
        incomplete: &mut HashSet<u32>,
        token_db: &mut TokenDb<'b>,
        symbols: &mut FileDb,
        data: &[u8],
    ) -> Result<bool, Error> {
        loop {
            while self.peek_eqs(data, &WHITESPACE) {
                self.current += 1;
            }

            if self.peek_eq_series(data, &[b'/', b'/']) {
                self.current += 2;
                while self.peek_neq(data, b'\n') && self.peek_neq_series(data, &CRLF) {
                    self.current += 1;
                }
            } else if self.peek_eq_series(data, &[b'/', b'*']) {
                self.current += 2;
                while self.peek_neq_series(data, &[b'*', b'/']) {
                    self.current += 1;
                }

                self.current += 2;
                continue;
            }

            if self.peek_eq(data, b'\n') {
                self.current += 1;
            } else if self.peek_eq_series(data, &CRLF) {
                self.current += 2;
            } else {
                break;
            }

            self.lex_macro(buckets, incomplete, token_db, symbols, data)?;
        }

        if self.current == data.len() {
            return Ok(true);
        }

        let tok = self.lex_token(buckets, symbols, data)?;
        self.output.push(tok);
        return Ok(false);
    }

    pub fn lex_macro(
        &mut self,
        buckets: BucketListRef<'b>,
        incomplete: &mut HashSet<u32>,
        token_db: &mut TokenDb<'b>,
        symbols: &mut FileDb,
        data: &[u8],
    ) -> Result<(), Error> {
        if self.peek_eq(data, b'#') {
            self.current += 1;
        } else {
            return Ok(());
        }

        // macros!
        let begin = self.current;
        while self.peek_neqs(data, &WHITESPACE) {
            self.current += 1;
        }

        let directive = unsafe { std::str::from_utf8_unchecked(&data[begin..self.current]) };
        match directive {
            "define" => {
                while self.peek_eqs(data, &WHITESPACE) {
                    self.current += 1;
                }

                let ident_begin = self.current;
                if ident_begin == data.len() {
                    return Err(error!(
                        "unexpected end of file",
                        l(ident_begin as u32, begin as u32, self.file),
                        "EOF found here"
                    ));
                }

                while self.peek_check(data, is_ident_char) {
                    self.current += 1;
                }

                // Don't add the empty string
                if self.current - ident_begin == 0 {
                    return Err(error!(
                        "expected an identifer for macro declaration",
                        l(ident_begin as u32, ident_begin as u32 + 1, self.file),
                        "This should be an identifier"
                    ));
                }

                let id = symbols.translate_add(ident_begin..self.current, self.file);

                macro_rules! consume_whitespace_macro {
                    () => {
                        while self.peek_eqs(data, &WHITESPACE) {
                            self.current += 1;
                        }

                        if self.current == data.len() {
                            break;
                        }

                        if self.peek_eq_series(data, &[b'/', b'/']) {
                            self.current += 2;
                            while self.peek_neq(data, b'\n') && self.peek_neq_series(data, &CRLF) {
                                self.current += 1;
                            }
                        } else if self.peek_eq_series(data, &[b'/', b'*']) {
                            self.current += 2;
                            while self.peek_neq_series(data, &[b'*', b'/']) {
                                self.current += 1;
                            }

                            self.current += 2;
                            continue;
                        }

                        if self.peek_eq(data, b'\n') {
                            break;
                        } else if self.peek_eq_series(data, &CRLF) {
                            break;
                        } else if self.peek_eq_series(data, &[b'\\', b'\n']) {
                            self.current += 2;
                            continue;
                        } else if self.peek_eq_series(data, &[b'\\', b'\r', b'\n']) {
                            self.current += 3;
                            continue;
                        }
                    };
                }

                if !self.peek_eq(data, b'(') {
                    self.output.push(Token::new(
                        TokenKind::MacroDef(id),
                        begin..self.current,
                        self.file,
                    ));

                    loop {
                        consume_whitespace_macro!();
                        let tok = self.lex_token(buckets, symbols, data)?;
                        self.output.push(tok);
                    }

                    self.output.push(Token::new(
                        TokenKind::MacroDefEnd,
                        self.current..self.current,
                        self.file,
                    ));

                    return Ok(());
                }

                self.output.push(Token::new(
                    TokenKind::FuncMacroDef(id),
                    begin..self.current,
                    self.file,
                ));

                loop {
                    consume_whitespace_macro!();
                    let tok = self.lex_token(buckets, symbols, data)?;
                    self.output.push(tok);
                }

                self.output.push(Token::new(
                    TokenKind::MacroDefEnd,
                    self.current..self.current,
                    self.file,
                ));

                return Ok(());
            }
            "include" => {
                while self.peek_eqs(data, &WHITESPACE) {
                    self.current += 1;
                }

                if self.peek_eq(data, b'"') {
                    self.current += 1;
                    let name_begin = self.current;
                    while self.peek_neq(data, b'"') {
                        self.current += 1;
                    }

                    let id = symbols.translate_add(name_begin..self.current, self.file);
                    self.current += 1;
                    if !self.peek_eq(data, b'\n') && !self.peek_eq_series(data, &CRLF) {
                        return Err(expected_newline("include", begin, self.current, self.file));
                    }

                    let map_err = |err| {
                        error!(
                            "Error finding file",
                            l(begin as u32, self.current as u32, self.file),
                            format!("got error '{}'", err)
                        )
                    };
                    let include_id = symbols.add_from_symbols(self.file, id).map_err(map_err)?;
                    self.output.push(Token::new(
                        TokenKind::Include(include_id),
                        begin..self.current,
                        self.file,
                    ));

                    if incomplete.contains(&include_id) {
                        return Err(error!(
                            "include cycle detected",
                            l(begin as u32, self.current as u32, self.file),
                            "found here"
                        ));
                    }

                    if let Some(_) = token_db.get(&include_id) {
                        return Ok(());
                    }

                    incomplete.insert(include_id);
                    let toks =
                        Lexer::new(include_id).lex_file(buckets, incomplete, token_db, symbols)?;
                    token_db.insert(include_id, toks);
                    incomplete.remove(&include_id);
                    return Ok(());
                } else if self.peek_eq(data, b'<') {
                    self.current += 1;
                    let name_begin = self.current;

                    while self.peek_neq(data, b'>') {
                        self.current += 1;
                    }

                    let id = symbols.translate_add(name_begin..self.current, self.file);
                    self.current += 1;
                    if !self.peek_eq(data, b'\n') && !self.peek_eq_series(data, &CRLF) {
                        return Err(expected_newline("include", begin, self.current, self.file));
                    }
                    self.output.push(Token::new(
                        TokenKind::IncludeSys(id),
                        begin..self.current,
                        self.file,
                    ));

                    if let Some(_) = token_db.get(&id) {
                        return Ok(());
                    }

                    let toks = Lexer::new(id).lex_file(buckets, incomplete, token_db, symbols)?;
                    token_db.insert(id, toks);
                    return Ok(());
                }
            }
            _ => {
                return Err(error!(
                    "invalid compiler directive",
                    l(begin as u32, self.current as u32, self.file),
                    "directive found here"
                ));
            }
        }

        return Ok(());
    }

    pub fn lex_token(
        &mut self,
        buckets: BucketListRef<'b>,
        symbols: &mut FileDb,
        data: &[u8],
    ) -> Result<Token<'b>, Error> {
        let begin = self.current;
        self.current += 1;

        macro_rules! ret_tok {
            ($arg1:expr) => {{
                return Ok(Token::new($arg1, begin..self.current, self.file));
            }};
        }

        match data[begin] {
            x if (x >= b'a' && x <= b'z') || x == b'_' => {
                while self.peek_check(data, is_ident_char) {
                    self.current += 1;
                }

                let word = unsafe { std::str::from_utf8_unchecked(&data[begin..self.current]) };
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
                    "union" => ret_tok!(TokenKind::Union),
                    "enum" => ret_tok!(TokenKind::Enum),
                    "typedef" => ret_tok!(TokenKind::Typedef),
                    "sizeof" => ret_tok!(TokenKind::Sizeof),
                    "void" => ret_tok!(TokenKind::Void),
                    "char" => ret_tok!(TokenKind::Char),
                    "int" => ret_tok!(TokenKind::Int),
                    "long" => ret_tok!(TokenKind::Long),
                    "unsigned" => ret_tok!(TokenKind::Unsigned),
                    "signed" => ret_tok!(TokenKind::Signed),
                    "float" => ret_tok!(TokenKind::Float),
                    "double" => ret_tok!(TokenKind::Double),
                    word => {
                        let id = symbols.translate_add(begin..self.current, self.file);
                        if word.ends_with("_t") || word == "va_list" {
                            ret_tok!(TokenKind::TypeIdent(id));
                        } else {
                            ret_tok!(TokenKind::Ident(id));
                        }
                    }
                }
            }

            x if (x >= b'A' && x <= b'Z') => {
                while self.peek_check(data, is_ident_char) {
                    self.current += 1;
                }

                let id = symbols.translate_add(begin..self.current, self.file);
                ret_tok!(TokenKind::TypeIdent(id));
            }

            x if (x >= b'0' && x <= b'9') => {
                let mut int_value: i32 = (x - b'0') as i32;
                while self.peek_check(data, |b| b >= b'0' && b <= b'9') {
                    int_value *= 10;
                    int_value += (data[self.current] - b'0') as i32;
                    self.current += 1;
                }

                ret_tok!(TokenKind::IntLiteral(int_value));
            }

            b'\"' => {
                let mut cur = self.lex_character(b'\"', data)?;
                let mut chars = Vec::new();
                while cur != CLOSING_CHAR {
                    chars.push(cur);
                    cur = self.lex_character(b'\"', data)?;
                }

                let string = unsafe { std::str::from_utf8_unchecked(&chars) };
                let string = buckets.add_str(string);
                ret_tok!(TokenKind::StringLiteral(string));
            }

            b'\'' => {
                let byte = self.lex_character(b'\'', data)?;
                if byte == CLOSING_CHAR {
                    return Err(error!(
                        "empty character literal",
                        l(begin as u32, self.current as u32, self.file),
                        "found here"
                    ));
                }

                let closing = self.expect(data)?;
                if closing != b'\'' {
                    return Err(error!(
                        "expected closing single quote",
                        l(begin as u32, self.current as u32, self.file),
                        "this should be a closing single quote"
                    ));
                }

                ret_tok!(TokenKind::CharLiteral(byte as i8));
            }

            b'{' => ret_tok!(TokenKind::LBrace),
            b'}' => ret_tok!(TokenKind::RBrace),
            b'(' => ret_tok!(TokenKind::LParen),
            b')' => ret_tok!(TokenKind::RParen),
            b'[' => ret_tok!(TokenKind::LBracket),
            b']' => ret_tok!(TokenKind::RBracket),
            b'~' => ret_tok!(TokenKind::Tilde),
            b';' => ret_tok!(TokenKind::Semicolon),
            b':' => ret_tok!(TokenKind::Colon),
            b',' => ret_tok!(TokenKind::Comma),
            b'?' => ret_tok!(TokenKind::Question),

            b'.' => {
                if self.peek_eq(data, b'.') {
                    self.current += 1;
                    if self.peek_eq(data, b'.') {
                        self.current += 1;
                        ret_tok!(TokenKind::DotDotDot);
                    }

                    return Err(invalid_token(self.file, begin, self.current));
                }

                ret_tok!(TokenKind::Dot);
            }
            b'+' => {
                if self.peek_eq(data, b'+') {
                    self.current += 1;
                    ret_tok!(TokenKind::PlusPlus);
                } else if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::PlusEq);
                } else {
                    ret_tok!(TokenKind::Plus);
                }
            }
            b'-' => {
                if self.peek_eq(data, b'-') {
                    self.current += 1;
                    ret_tok!(TokenKind::DashDash);
                } else if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::DashEq);
                } else if self.peek_eq(data, b'>') {
                    self.current += 1;
                    ret_tok!(TokenKind::Arrow);
                } else {
                    ret_tok!(TokenKind::Dash);
                }
            }
            b'/' => {
                if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::SlashEq);
                } else {
                    ret_tok!(TokenKind::Slash);
                }
            }
            b'*' => {
                if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::StarEq);
                } else {
                    ret_tok!(TokenKind::Star);
                }
            }
            b'%' => {
                if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::PercentEq);
                } else {
                    ret_tok!(TokenKind::Percent);
                }
            }
            b'>' => {
                if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::Geq);
                } else {
                    ret_tok!(TokenKind::Gt);
                }
            }
            b'<' => {
                if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::Leq);
                } else {
                    ret_tok!(TokenKind::Lt);
                }
            }
            b'!' => {
                if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::Neq);
                } else {
                    ret_tok!(TokenKind::Bang);
                }
            }
            b'=' => {
                if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::EqEq);
                } else {
                    ret_tok!(TokenKind::Eq);
                }
            }
            b'|' => {
                if self.peek_eq(data, b'|') {
                    self.current += 1;
                    ret_tok!(TokenKind::LineLine);
                } else if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::LineEq);
                } else {
                    ret_tok!(TokenKind::Line);
                }
            }
            b'&' => {
                if self.peek_eq(data, b'&') {
                    self.current += 1;
                    ret_tok!(TokenKind::AmpAmp);
                } else if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::AmpEq);
                } else {
                    ret_tok!(TokenKind::Amp);
                }
            }
            b'^' => {
                if self.peek_eq(data, b'=') {
                    self.current += 1;
                    ret_tok!(TokenKind::CaretEq);
                } else {
                    ret_tok!(TokenKind::Caret);
                }
            }

            x => {
                return Err(invalid_token(self.file, begin, self.current));
            }
        }
    }

    #[inline]
    pub fn expect(&mut self, data: &[u8]) -> Result<u8, Error> {
        if self.current == data.len() {
            return Err(error!("unexpected end of file"));
        }

        let cur = self.current;
        self.current += 1;
        return Ok(data[cur]);
    }

    #[inline]
    pub fn peek_expect(&self, data: &[u8]) -> Result<u8, Error> {
        if self.current == data.len() {
            return Err(error!("unexpected end of file"));
        }

        return Ok(data[self.current]);
    }

    #[inline]
    pub fn peek_check(&self, data: &[u8], checker: impl Fn(u8) -> bool) -> bool {
        if self.current >= data.len() {
            return false;
        }

        return checker(data[self.current]);
    }

    #[inline]
    pub fn peek_eq(&self, data: &[u8], byte: u8) -> bool {
        if self.current >= data.len() {
            return false;
        }

        return data[self.current] == byte;
    }

    pub fn peek_neq_series(&self, data: &[u8], bytes: &[u8]) -> bool {
        let byte_len = bytes.len();
        if self.current + bytes.len() > data.len() {
            return false;
        }

        let eq_slice = &data[(self.current)..(self.current + byte_len)];
        return eq_slice != bytes;
    }

    pub fn peek_eq_series(&self, data: &[u8], bytes: &[u8]) -> bool {
        let byte_len = bytes.len();
        if self.current + bytes.len() > data.len() {
            return false;
        }

        let eq_slice = &data[(self.current)..(self.current + byte_len)];
        return eq_slice == bytes;
    }

    #[inline]
    pub fn peek_neq(&self, data: &[u8], byte: u8) -> bool {
        if self.current >= data.len() {
            return false;
        }

        return data[self.current] != byte;
    }

    #[inline]
    pub fn peek_neqs(&self, data: &[u8], bytes: &[u8]) -> bool {
        if self.current >= data.len() {
            return false;
        }

        for byte in bytes {
            if data[self.current] == *byte {
                return false;
            }
        }

        return true;
    }

    #[inline]
    pub fn peek_eqs(&self, data: &[u8], bytes: &[u8]) -> bool {
        if self.current >= data.len() {
            return false;
        }

        for byte in bytes {
            if data[self.current] == *byte {
                return true;
            }
        }

        return false;
    }

    pub fn lex_character(&mut self, surround: u8, data: &[u8]) -> Result<u8, Error> {
        loop {
            let cur_b = self.expect(data)?;
            let cur: char = cur_b.into();

            if !cur.is_ascii() {
                return Err(error!(
                    "character is not valid ascii",
                    l(self.current as u32 - 1, self.current as u32, self.file),
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
                        l(self.current as u32 - 1, self.current as u32, self.file),
                        "invalid character here"
                    ));
                } else {
                    return Err(error!(
                        "invalid character found when parsing character literal",
                        l(self.current as u32 - 1, self.current as u32, self.file),
                        "invalid character here"
                    ));
                }
            }

            if cur_b != b'\\' {
                return Ok(cur_b);
            }

            match self.expect(data)? {
                b'n' => return Ok(b'\n'),
                b'\n' => continue,
                b'\'' => return Ok(b'\''),
                b'"' => return Ok(b'"'),
                _ => {
                    return Err(error!(
                        "invalid escape sequence",
                        l(self.current as u32 - 2, self.current as u32, self.file),
                        "invalid escape sequence here"
                    ))
                }
            }
        }
    }
}

pub fn is_ident_char(cur: u8) -> bool {
    (cur >= b'a' && cur <= b'z')
        || (cur >= b'A' && cur <= b'Z')
        || cur == b'_'
        || (cur >= b'0' && cur <= b'9')
}

#[inline]
pub fn expected_newline(
    directive_name: &'static str,
    begin: usize,
    current: usize,
    file: u32,
) -> Error {
    return error!(
        &format!("expected newline after {} directive", directive_name),
        l(begin as u32, current as u32, file),
        "directive here"
    );
}
