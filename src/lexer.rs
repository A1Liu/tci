use crate::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Ident(u32),
    TypeIdent(u32),
    IntLiteral(i32),

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
pub struct Token {
    pub kind: TokenKind,
    pub range: Range,
}

impl Token {
    pub fn new(kind: TokenKind, range: core::ops::Range<usize>) -> Self {
        Self {
            kind,
            range: r(range.start as u32, range.end as u32),
        }
    }
}

pub const MAIN_SYMBOL: u32 = 0;
pub const _MAIN_SYMBOL: u32 = 1;

pub struct Symbols<'a> {
    pub names: Vec<&'a str>,
    pub translate: HashMap<&'a str, u32>,
}

impl<'a> Symbols<'a> {
    pub fn new() -> Self {
        let mut new_symbols = Self {
            names: Vec::new(),
            translate: HashMap::new(),
        };

        assert_eq!(MAIN_SYMBOL, new_symbols.translate_add("main"));
        assert_eq!(_MAIN_SYMBOL, new_symbols.translate_add("_main"));
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

pub fn lex_file<'a>(symbols: &mut Symbols<'a>, data: &'a str) -> Vec<Token> {
    let mut toks = Vec::new();
    let bytes = data.as_bytes();
    let mut current = 0;
    let mut tok = lex_token(symbols, bytes, &mut current);
    toks.push(tok);

    while tok.kind != TokenKind::End {
        tok = lex_token(symbols, bytes, &mut current);
        toks.push(tok);
    }

    return toks;
}

pub fn lex_token<'a>(symbols: &mut Symbols<'a>, data: &'a [u8], current: &mut usize) -> Token {
    while *current < data.len()
        && match data[*current] {
            b' ' | b'\t' | b'\n' => true,
            _ => false,
        }
    {
        *current += 1;
    }

    if *current == data.len() {
        return Token::new(TokenKind::End, *current..*current);
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
                "if" => return Token::new(TokenKind::If, begin..*current),
                "else" => return Token::new(TokenKind::Else, begin..*current),
                "do" => return Token::new(TokenKind::Do, begin..*current),
                "while" => return Token::new(TokenKind::While, begin..*current),
                "for" => return Token::new(TokenKind::For, begin..*current),
                "break" => return Token::new(TokenKind::Break, begin..*current),
                "continue" => return Token::new(TokenKind::Continue, begin..*current),
                "return" => return Token::new(TokenKind::Return, begin..*current),
                "struct" => return Token::new(TokenKind::Struct, begin..*current),
                "typedef" => return Token::new(TokenKind::Typedef, begin..*current),
                "void" => return Token::new(TokenKind::Void, begin..*current),
                "int" => return Token::new(TokenKind::Int, begin..*current),
                "char" => return Token::new(TokenKind::Char, begin..*current),
                word => {
                    let id = symbols.translate_add(word);
                    return Token::new(TokenKind::Ident(id), begin..*current);
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

            return Token::new(TokenKind::TypeIdent(id), begin..*current);
        }

        b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' => {
            let mut int_value: i32 = 0;
            while data[*current] >= b'0' && data[*current] <= b'9' {
                *current += 1;
                int_value *= 10;
                int_value += (data[*current] - b'0') as i32;
            }
            return Token::new(TokenKind::IntLiteral(int_value), begin..*current);
        }

        b'{' => return Token::new(TokenKind::LBrace, begin..*current),
        b'}' => return Token::new(TokenKind::RBrace, begin..*current),
        b'(' => return Token::new(TokenKind::LParen, begin..*current),
        b')' => return Token::new(TokenKind::RParen, begin..*current),
        b'[' => return Token::new(TokenKind::LBracket, begin..*current),
        b']' => return Token::new(TokenKind::RBracket, begin..*current),
        b'~' => return Token::new(TokenKind::Tilde, begin..*current),
        b'.' => return Token::new(TokenKind::Dot, begin..*current),
        b';' => return Token::new(TokenKind::Semicolon, begin..*current),
        b',' => return Token::new(TokenKind::Comma, begin..*current),

        b'+' => {
            if data[*current] == b'+' {
                *current += 1;
                return Token::new(TokenKind::PlusPlus, begin..*current);
            } else if data[*current] == b'=' {
                *current += 1;
                return Token::new(TokenKind::PlusEq, begin..*current);
            } else {
                return Token::new(TokenKind::Plus, begin..*current);
            }
        }
        b'-' => {
            if data[*current] == b'-' {
                *current += 1;
                return Token::new(TokenKind::DashDash, begin..*current);
            } else if data[*current] == b'=' {
                *current += 1;
                return Token::new(TokenKind::DashEq, begin..*current);
            } else if data[*current] == b'>' {
                *current += 1;
                return Token::new(TokenKind::Arrow, begin..*current);
            } else {
                return Token::new(TokenKind::Dash, begin..*current);
            }
        }
        b'/' => {
            if data[*current] == b'=' {
                *current += 1;
                return Token::new(TokenKind::SlashEq, begin..*current);
            } else {
                return Token::new(TokenKind::Slash, begin..*current);
            }
        }
        b'*' => {
            if data[*current] == b'=' {
                *current += 1;
                return Token::new(TokenKind::StarEq, begin..*current);
            } else {
                return Token::new(TokenKind::Star, begin..*current);
            }
        }
        b'%' => {
            if data[*current] == b'=' {
                *current += 1;
                return Token::new(TokenKind::PercentEq, begin..*current);
            } else {
                return Token::new(TokenKind::Percent, begin..*current);
            }
        }
        b'>' => {
            if data[*current] == b'=' {
                *current += 1;
                return Token::new(TokenKind::Geq, begin..*current);
            } else {
                return Token::new(TokenKind::Gt, begin..*current);
            }
        }
        b'<' => {
            if data[*current] == b'+' {
                *current += 1;
                return Token::new(TokenKind::Leq, begin..*current);
            } else {
                return Token::new(TokenKind::Gt, begin..*current);
            }
        }
        b'!' => {
            if data[*current] == b'=' {
                *current += 1;
                return Token::new(TokenKind::Neq, begin..*current);
            } else {
                return Token::new(TokenKind::Not, begin..*current);
            }
        }
        b'=' => {
            if data[*current] == b'=' {
                *current += 1;
                return Token::new(TokenKind::EqEq, begin..*current);
            } else {
                return Token::new(TokenKind::Eq, begin..*current);
            }
        }
        b'|' => {
            if data[*current] == b'|' {
                *current += 1;
                return Token::new(TokenKind::LineLine, begin..*current);
            } else if data[*current] == b'=' {
                *current += 1;
                return Token::new(TokenKind::LineEq, begin..*current);
            } else {
                return Token::new(TokenKind::Line, begin..*current);
            }
        }
        b'&' => {
            if data[*current] == b'&' {
                *current += 1;
                return Token::new(TokenKind::AmpAmp, begin..*current);
            } else if data[*current] == b'=' {
                *current += 1;
                return Token::new(TokenKind::AmpEq, begin..*current);
            } else {
                return Token::new(TokenKind::Amp, begin..*current);
            }
        }
        b'^' => {
            if data[*current] == b'=' {
                *current += 1;
                return Token::new(TokenKind::CaretEq, begin..*current);
            } else {
                return Token::new(TokenKind::Caret, begin..*current);
            }
        }

        _ => return Token::new(TokenKind::Invalid, begin..*current),
    }
}
