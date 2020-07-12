use core::ops::Range;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    Ident(u32),
    IntLiteral(u32),

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

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub range: Range<u32>,
}

impl Token {
    pub fn new(kind: TokenKind, range: Range<usize>) -> Self {
        Self {
            kind,
            range: (range.start as u32)..(range.end as u32),
        }
    }
}

pub struct Lexer<'a> {
    pub data: &'a [u8],
    pub current: usize,
    pub symbols: Vec<&'a str>,
    pub translate: HashMap<&'a str, u32>,
}

impl<'a> Lexer<'a> {
    pub fn new(data: &'a str) -> Self {
        Self {
            data: data.as_bytes(),
            current: 0,
            symbols: Vec::new(),
            translate: HashMap::new(),
        }
    }

    pub fn next(&mut self) -> Token {
        while self.current < self.data.len()
            && match self.data[self.current] {
                b' ' | b'\t' | b'\n' => true,
                _ => false,
            }
        {
            self.current += 1;
        }

        if self.current == self.data.len() {
            return Token::new(TokenKind::End, self.current..self.current);
        }

        let begin = self.current;
        self.current += 1;

        let is_word = match self.data[begin] {
            x if (x >= b'a' && x <= b'z') || (x >= b'A' && x <= b'Z') => true,

            b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' => false,

            b'{' => return Token::new(TokenKind::LBrace, begin..self.current),
            b'}' => return Token::new(TokenKind::RBrace, begin..self.current),
            b'(' => return Token::new(TokenKind::LParen, begin..self.current),
            b')' => return Token::new(TokenKind::RParen, begin..self.current),
            b'[' => return Token::new(TokenKind::LBracket, begin..self.current),
            b']' => return Token::new(TokenKind::RBracket, begin..self.current),
            b'~' => return Token::new(TokenKind::Tilde, begin..self.current),
            b'.' => return Token::new(TokenKind::Dot, begin..self.current),
            b';' => return Token::new(TokenKind::Semicolon, begin..self.current),
            b',' => return Token::new(TokenKind::Comma, begin..self.current),

            b'+' => {
                if self.data[self.current] == b'+' {
                    self.current += 1;
                    return Token::new(TokenKind::PlusPlus, begin..self.current);
                } else if self.data[self.current] == b'=' {
                    self.current += 1;
                    return Token::new(TokenKind::PlusEq, begin..self.current);
                } else {
                    return Token::new(TokenKind::Plus, begin..self.current);
                }
            }
            b'-' => {
                if self.data[self.current] == b'-' {
                    self.current += 1;
                    return Token::new(TokenKind::DashDash, begin..self.current);
                } else if self.data[self.current] == b'=' {
                    self.current += 1;
                    return Token::new(TokenKind::DashEq, begin..self.current);
                } else if self.data[self.current] == b'>' {
                    self.current += 1;
                    return Token::new(TokenKind::Arrow, begin..self.current);
                } else {
                    return Token::new(TokenKind::Dash, begin..self.current);
                }
            }
            b'/' => {
                if self.data[self.current] == b'=' {
                    self.current += 1;
                    return Token::new(TokenKind::SlashEq, begin..self.current);
                } else {
                    return Token::new(TokenKind::Slash, begin..self.current);
                }
            }
            b'*' => {
                if self.data[self.current] == b'=' {
                    self.current += 1;
                    return Token::new(TokenKind::StarEq, begin..self.current);
                } else {
                    return Token::new(TokenKind::Star, begin..self.current);
                }
            }
            b'%' => {
                if self.data[self.current] == b'=' {
                    self.current += 1;
                    return Token::new(TokenKind::PercentEq, begin..self.current);
                } else {
                    return Token::new(TokenKind::Percent, begin..self.current);
                }
            }
            b'>' => {
                if self.data[self.current] == b'=' {
                    self.current += 1;
                    return Token::new(TokenKind::Geq, begin..self.current);
                } else {
                    return Token::new(TokenKind::Gt, begin..self.current);
                }
            }
            b'<' => {
                if self.data[self.current] == b'+' {
                    self.current += 1;
                    return Token::new(TokenKind::Leq, begin..self.current);
                } else {
                    return Token::new(TokenKind::Gt, begin..self.current);
                }
            }
            b'!' => {
                if self.data[self.current] == b'=' {
                    self.current += 1;
                    return Token::new(TokenKind::Neq, begin..self.current);
                } else {
                    return Token::new(TokenKind::Not, begin..self.current);
                }
            }
            b'=' => {
                if self.data[self.current] == b'=' {
                    self.current += 1;
                    return Token::new(TokenKind::EqEq, begin..self.current);
                } else {
                    return Token::new(TokenKind::Eq, begin..self.current);
                }
            }
            b'|' => {
                if self.data[self.current] == b'|' {
                    self.current += 1;
                    return Token::new(TokenKind::LineLine, begin..self.current);
                } else if self.data[self.current] == b'=' {
                    self.current += 1;
                    return Token::new(TokenKind::LineEq, begin..self.current);
                } else {
                    return Token::new(TokenKind::Line, begin..self.current);
                }
            }
            b'&' => {
                if self.data[self.current] == b'&' {
                    self.current += 1;
                    return Token::new(TokenKind::AmpAmp, begin..self.current);
                } else if self.data[self.current] == b'=' {
                    self.current += 1;
                    return Token::new(TokenKind::AmpEq, begin..self.current);
                } else {
                    return Token::new(TokenKind::Amp, begin..self.current);
                }
            }
            b'^' => {
                if self.data[self.current] == b'=' {
                    self.current += 1;
                    return Token::new(TokenKind::CaretEq, begin..self.current);
                } else {
                    return Token::new(TokenKind::Caret, begin..self.current);
                }
            }

            _ => return Token::new(TokenKind::Invalid, begin..self.current),
        };

        if !is_word {
            let mut int_value: u32 = 0;
            while self.data[self.current] >= b'0' && self.data[self.current] <= b'9' {
                self.current += 1;
                int_value *= 10;
                int_value += (self.data[self.current] - b'0') as u32;
            }
            return Token::new(TokenKind::IntLiteral(int_value), begin..self.current);
        }

        let mut cur = self.data[self.current];
        while (cur >= b'a' && cur <= b'z')
            || (cur >= b'A' && cur <= b'Z')
            || cur == b'_'
            || (cur >= b'0' && cur <= b'9')
        {
            self.current += 1;
            cur = self.data[self.current];
        }

        let word = unsafe { std::str::from_utf8_unchecked(&self.data[begin..self.current]) };

        match word {
            "if" => return Token::new(TokenKind::If, begin..self.current),
            "else" => return Token::new(TokenKind::Else, begin..self.current),
            "do" => return Token::new(TokenKind::Do, begin..self.current),
            "while" => return Token::new(TokenKind::While, begin..self.current),
            "for" => return Token::new(TokenKind::For, begin..self.current),
            "break" => return Token::new(TokenKind::Break, begin..self.current),
            "continue" => return Token::new(TokenKind::Continue, begin..self.current),
            "return" => return Token::new(TokenKind::Return, begin..self.current),
            "struct" => return Token::new(TokenKind::Struct, begin..self.current),
            "typedef" => return Token::new(TokenKind::Typedef, begin..self.current),
            "void" => return Token::new(TokenKind::Void, begin..self.current),
            "int" => return Token::new(TokenKind::Int, begin..self.current),
            "char" => return Token::new(TokenKind::Char, begin..self.current),
            x => {
                let idx = self.symbols.len() as u32;
                self.symbols.push(x);
                self.translate.insert(x, idx);
                return Token::new(TokenKind::Ident(idx), begin..self.current);
            }
        }
    }
}
