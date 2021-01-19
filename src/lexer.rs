use crate::buckets::*;
use crate::filedb::*;
use crate::util::*;
use codespan_reporting::files::Files;
use core::{mem, str};
use std::collections::HashMap;

pub const CLOSING_CHAR: u8 = !0;

#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum NumChar {
    // values
    _0 = 0,
    _1,
    _2,
    _3,
    _4,
    _5,
    _6,
    _7,
    _8,
    _9,

    _A,
    _B,
    _C,
    _D,
    _E,
    _F,

    _L,
    _X,
    _U,
    _INVALID,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Ident(u32),
    IntChar(NumChar),
    StringLit(&'static str),
    CharLit(i8),

    Whitespace,

    Pragma(&'static str),

    Void,
    Char,
    Int,
    Long,
    Float,
    Double,
    Unsigned,
    Static,
    Signed,
    Struct,
    Union,
    Enum,
    Sizeof,
    Typedef,
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
    Return,
    Goto,

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

    Unimplemented,
    Case,
    Const,
    Default,
    Extern,
    Switch,
    Short,
}

pub enum MacroTok {
    Tok(TokenKind),
}

#[derive(Debug, PartialEq)]
pub enum RawTok {
    Tok(TokenKind),
    Include(u32),
    Ifdef(u32),
    Ifndef(u32),
    Endif,
    Noop,
    Else,

    If,
    Define(u32),
    FuncDefine(u32),
    EndPPLine,
}

#[derive(Debug, Clone)]
pub enum Macro {
    Func {
        params: Vec<u32>,
        toks: Vec<TokenKind>,
    },
    Value(Vec<TokenKind>),
    Marker,
}

lazy_static! {
    pub static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenKind> = {
        let mut set = HashMap::new();
        set.insert("auto", TokenKind::Unimplemented);
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
        set.insert("register", TokenKind::Unimplemented);
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
        set.insert("volatile", TokenKind::Unimplemented);
        set.insert("while", TokenKind::While);
        set.insert("_Alignas", TokenKind::Unimplemented);
        set.insert("_Alignof", TokenKind::Unimplemented);
        set.insert("_Atomic", TokenKind::Unimplemented);
        set.insert("_Bool", TokenKind::Unimplemented);
        set.insert("_Complex", TokenKind::Unimplemented);
        set.insert("_Generic", TokenKind::Unimplemented);
        set.insert("_Imaginary", TokenKind::Unimplemented);
        set.insert("_Noreturn", TokenKind::Unimplemented);
        set.insert("_Static_assert", TokenKind::Unimplemented);
        set.insert("_Thread_local", TokenKind::Unimplemented);
        set.insert("_Float16", TokenKind::Unimplemented);
        set.insert("_Float16x", TokenKind::Unimplemented);
        set.insert("_Float32", TokenKind::Unimplemented);
        set.insert("_Float32x", TokenKind::Unimplemented);
        set.insert("_Float64", TokenKind::Unimplemented);
        set.insert("_Float64x", TokenKind::Unimplemented);
        set.insert("_Float128", TokenKind::Unimplemented);
        set.insert("_Float128x", TokenKind::Unimplemented);
        set.insert("_Decimal32", TokenKind::Unimplemented);
        set.insert("_Decimal32x", TokenKind::Unimplemented);
        set.insert("_Decimal64", TokenKind::Unimplemented);
        set.insert("_Decimal64x", TokenKind::Unimplemented);
        set.insert("_Decimal128", TokenKind::Unimplemented);
        set.insert("_Decimal128x", TokenKind::Unimplemented);

        set
    };
}

#[inline]
pub fn invalid_token(begin: usize, end: usize, file: u32) -> Error {
    return error!(
        "invalid token",
        l(begin as u32, end as u32, file),
        "token found here"
    );
}

const WHITESPACE: [u8; 2] = [b' ', b'\t'];
const CRLF: [u8; 2] = [b'\r', b'\n'];

pub struct Lexer<'a> {
    pub buckets: BucketListFactory,
    pub symbols: Symbols,
    pub files: &'a FileDb,

    pub macros: HashMap<u32, (Macro, CodeLoc)>,
    pub toks: Vec<TokenKind>,
    pub locs: Vec<CodeLoc>,
}

impl<'a> Drop for Lexer<'a> {
    fn drop(&mut self) {
        unsafe { self.buckets.dealloc() };
    }
}

impl<'a> Lexer<'a> {
    pub fn new(files: &'a FileDb) -> Self {
        Self {
            buckets: BucketListFactory::new(),
            symbols: Symbols::new(),
            files,

            macros: HashMap::new(),
            toks: Vec::new(),
            locs: Vec::new(),
        }
    }

    pub fn symbols(mut self) -> Symbols {
        return mem::replace(&mut self.symbols, Symbols::new());
    }

    pub fn lex(&mut self, file: u32) -> Result<(u32, Vec<TokenKind>, Vec<CodeLoc>), Error> {
        self.macros.clear();
        self.toks.clear();
        self.locs.clear();

        let data = self.files.source(file).unwrap().as_bytes();
        let mut lexers = TaggedMultiArray::new();
        lexers.push_from(SimpleLexer::new(file), data);

        loop {
            let TE(lexer, data) = match lexers.last_mut() {
                Some(a) => a,
                None => break,
            };

            match self.lex_file_until_include(lexer, data)? {
                Some(include) => {
                    let loc = lexer.loc();
                    let mut iter = (&lexers).into_iter();
                    if iter.find(|TE(lex, _)| lex.file == include).is_some() {
                        return Err(error!("include cycle detected", loc, "cycle found here"));
                    }

                    let data = self.files.source(include).unwrap().as_bytes();
                    lexers.push_from(SimpleLexer::new(include), data);
                }
                None => {
                    lexers.pop();
                }
            }
        }

        let toks = mem::replace(&mut self.toks, Vec::new());
        let locs = mem::replace(&mut self.locs, Vec::new());
        return Ok((file, toks, locs));
    }

    pub fn lex_file_until_include(
        &mut self,
        lexer: &mut SimpleLexer,
        data: &[u8],
    ) -> Result<Option<u32>, Error> {
        loop {
            let tok = match lexer.lex(&*self.buckets, &mut self.symbols, self.files, data)? {
                Some(tok) => tok,
                None => return Ok(None),
            };

            match tok {
                RawTok::Noop => continue,
                RawTok::Include(id) => return Ok(Some(id)),
                RawTok::Tok(TokenKind::Ident(id)) => {
                    let (mac, loc) = if let Some((mac, loc)) = self.macros.get(&id) {
                        ((*mac).clone(), *loc)
                    } else {
                        self.toks.push(TokenKind::Ident(id));
                        self.locs.push(lexer.loc());
                        continue;
                    };

                    self.expand_macro(lexer, data, id, &mac, loc)?;
                }
                RawTok::Tok(tok) => {
                    self.toks.push(tok);
                    self.locs.push(lexer.loc());
                }

                RawTok::If => {
                    let should_write = self.eval_macro_if(lexer, data)?;
                    let prev_should_write = lexer.should_write.last().map(|a| *a).unwrap_or(true);

                    lexer.should_write.push(should_write && prev_should_write);
                }
                RawTok::Ifdef(def) => {
                    let should_write = self.macros.contains_key(&def);
                    let prev_should_write = lexer.should_write.last().map(|a| *a).unwrap_or(true);

                    lexer.should_write.push(should_write && prev_should_write);
                }
                RawTok::Ifndef(def) => {
                    let should_write = !self.macros.contains_key(&def);
                    let prev_should_write = lexer.should_write.last().map(|a| *a).unwrap_or(true);

                    lexer.should_write.push(should_write && prev_should_write);
                }
                RawTok::Endif => {
                    let loc = lexer.loc();
                    let or_else = move || error!("#endif without matching #if", loc, "found here");
                    lexer.should_write.pop().ok_or_else(or_else)?;
                }
                RawTok::Else => {
                    let loc = lexer.loc();
                    let or_else = move || error!("#else without matching #if", loc, "found here");
                    let last = lexer.should_write.last_mut().ok_or_else(or_else)?;
                    *last = !*last;
                }

                RawTok::Define(id) => {
                    let (_macro, loc) = self.parse_macro_defn(lexer, data, lexer.loc())?;
                    self.macros.insert(id, (_macro, loc));
                }
                RawTok::FuncDefine(id) => {
                    let (_macro, loc) = self.parse_func_macro_defn(lexer, data, lexer.loc())?;
                    self.macros.insert(id, (_macro, loc));
                }
                RawTok::EndPPLine => panic!("this should never happen"),
            }
        }
    }

    pub fn eval_macro_if(&mut self, lexer: &mut SimpleLexer, data: &[u8]) -> Result<bool, Error> {
        let val = self.eval_macro_or(lexer, data)?;
        let end_line = self.expect_raw_tok(lexer, data)?;
        if end_line != RawTok::EndPPLine {
            return Err(error!("expected newline", lexer.loc(), "here"));
        }

        return Ok(val);
    }

    pub fn eval_macro_or(&mut self, lexer: &mut SimpleLexer, data: &[u8]) -> Result<bool, Error> {
        return self.eval_macro_and(lexer, data);
    }

    pub fn eval_macro_and(&mut self, lexer: &mut SimpleLexer, data: &[u8]) -> Result<bool, Error> {
        return self.eval_macro_not(lexer, data);
    }

    pub fn eval_macro_not(&mut self, lexer: &mut SimpleLexer, data: &[u8]) -> Result<bool, Error> {
        // remember the '!=' operator is like a boolean XOR
        // let target = false;
        return self.eval_macro_defined(lexer, data);
    }

    pub fn eval_macro_defined(
        &mut self,
        lexer: &mut SimpleLexer,
        data: &[u8],
    ) -> Result<bool, Error> {
        let tok = self.expect_tok(lexer, data)?;
        if let TokenKind::Ident(id) = tok {
            if id != BuiltinSymbol::MacroDefined as u32 {
                return Err(error!(
                    "expected call to 'defined'",
                    lexer.loc(),
                    "token found here should be the word 'defined'"
                ));
            }
        } else {
            return Err(error!(
                "expected call to 'defined'",
                lexer.loc(),
                "token found here should be the word 'defined'"
            ));
        }

        let tok = self.expect_tok(lexer, data)?;
        if tok != TokenKind::LParen {
            return Err(error!(
                "expected call to 'defined'",
                lexer.loc(),
                "token found here should be a '('"
            ));
        }

        let tok = self.expect_tok(lexer, data)?;
        let id = if let TokenKind::Ident(id) = tok {
            id
        } else {
            return Err(error!(
                "expected call to 'defined'",
                lexer.loc(),
                "token found here should be the word 'defined'"
            ));
        };

        let tok = self.expect_tok(lexer, data)?;
        if tok != TokenKind::RParen {
            return Err(error!(
                "expected call to 'defined'",
                lexer.loc(),
                "token found here should be a ')'"
            ));
        }

        return Ok(self.macros.contains_key(&id));
    }

    pub fn expand_macro(
        &mut self,
        lexer: &mut SimpleLexer,
        data: &[u8],
        id: u32,
        mac: &Macro,
        loc: CodeLoc,
    ) -> Result<(), Error> {
        let mut expanded = Vec::new();
        expanded.push(id);

        let begin = lexer.loc();

        let expansion = match mac {
            Macro::Func { params, toks } => {
                if self.expect_tok(lexer, data)? != TokenKind::LParen {
                    return Err(error!(
                        "TCI requires function macro to be called",
                        lexer.loc(),
                        "this should be a '(' character"
                    ));
                }

                let mut actual_params = Vec::new();
                let mut paren_count = 0;
                let mut current_tok = self.expect_tok(lexer, data)?;

                if current_tok != TokenKind::RParen {
                    loop {
                        let mut current_param = Vec::new();
                        while paren_count != 0
                            || (current_tok != TokenKind::RParen && current_tok != TokenKind::Comma)
                        {
                            current_param.push(current_tok);
                            match current_tok {
                                TokenKind::LParen => paren_count += 1,
                                TokenKind::RParen => paren_count -= 1,
                                _ => {}
                            }

                            current_tok = self.expect_tok(lexer, data)?;
                        }

                        actual_params.push(current_param);
                        if current_tok == TokenKind::RParen {
                            break;
                        }

                        current_tok = self.expect_tok(lexer, data)?;
                    }
                }

                if params.len() != actual_params.len() {
                    return Err(error!(
                        "provided wrong number of arguments to macro",
                        loc,
                        format!("macro defined here (takes in {} arguments)", params.len()),
                        l_from(begin, lexer.loc()),
                        format!(
                            "macro used here (passed in {} arguments)",
                            actual_params.len()
                        )
                    ));
                }

                let mut params_hash = HashMap::new();
                for (idx, param) in actual_params.into_iter().enumerate() {
                    params_hash.insert(params[idx], param);
                }

                self.expand_macro_simple(params_hash, &toks)
            }
            Macro::Value(toks) => self.expand_macro_simple(HashMap::new(), &toks),
            Macro::Marker => {
                return Err(error!(
                    "used marker macro in code",
                    loc, "macro defined here", begin, "used here"
                ))
            }
        };

        let loc = l_from(begin, lexer.loc());
        let output = self.expand_macro_rec(&mut expanded, &expansion, loc)?;

        self.toks.extend_from_slice(&output);
        self.locs.resize(self.toks.len(), loc);

        return Ok(());
    }

    pub fn expand_macro_simple(
        &self,
        params: HashMap<u32, Vec<TokenKind>>,
        toks: &[TokenKind],
    ) -> Vec<TokenKind> {
        let mut output = Vec::new();

        for tok in toks {
            match tok {
                TokenKind::Ident(id) => {
                    if let Some(expand) = params.get(id) {
                        for tok in expand {
                            output.push(*tok);
                        }
                    } else {
                        output.push(*tok);
                    }
                }
                x => {
                    output.push(*x);
                }
            }
        }

        return output;
    }

    pub fn expand_macro_rec(
        &self,
        expanded: &mut Vec<u32>,
        tokens: &[TokenKind],
        loc: CodeLoc,
    ) -> Result<Vec<TokenKind>, Error> {
        let mut toks = tokens.iter();
        let mut output = Vec::new();

        let expect = || error!("expected token");

        while let Some(tok) = toks.next() {
            let id = match tok {
                TokenKind::Ident(id) => *id,
                _ => {
                    output.push(*tok);
                    continue;
                }
            };

            let (macro_def, def_loc) = match self.macros.get(&id) {
                Some(def) => {
                    if expanded.contains(&id) {
                        output.push(*tok); // TODO output warning here
                        continue;
                    }

                    def
                }
                None => {
                    output.push(*tok);
                    continue;
                }
            };

            let (macro_params, macro_toks) = match &macro_def {
                Macro::Marker => {
                    return Err(error!(
                        "used marker macro in code",
                        *def_loc, "macro defined here", loc, "used here"
                    ))
                }
                Macro::Value(toks) => {
                    expanded.push(id);
                    let mut expanded_toks = self.expand_macro_rec(expanded, toks, loc)?;
                    expanded.pop();
                    output.append(&mut expanded_toks);
                    continue;
                }
                Macro::Func { params, toks } => (params, toks),
            };

            let lparen_tok = toks.next().ok_or_else(expect)?;
            match lparen_tok {
                TokenKind::LParen => {}
                _ => {
                    return Err(error!(
                        "expected a left paren '(' because of function macro invokation",
                        loc, "macro used here", *def_loc, "macro defined here"
                    ));
                }
            }

            let mut actual_params = Vec::new();
            let mut paren_count = 0;
            let mut current_tok = *toks.next().ok_or_else(expect)?;

            if current_tok != TokenKind::RParen {
                loop {
                    let mut current_param = Vec::new();
                    while paren_count != 0
                        || (current_tok != TokenKind::RParen && current_tok != TokenKind::Comma)
                    {
                        current_param.push(current_tok);
                        match current_tok {
                            TokenKind::LParen => paren_count += 1,
                            TokenKind::RParen => paren_count -= 1,
                            _ => {}
                        }

                        current_tok = *toks.next().ok_or_else(expect)?;
                    }

                    actual_params.push(current_param);
                    if current_tok == TokenKind::RParen {
                        break;
                    }

                    current_tok = *toks.next().ok_or_else(expect)?;
                }
            }

            if macro_params.len() != actual_params.len() {
                return Err(error!(
                    "provided wrong number of arguments to macro",
                    *def_loc,
                    format!(
                        "macro defined here (takes in {} arguments)",
                        macro_params.len()
                    ),
                    loc,
                    format!(
                        "macro used here (passed in {} arguments)",
                        actual_params.len()
                    )
                ));
            }

            let mut params_hash = HashMap::new();
            for (idx, param) in actual_params.into_iter().enumerate() {
                params_hash.insert(macro_params[idx], param);
            }

            expanded.push(id);
            let expanded_toks = self.expand_macro_simple(params_hash, macro_toks);
            let mut expanded_toks = self.expand_macro_rec(expanded, &expanded_toks, loc)?;
            expanded.pop();
            output.append(&mut expanded_toks);
        }

        return Ok(output);
    }

    pub fn parse_macro_defn(
        &mut self,
        lexer: &mut SimpleLexer,
        data: &[u8],
        define_loc: CodeLoc,
    ) -> Result<(Macro, CodeLoc), Error> {
        let mut out = Vec::new();
        let mut loc = define_loc;

        loop {
            let next = match self.expect_raw_tok(lexer, data)? {
                RawTok::EndPPLine => {
                    let loc = l_from(define_loc, loc);
                    if out.len() == 0 {
                        return Ok((Macro::Marker, loc));
                    } else {
                        return Ok((Macro::Value(out), loc));
                    }
                }
                RawTok::Tok(t) => t,
                x => {
                    return Err(error!(
                        "expected token or new line, found something else",
                        lexer.loc(),
                        format!("this was lexed as a {:?}", x)
                    ))
                }
            };

            out.push(next);
            loc = lexer.loc();
        }
    }

    pub fn parse_func_macro_defn(
        &mut self,
        lexer: &mut SimpleLexer,
        data: &[u8],
        define_loc: CodeLoc,
    ) -> Result<(Macro, CodeLoc), Error> {
        let mut params = Vec::new();
        let mut tok = self.expect_tok(lexer, data)?;
        while let TokenKind::Ident(id) = tok {
            params.push(id);
            tok = self.expect_tok(lexer, data)?;
            if let TokenKind::Comma = tok {
                tok = self.expect_tok(lexer, data)?;
            } else {
                break;
            }
        }

        if tok != TokenKind::RParen {
            return Err(error!(
                "expected right paren here",
                lexer.loc(),
                format!("this token was lexed as {:?}", tok)
            ));
        }

        let mut toks = Vec::new();
        let mut loc = lexer.loc();
        loop {
            let next = match self.expect_raw_tok(lexer, data)? {
                RawTok::EndPPLine => {
                    let loc = l_from(define_loc, loc);
                    return Ok((Macro::Func { params, toks }, loc));
                }
                RawTok::Tok(t) => t,
                x => {
                    return Err(error!(
                        "expected token or new line, found something else",
                        lexer.loc(),
                        format!("this was lexed as a {:?}", x)
                    ))
                }
            };

            toks.push(next);
            loc = lexer.loc();
        }
    }

    pub fn expect_tok_opt(
        &mut self,
        lexer: &mut SimpleLexer,
        data: &[u8],
    ) -> Result<Option<TokenKind>, Error> {
        let tok = match self.simple_lex(lexer, data)? {
            Some(t) => t,
            None => return Ok(None),
        };

        match tok {
            RawTok::Tok(t) => return Ok(Some(t)),
            x => {
                return Err(error!(
                    "expected token, found something else",
                    lexer.loc(),
                    format!("this was lexed as a {:?}", x)
                ));
            }
        }
    }

    pub fn expect_tok(&mut self, lexer: &mut SimpleLexer, data: &[u8]) -> Result<TokenKind, Error> {
        match self.expect_raw_tok(lexer, data)? {
            RawTok::Tok(t) => return Ok(t),
            x => {
                return Err(error!(
                    "expected token, found something else",
                    lexer.loc(),
                    format!("this was lexed as a {:?}", x)
                ));
            }
        }
    }

    pub fn expect_raw_tok(
        &mut self,
        lexer: &mut SimpleLexer,
        data: &[u8],
    ) -> Result<RawTok, Error> {
        let tok = self.simple_lex(lexer, data)?;
        let or_else = || {
            error!(
                "expected token",
                lexer.loc(),
                "expecting another token after this one"
            )
        };

        return Ok(tok.ok_or_else(or_else)?);
    }

    #[inline]
    pub fn simple_lex(
        &mut self,
        lexer: &mut SimpleLexer,
        data: &[u8],
    ) -> Result<Option<RawTok>, Error> {
        return lexer.lex(&*self.buckets, &mut self.symbols, self.files, data);
    }
}

#[derive(Debug)]
pub struct SimpleLexer {
    pub at_line_begin: bool,
    pub in_macro: bool,
    pub in_number: bool,

    pub begin: usize,
    pub current: usize,
    pub file: u32,
    pub should_write: Vec<bool>, // yeah yeah yeah whatever
}

impl SimpleLexer {
    pub fn new(file: u32) -> Self {
        Self {
            at_line_begin: true,
            in_macro: false,
            in_number: false,
            begin: 0,
            current: 0,
            file,
            should_write: Vec::new(),
        }
    }

    pub fn lex(
        &mut self,
        buckets: &impl Allocator<'static>,
        symbols: &mut Symbols,
        files: &FileDb,
        data: &[u8],
    ) -> Result<Option<RawTok>, Error> {
        let tok = self._lex(buckets, symbols, files, data)?;
        if let Some(tok) = tok {
            match tok {
                RawTok::Ifdef(_) | RawTok::Ifndef(_) | RawTok::Endif => return Ok(Some(tok)),
                _ => {
                    if self.should_write.last().map(|a| *a).unwrap_or(true) {
                        return Ok(Some(tok));
                    }

                    return Ok(Some(RawTok::Noop));
                }
            }
        }

        return Ok(None);
    }

    pub fn _lex(
        &mut self,
        buckets: &impl Allocator<'static>,
        symbols: &mut Symbols,
        files: &FileDb,
        data: &[u8],
    ) -> Result<Option<RawTok>, Error> {
        macro_rules! ret {
            ($arg1:expr) => {{
                self.at_line_begin = false;
                self.in_number = false;
                return Ok(Some(RawTok::Tok($arg1)));
            }};
        }

        macro_rules! num_ret {
            ($arg1:expr) => {{
                self.at_line_begin = false;
                self.in_number = true;
                return Ok(Some(RawTok::Tok($arg1)));
            }};
        }

        macro_rules! incr_ret {
            ($arg1:expr) => {{
                self.current += 1;
                self.at_line_begin = false;
                return Ok(Some(RawTok::Tok($arg1)));
            }};
        }

        if self.kill_whitespace(data, self.in_macro)? {
            self.in_number = false;

            if !self.in_macro {
                return Ok(Some(RawTok::Tok(TokenKind::Whitespace)));
            }
        }

        self.begin = self.current;

        if self.current == data.len() {
            if self.in_macro {
                self.in_macro = false;
                return Ok(Some(RawTok::EndPPLine));
            }

            return Ok(None);
        }

        if self.in_macro {
            if self.peek_eq(data, b'\n') {
                self.current += 1;
                self.at_line_begin = true;
                self.in_macro = false;
                self.in_number = false;
                return Ok(Some(RawTok::EndPPLine));
            } else if self.peek_eq_series(data, &CRLF) {
                self.current += 2;
                self.at_line_begin = true;
                self.in_macro = false;
                self.in_number = false;
                return Ok(Some(RawTok::EndPPLine));
            }
        }

        self.current += 1;

        match data[self.begin] {
            x if (x >= b'A' && x <= b'Z') || (x >= b'a' && x <= b'z') || x == b'_' => {
                if self.in_number {
                    match x {
                        b'a' | b'A' => num_ret!(TokenKind::IntChar(NumChar::_A)),
                        b'b' | b'B' => num_ret!(TokenKind::IntChar(NumChar::_B)),
                        b'c' | b'C' => num_ret!(TokenKind::IntChar(NumChar::_C)),
                        b'd' | b'D' => num_ret!(TokenKind::IntChar(NumChar::_D)),
                        b'e' | b'E' => num_ret!(TokenKind::IntChar(NumChar::_E)),
                        b'f' | b'F' => num_ret!(TokenKind::IntChar(NumChar::_F)),
                        b'l' | b'L' => num_ret!(TokenKind::IntChar(NumChar::_L)),
                        b'x' | b'X' => num_ret!(TokenKind::IntChar(NumChar::_X)),
                        b'u' | b'U' => num_ret!(TokenKind::IntChar(NumChar::_U)),
                        x => num_ret!(TokenKind::IntChar(NumChar::_INVALID)),
                    }
                }

                while self.peek_check(data, is_ident_char) {
                    self.current += 1;
                }

                let word =
                    unsafe { std::str::from_utf8_unchecked(&data[self.begin..self.current]) };
                if let Some(kind) = RESERVED_KEYWORDS.get(word) {
                    ret!(*kind);
                }

                let id = symbols.add_str(word);
                ret!(TokenKind::Ident(id));
            }

            b'0' => num_ret!(TokenKind::IntChar(NumChar::_0)),
            b'1' => num_ret!(TokenKind::IntChar(NumChar::_1)),
            b'2' => num_ret!(TokenKind::IntChar(NumChar::_2)),
            b'3' => num_ret!(TokenKind::IntChar(NumChar::_3)),
            b'4' => num_ret!(TokenKind::IntChar(NumChar::_4)),
            b'5' => num_ret!(TokenKind::IntChar(NumChar::_5)),
            b'6' => num_ret!(TokenKind::IntChar(NumChar::_6)),
            b'7' => num_ret!(TokenKind::IntChar(NumChar::_7)),
            b'8' => num_ret!(TokenKind::IntChar(NumChar::_8)),
            b'9' => num_ret!(TokenKind::IntChar(NumChar::_9)),

            b'\"' => {
                let mut cur = self.lex_character(b'\"', data)?;
                let mut chars = Vec::new();
                while cur != CLOSING_CHAR {
                    chars.push(cur);
                    cur = self.lex_character(b'\"', data)?;
                }

                let string = unsafe { std::str::from_utf8_unchecked(&chars) };
                let string = buckets.add_str(string);
                ret!(TokenKind::StringLit(string));
            }

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

            x => {
                return Err(invalid_token(self.begin, self.current, self.file));
            }
        }
    }

    pub fn lex_directive(
        &mut self,
        buckets: &impl Allocator<'static>,
        symbols: &mut Symbols,
        files: &FileDb,
        data: &[u8],
    ) -> Result<RawTok, Error> {
        let directive = {
            let begin = self.current;
            while self.peek_check(data, is_ident_char) {
                self.current += 1;
            }

            unsafe { str::from_utf8_unchecked(&data[begin..self.current]) }
        };

        match directive {
            "if" => {
                self.in_macro = true;
                return Ok(RawTok::If);
            }
            "else" => {
                self.kill_whitespace(data, true)?;

                if self.peek_neq(data, b'\n') && self.peek_neq_series(data, &CRLF) {
                    self.begin = self.current;
                    self.current += 1;
                    return Err(error!(
                        "#else must be on its own line",
                        self.loc(),
                        "#else found here"
                    ));
                }

                return Ok(RawTok::Else);
            }
            "ifndef" => {
                while self.peek_eqs(data, &WHITESPACE) {
                    self.current += 1;
                }

                let ident_begin = self.current;
                while self.peek_check(data, is_ident_char) {
                    self.current += 1;
                }

                let ident = unsafe { str::from_utf8_unchecked(&data[ident_begin..self.current]) };

                // Don't add the empty string
                if ident == "" {
                    return Err(error!(
                        "expected an identifer for ifndef",
                        l(ident_begin as u32, ident_begin as u32 + 1, self.file),
                        "This should be an identifier"
                    ));
                }

                let ident = symbols.add_str(ident);

                return Ok(RawTok::Ifndef(ident));
            }
            "ifdef" => {
                while self.peek_eqs(data, &WHITESPACE) {
                    self.current += 1;
                }

                let ident_begin = self.current;
                while self.peek_check(data, is_ident_char) {
                    self.current += 1;
                }

                let ident = unsafe { str::from_utf8_unchecked(&data[ident_begin..self.current]) };

                // Don't add the empty string
                if ident == "" {
                    return Err(error!(
                        "expected an identifer for ifdef",
                        l(ident_begin as u32, ident_begin as u32 + 1, self.file),
                        "This should be an identifier"
                    ));
                }

                let ident = symbols.add_str(ident);

                return Ok(RawTok::Ifdef(ident));
            }
            "endif" => {
                self.kill_whitespace(data, true)?;

                if self.peek_neq(data, b'\n') && self.peek_neq_series(data, &CRLF) {
                    self.begin = self.current;
                    self.current += 1;
                    return Err(error!(
                        "#endif must be on its own line",
                        self.loc(),
                        "#endif found here"
                    ));
                }

                return Ok(RawTok::Endif);
            }

            "pragma" => {
                self.current += 1;
                let begin = self.current;
                while self.peek_neq(data, b'\n') && self.peek_neq_series(data, &CRLF) {
                    self.current += 1;
                }

                let pragma = unsafe { str::from_utf8_unchecked(&data[begin..self.current]) };
                let pragma = buckets.add_str(pragma);

                return Ok(RawTok::Tok(TokenKind::Pragma(pragma)));
            }
            "define" => {
                while self.peek_eqs(data, &WHITESPACE) {
                    self.current += 1;
                }

                let ident_begin = self.current;
                while self.peek_check(data, is_ident_char) {
                    self.current += 1;
                }

                let ident = unsafe { str::from_utf8_unchecked(&data[ident_begin..self.current]) };

                // Don't add the empty string
                if ident == "" {
                    return Err(error!(
                        "expected an identifer for macro definition",
                        l(ident_begin as u32, ident_begin as u32 + 1, self.file),
                        "This should be an identifier"
                    ));
                }

                let ident = symbols.add_str(ident);
                self.in_macro = true;

                if self.peek_eq(data, b'(') {
                    self.current += 1;
                    return Ok(RawTok::FuncDefine(ident));
                } else {
                    return Ok(RawTok::Define(ident));
                }
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

                    let name_end = self.current;
                    self.current += 1;
                    if self.peek_neq(data, b'\n') && self.peek_neq_series(data, &CRLF) {
                        return Err(expected_newline(
                            "include",
                            self.begin,
                            self.current,
                            self.file,
                        ));
                    }

                    if !self.should_write.last().map(|a| *a).unwrap_or(true) {
                        return Ok(RawTok::Noop);
                    }

                    let map_err = |err| {
                        error!(
                            "error finding file",
                            self.loc(),
                            format!("got error '{}'", err)
                        )
                    };

                    let include_name =
                        unsafe { str::from_utf8_unchecked(&data[name_begin..name_end]) };
                    let include_id = files
                        .resolve_include(include_name, self.file)
                        .map_err(map_err)?;

                    return Ok(RawTok::Include(include_id));
                } else if self.peek_eq(data, b'<') {
                    self.current += 1;
                    let name_begin = self.current;

                    while self.peek_neqs(data, &[b'>', b'\n']) && self.peek_neq_series(data, &CRLF)
                    {
                        self.current += 1;
                    }

                    let name_end = self.current;

                    if b'>' != self.expect(data)? {
                        return Err(error!(
                            "expected a '>'",
                            l((self.current - 1) as u32, self.current as u32, self.file),
                            "this should be a '>'"
                        ));
                    }

                    if self.peek_neq(data, b'\n') && self.peek_neq_series(data, &CRLF) {
                        return Err(expected_newline(
                            "include",
                            self.begin,
                            self.current,
                            self.file,
                        ));
                    }

                    let sys_file = unsafe { str::from_utf8_unchecked(&data[name_begin..name_end]) };
                    if !self.should_write.last().map(|a| *a).unwrap_or(true) {
                        return Ok(RawTok::Noop);
                    }

                    let map_err = |err| {
                        error!(
                            "error finding file",
                            self.loc(),
                            format!("got error '{}'", err)
                        )
                    };

                    let id = files.resolve_system_include(sys_file).map_err(map_err)?;
                    return Ok(RawTok::Include(id));
                } else {
                    return Err(error!(
                        "expected a '<' or '\"' here",
                        self.loc(),
                        "directive found here"
                    ));
                }
            }
            x => {
                return Err(error!(
                    "invalid compiler directive",
                    self.loc(),
                    format!("found here to be `{}`", x)
                ));
            }
        }
    }

    pub fn kill_whitespace(&mut self, data: &[u8], avoid_newlines: bool) -> Result<bool, Error> {
        self.begin = self.current;

        loop {
            while self.peek_eqs(data, &WHITESPACE) {
                self.current += 1;
                self.at_line_begin = false;
            }

            if self.peek_eq_series(data, &[b'/', b'/']) {
                self.current += 2;
                loop {
                    if self.current == data.len() {
                        return Ok(true);
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
                    } else {
                        self.current += 1;
                    }
                }
            } else if self.peek_eq_series(data, &[b'/', b'*']) {
                self.current += 2;
                loop {
                    if self.current == data.len() {
                        return Err(error!(
                            "block comment still open when file ends",
                            self.loc(),
                            "comment is here"
                        ));
                    }

                    if self.peek_eq_series(data, &[b'*', b'/']) {
                        break;
                    }

                    self.current += 1;
                }

                self.current += 2;
                self.at_line_begin = false;
                continue;
            }

            if self.peek_eq(data, b'\n') {
                if avoid_newlines {
                    break;
                }

                self.current += 1;
                self.at_line_begin = true;
            } else if self.peek_eq_series(data, &CRLF) {
                if avoid_newlines {
                    break;
                }

                self.current += 2;
                self.at_line_begin = true;
            } else if self.peek_eq_series(data, &[b'\\', b'\n']) {
                self.current += 2;
            } else if self.peek_eq_series(data, &[b'\\', b'\r', b'\n']) {
                self.current += 3;
            } else {
                break;
            }
        }

        return Ok(self.begin != self.current);
    }

    #[inline]
    pub fn loc(&self) -> CodeLoc {
        return l(self.begin as u32, self.current as u32, self.file);
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
                b't' => return Ok(b'\t'),
                b'\'' => return Ok(b'\''),
                b'"' => return Ok(b'"'),

                // \nnn where each 'n' is an octal digit
                x @ b'0'..=b'7' => {
                    let mut c = x - b'0';
                    if !self.peek_check(data, |c| c >= b'0' && c <= b'7') {
                        return Ok(c);
                    }

                    c *= 8;
                    c += data[self.current] - b'0';
                    self.current += 1;

                    if !self.peek_check(data, |c| c >= b'0' && c <= b'7') {
                        return Ok(c);
                    }

                    c *= 8;
                    c += data[self.current] - b'0';
                    self.current += 1;

                    return Ok(c);
                }

                b'\n' => continue,
                b'\r' => {
                    if self.peek_eq(data, b'\n') {
                        self.current += 1;
                        continue;
                    } else {
                        return Err(error!(
                            "encoding of the file is probably messed up",
                            l(self.current as u32 - 1, self.current as u32, self.file),
                            "invalid character here"
                        ));
                    }
                }

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
