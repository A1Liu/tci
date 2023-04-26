use crate::api::*;

#[test]
#[timeout(300)]
fn test_lexer() {
    const SOURCE_TEXT: &'static str = r#"
int main(int argc, char* argv[]) {
    return *"printf"[1 + 1];
}
"#;

    const TOKENS: &'static [TokenKind] = &[
        TokenKind::Newline,
        TokenKind::Int,
        TokenKind::Ident,
        TokenKind::LParen,
        TokenKind::Int,
        TokenKind::Ident,
        TokenKind::Comma,
        TokenKind::Char,
        TokenKind::Star,
        TokenKind::Ident,
        TokenKind::LBracket,
        TokenKind::RBracket,
        TokenKind::RParen,
        TokenKind::LBrace,
        TokenKind::Newline,
        TokenKind::Return,
        TokenKind::Star,
        TokenKind::StringLit,
        TokenKind::LBracket,
        TokenKind::PreprocessingNum,
        TokenKind::Plus,
        TokenKind::PreprocessingNum,
        TokenKind::RBracket,
        TokenKind::Semicolon,
        TokenKind::Newline,
        TokenKind::RBrace,
        TokenKind::Newline,
    ];

    let res = lex("main.c", SOURCE_TEXT).expect("Expected lex to succeed");
    let mut index = 0;
    for tok in res.tokens.iter() {
        if *tok.kind != TOKENS[index] {
            panic!(
                "At index {}, expected {:?} but got {:?}",
                index, TOKENS[index], tok.kind
            );
        }

        index += 1;
    }

    if index != TOKENS.len() {
        panic!("didn't consume all tokens (only consumed {})", index);
    }
}
