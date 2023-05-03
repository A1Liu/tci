/*!
The job of this module is to convert tokens from the raw lexed tokens
into tokens usable by the parser. This means:

1. Expanding macros, and removing macro directives
2. Removing comment, newline, and etc. tokens
   (maybe in the future they'll be stored somewhere separately as well)
3. Expanding and parsing `TokenKind::PreprocessingNum`, and converting it
   into number tokens with attached data.

This job is done by `expand_macros`. It reads the tokens that it's given
and returns a new set of tokens with everything fixed up properly.
*/

use crate::api::*;

pub fn expand_macros(tokens: TokenSlice) -> TokenVec {
    let mut output = TokenVec::new();

    for tok in &tokens {
        match *tok.kind {
            TokenKind::Newline => continue,
            TokenKind::Comment => continue,

            _ => {}
        }

        output.push(Token {
            kind: *tok.kind,
            start: *tok.start,
            symbol: *tok.symbol,
        })
    }

    return output;
}
