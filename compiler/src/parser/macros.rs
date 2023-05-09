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

pub fn expand_macros(
    tokens: TokenSlice,
    file_db: &FileDb,
    translation_unit: &TranslationUnitDebugInfo,
) -> Result<TokenVec, Error> {
    let mut output = TokenVec::new();

    let mut buf_tmp = String::new();

    for tok in &tokens {
        buf_tmp.clear();

        match *tok.kind {
            TokenKind::Newline | TokenKind::Comment => continue,

            TokenKind::PreprocessingNum => {
                let start = *tok.start;
                let range = translation_unit.token_range(start);
                let file_start = range.start;
                let file = range.file;

                let str = file_db.files[file as usize].source.as_bytes();
                [file_start..];

                let (mut file_index, radix) = match (str[file_start], str.get(file_start + 1)) {
                    // Decimal
                    (b'1'..=b'9', _) => (file_start, 10),

                    // Octal
                    (b'0', Some(b'0'..=b'9')) => (file_start + 1, 8u32),

                    // Hex
                    (b'0', Some(b'x')) => (file_start + 2, 16),

                    // Binary
                    (b'0', Some(b'b')) => (file_start + 2, 2),

                    // Decimal zero
                    (b'0', _) => (file_start, 10),

                    x => panic!("preprocessing num had weird character: {:?}", x),
                };

                // https://en.cppreference.com/w/c/language/integer_constant
                let mut floating = false;
                let mut suffix = false;
                while let Some(&b) = str.get(file_index) {
                    let c = b.to_ascii_lowercase() as char;
                    // TODO: Skip quotes in integer literals
                    match (c, str.get(file_index + 1)) {
                        ('0'..='9', _) => buf_tmp.push(c),

                        ('e' | 'p', Some(&sign @ (b'-' | b'+'))) => {
                            floating = true;
                            buf_tmp.push(c);
                            buf_tmp.push(sign as char);
                            file_index += 1;
                        }

                        ('a'..='f', _) if radix > 10 => buf_tmp.push(c),

                        ('a'..='z', _) => {
                            suffix = true;
                            break;
                        }

                        ('.', _) => {
                            floating = true;
                            buf_tmp.push(c);
                        }

                        (' ' | '\t' | '\r' | '\n', _) => break,

                        c => panic!("got weird character when parsing int: {:?}", c),
                    }

                    file_index += 1;
                }

                if floating {
                    // https://en.cppreference.com/w/c/language/floating_constant
                    unimplemented!("floating point numbers aren't implemented yet");
                }
                if suffix {
                    unimplemented!("suffixes aren't implemented yet");
                }

                let val = match u64::from_str_radix(&buf_tmp, radix) {
                    Ok(val) => val,
                    Err(e) => throw!(Todo, "failed to parse int", start),
                };

                let mut tok = tok.to_owned();
                tok.data = val;

                output.push(tok);
                continue;
            }

            _ => {}
        }

        output.push(tok.to_owned());
    }

    return Ok(output);
}
