use crate::env::runtime::tokens::{Location, Token, TK_IDENTIFIER, TK_STRING, TK_RAW_STRING, TK_NUMBER, TK_BOOLEAN, TK_OPERATOR, TK_SEPARATOR, TK_INVALID, TK_EOF};
use std::borrow::Cow;

const SYMBOL_OPERATORS: [&str; 45] = [
    "->", ">=", "<=", "==", "!=", "+=", "-=", "*=", "/=", "=>", "=!", "=+", "=-", "=*",
    "=/", "=^", "=", "<<", ">>", ":=", "^^^", "^^", "++", "--", "+", "-", "^", "*", "/",
    ">", "<", "!", "%", "||", "&&", "??", "|>",  "|", "#", "~", "$", "?", "&", "^=", "%=",
];
const WORD_OPERATORS: [&str; 15] = ["lshift", "rshift", "isn't", "isnt", "xnor", "nein", "band", "bnot", "and", "not", "xor", "bor", "in", "or", "is"];
const SEPARATORS: [&str; 13] = ["...", "..", "(", ")", "{", "}", "[", "]", ";", ":", ".", ",", "\\"];

pub struct Lexer<'a> {
    code: &'a str,
    file_path: &'a str,
    line_offsets: Vec<usize>,
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str, file_path: &'a str) -> Self {
        let mut line_offsets = vec![0];
        line_offsets.extend(code.match_indices('\n').map(|(idx, _)| idx + 1));
        Self {
            code,
            file_path,
            line_offsets,
        }
    }

    fn make_token(&self, kind: &'static str, text: &str, start: usize, end: usize) -> Token {
        let line_number = match self.line_offsets.binary_search(&start) {
            Ok(l) => l,
            Err(l) => if l == 0 { 0 } else { l - 1 },
        };
        let line_offset = self.line_offsets[line_number];
        Token::new_static(
            kind,
            Cow::Owned(text.to_string()),
            Some(Location::new(
                self.file_path.to_string(),
                self.code[line_offset..]
                    .lines()
                    .next()
                    .unwrap_or("")
                    .to_string(),
                line_number + 1,
                (start - line_offset, end - line_offset),
                "".to_string(),
            )),
        )
    }

    fn is_identifier_start(c: char) -> bool {
        c.is_alphabetic() || c == '_' || Lexer::is_emoji(c)
    }

    fn is_identifier_continue(c: char) -> bool {
        c.is_alphanumeric() || c == '_' || Lexer::is_emoji(c)
    }

    fn is_emoji(c: char) -> bool {
        matches!(c as u32,
            0x1F600..=0x1F64F   // emoticons
            | 0x1F300..=0x1F5FF // symbols & pictographs
            | 0x1F680..=0x1F6FF // transport & map
            | 0x2600..=0x26FF   // misc symbols
            | 0x2700..=0x27BF   // dingbats
            | 0x1F900..=0x1F9FF // supplemental symbols & pictographs
            | 0x1FA70..=0x1FAFF // symbols & pictographs extended-A
            | 0x200D            // zero width joiner
            | 0xFE0F            // variation selector-16
        )
    }

    // 23ms to 147.1µs
    // fuck regex
    pub fn tokenize(&self) -> Vec<Token> {
        let mut pos = 0;
        let len = self.code.len();
        let mut tokens = Vec::with_capacity(len / 3);

        'outer: while pos < len {
            let slice = &self.code[pos..];
            let mut chars = slice.char_indices();

            if let Some((_, ch)) = chars.next() {
                if ch.is_whitespace() {
                    pos += ch.len_utf8();
                    continue;
                }
            }

            // INLINE COMMENT <# ... #>
            if slice.starts_with("<#") {
                let mut end = pos + 2;
                while end < len && !self.code[end..].starts_with("#>") {
                    end += self.code[end..].chars().next().unwrap().len_utf8();
                }
                if end < len && self.code[end..].starts_with("#>") {
                    end += 2;
                }
                pos = end;
                continue;
            }

            // SINGLE-LINE COMMENT //
            if slice.starts_with("//") {
                let mut end = pos + 2;
                while end < len && !self.code[end..].starts_with("\n") {
                    end += self.code[end..].chars().next().unwrap().len_utf8();
                }
                pos = end;
                continue;
            }

            // MULTI-LINE COMMENT /* ... */
            if slice.starts_with("/*") {
                let mut end = pos + 2;
                while end < len && !self.code[end..].starts_with("*/") {
                    end += self.code[end..].chars().next().unwrap().len_utf8();
                }
                if end < len && self.code[end..].starts_with("*/") {
                    end += 2;
                }
                pos = end;
                continue;
            }

            // STRINGS
            if let Some((_, _)) = chars.next() {
                let mut end = pos;
                let mut quote = '\0';
                let mut raw = false;

                let mut iter = slice.char_indices();
                let (_, first) = iter.next().unwrap();

                if first == 'r' || first == 'f' || first == 'b' {
                    if let Some((idx2, c2)) = iter.next() {
                        if c2 == '"' || c2 == '\'' {
                            quote = c2;
                            end += idx2 + c2.len_utf8();
                            raw = first == 'r';
                        } else if c2 == 'r' {
                            if let Some((idx3, q)) = iter.next() {
                                if q == '"' || q == '\'' {
                                    quote = q;
                                    end += idx3 + q.len_utf8();
                                    raw = true;
                                }
                            }
                        }
                    }
                } else if first == '"' || first == '\'' {
                    quote = first;
                    end += first.len_utf8();
                }

                if quote != '\0' {
                    let mut closed = false;
                    let mut local_end = end;

                    while local_end < len {
                        let c = self.code[local_end..].chars().next().unwrap();
                        let c_len = c.len_utf8();

                        local_end += c_len;

                        if c == quote {
                            closed = true;
                            break;
                        }

                        if c == '\\' && !raw && local_end < len {
                            local_end += self.code[local_end..].chars().next().unwrap().len_utf8();
                        }
                    }

                    let token_kind = if raw {
                        TK_RAW_STRING
                    } else if closed {
                        TK_STRING
                    } else {
                        TK_INVALID
                    };

                    if token_kind == TK_INVALID {
                        let mut invalid_pos = pos;
                        while invalid_pos < local_end {
                            let c_len = self.code[invalid_pos..].chars().next().unwrap().len_utf8();
                            tokens.push(self.make_token(
                                TK_INVALID,
                                &self.code[invalid_pos..invalid_pos + c_len],
                                invalid_pos,
                                invalid_pos + c_len,
                            ));
                            invalid_pos += c_len;
                        }
                    } else {
                        let mut raw_text = String::new();
                        for ch in self.code[pos..local_end].chars() {
                            if ch == '\n' {
                                raw_text.push_str("\\n");
                            } else if ch == '\r' {
                                raw_text.push_str("\\n");
                            } else {
                                raw_text.push(ch);
                            }
                        }

                        tokens.push(self.make_token(token_kind, &raw_text, pos, local_end));
                    }

                    pos = local_end;
                    continue 'outer;
                }
            }

            // BOOLEAN
            for &b in &["true", "false", "null"] {
                if slice.starts_with(b) {
                    tokens.push(self.make_token(TK_BOOLEAN, b, pos, pos + b.len()));
                    pos += b.len();
                    continue 'outer;
                }
            }

            // NUMBERS
            let start = pos;
            let s = &self.code[start..];
            let mut chars = s.char_indices().peekable();
            let mut rel_end = 0;

            let prev_is_digit = start > 0
                && self.code[..start].chars().last().is_some_and(|c| c.is_ascii_digit());

            if let Some(&(i, c)) = chars.peek() {
                if (c == '-' || c == '+') && !prev_is_digit {
                    rel_end = i + c.len_utf8();
                    chars.next();
                }
            }

            fn consume_while<F>(chars: &mut std::iter::Peekable<std::str::CharIndices>, rel_end: &mut usize, mut pred: F)
            where F: FnMut(char) -> bool {
                while let Some(&(i, c)) = chars.peek() {
                    if pred(c) {
                        *rel_end = i + c.len_utf8();
                        chars.next();
                    } else {
                        break;
                    }
                }
            }

            {
                let mut tmp = chars.clone();
                let mut tmp_rel_end = rel_end;
                let mut matched = false;

                if let Some(&(_, c)) = tmp.peek() {
                    if c.is_ascii_digit() {
                        consume_while(&mut tmp, &mut tmp_rel_end, |ch| ch.is_ascii_digit());
                        if let Some(&(i2, '#')) = tmp.peek() {
                            tmp.next();
                            tmp_rel_end = i2 + 1;
                            consume_while(&mut tmp, &mut tmp_rel_end, |ch| ch.is_ascii_alphanumeric() || ch == '_');
                            matched = true;
                        }
                    }
                }

                if matched {
                    rel_end = tmp_rel_end;
                    let end = start + rel_end;
                    tokens.push(self.make_token(TK_NUMBER, &self.code[start..end], start, end));
                    pos = end;
                    continue;
                }
            }

            if let Some(&(_, '0')) = chars.peek() {
                let mut tmp = chars.clone();
                tmp.next();
                if let Some(&(i2, base)) = tmp.peek() {
                    let digits = match base {
                        'b' | 'B' => "01_",
                        'o' | 'O' => "01234567_",
                        'x' | 'X' => "0123456789abcdefABCDEF_",
                        _ => "",
                    };
                    if !digits.is_empty() {
                        tmp.next();
                        let mut tmp_rel_end = i2 + base.len_utf8();
                        consume_while(&mut tmp, &mut tmp_rel_end, |ch| digits.contains(ch));
                        rel_end = tmp_rel_end;
                        let end = start + rel_end;
                        tokens.push(self.make_token(TK_NUMBER, &self.code[start..end], start, end));
                        pos = end;
                        continue;
                    }
                }
            }

            let mut has_digits = false;
            let mut dot_seen = false;
            let mut in_exponent = false;
            let mut last_char: Option<char> = None;

            while let Some(&(i, c)) = chars.peek() {
                let accept = match c {
                    '0'..='9' => { has_digits = true; true }
                    '_' => true,
                    '.' => {
                        if dot_seen || in_exponent { false } else if let Some((_, next_ch)) = chars.clone().nth(1) {
                            if next_ch == '.' { false } else { dot_seen = true; true }
                        } else { dot_seen = true; true }
                    }
                    'e' | 'E' => { if has_digits && !in_exponent { in_exponent = true; true } else { false } }
                    '+' | '-' => matches!(last_char, Some('e') | Some('E')),
                    _ => false,
                };
                if !accept { break; }
                rel_end = i + c.len_utf8();
                last_char = Some(c);
                chars.next();
            }

            if has_digits {
                let mut tmp = chars.clone();

                if let Some(&(i2, 'i')) = tmp.peek() {
                    let tmp_rel_end;
                    tmp_rel_end = i2 + 1;
                    tmp.next();
                    rel_end = tmp_rel_end;
                }

                let end = start + rel_end;
                tokens.push(self.make_token(TK_NUMBER, &self.code[start..end], start, end));
                pos = end;
                continue;
            }

            // WORD OPERATORS
            let mut matched = false;
            for &op in &WORD_OPERATORS {
                if slice.starts_with(op) {
                    let before_ok = pos == 0
                        || !self.code[..pos].chars().rev().next().unwrap().is_alphanumeric()
                        && self.code[..pos].chars().rev().next().unwrap() != '_';
                    let after_pos = pos + op.len();
                    let after_ok = after_pos >= len || {
                        let next_ch = self.code[after_pos..].chars().next().unwrap();
                        !next_ch.is_alphanumeric() && next_ch != '_'
                    };

                    if before_ok && after_ok {
                        tokens.push(self.make_token(TK_OPERATOR, op, pos, pos + op.len()));
                        pos += op.len();
                        matched = true;
                        break;
                    }
                }
            }
            if matched { continue; }

            // -li operator
            if slice.starts_with("-li") {
                tokens.push(self.make_token(TK_OPERATOR, "-li", pos, pos + 3));
                pos += 3;
                continue;
            }

            // SYMBOL OPERATORS
            let mut op_matched = false;
            for &op in &SYMBOL_OPERATORS {
                if slice.starts_with(op) {
                    tokens.push(self.make_token(TK_OPERATOR, op, pos, pos + op.len()));
                    pos += op.len();
                    op_matched = true;
                    break;
                }
            }
            if op_matched { continue; }

            // SEPARATORS
            let mut sep_matched = false;
            for &sep in &SEPARATORS {
                if slice.starts_with(sep) {
                    tokens.push(self.make_token(TK_SEPARATOR, sep, pos, pos + sep.len()));
                    pos += sep.len();
                    sep_matched = true;
                    break;
                }
            }
            if sep_matched { continue; }

            // IDENTIFIER
            if let Some(c) = slice.chars().next() {
                if Self::is_identifier_start(c) {
                    let mut end = pos + c.len_utf8();
                    for ch in self.code[end..].chars() {
                        if Self::is_identifier_continue(ch) {
                            end += ch.len_utf8();
                        } else { break; }
                    }
                    tokens.push(self.make_token(TK_IDENTIFIER, &self.code[pos..end], pos, end));
                    pos = end;
                    continue;
                }
            }

            // INVALID
            if let Some(c) = slice.chars().next() {
                tokens.push(self.make_token(
                    TK_INVALID,
                    &self.code[pos..pos + c.len_utf8()],
                    pos,
                    pos + c.len_utf8(),
                ));
                pos += c.len_utf8();
            }
        }

        tokens.push(Token::new_static(TK_EOF, Cow::Borrowed("\0"), tokens.last().and_then(|t| t.2.clone())));
        tokens
    }
}
