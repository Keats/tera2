use std::fmt;

use crate::errors::{Error, ErrorKind};
use crate::utils::Span;

// handwritten lexer, peekable iterator taken from minijinja

fn memstr(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    haystack
        .windows(needle.len())
        .position(|window| window == needle)
}

/// Will try to go over `-? {name} -?%}`.
/// Returns None if the name doesn't match the tag or the (offset, ws) tuple for the end of the tag
fn skip_tag(block_str: &str, name: &str) -> Option<(usize, bool)> {
    let mut ptr = block_str;

    if let Some(rest) = ptr.strip_prefix('-') {
        ptr = rest;
    }
    while let Some(rest) = ptr.strip_prefix(|x: char| x.is_ascii_whitespace()) {
        ptr = rest;
    }

    ptr = ptr.strip_prefix(name)?;

    while let Some(rest) = ptr.strip_prefix(|x: char| x.is_ascii_whitespace()) {
        ptr = rest;
    }
    let mut outer_ws = false;
    if let Some(rest) = ptr.strip_prefix('-') {
        ptr = rest;
        outer_ws = true;
    }
    ptr = ptr.strip_prefix("%}")?;

    Some((block_str.len() - ptr.len(), outer_ws))
}

/// We want to find the next time we see `{{`, `{%` or `{#`
fn find_start_marker(tpl: &str) -> Option<usize> {
    let bytes = tpl.as_bytes();
    let mut offset = 0;
    loop {
        let idx = &bytes[offset..].iter().position(|&x| x == b'{')?;
        if let Some(b'{') | Some(b'%') | Some(b'#') = bytes.get(offset + idx + 1) {
            return Some(offset + idx);
        }
        offset += idx + 1;
    }
}

enum State {
    /// Anything not in the other two states
    Template,
    /// In `{{ ... }}`
    Variable,
    /// In `{% ... %}`
    Tag,
}

#[derive(PartialEq)]
pub(crate) enum Token<'a> {
    Content(&'a str),
    // We handle the raw tag in the lexer but we have to emit a single token for it
    // so this is equivalent to `TagStart(bool), Content(&'a str), TagEnd(bool)`
    // This token will never appear in the parser
    RawContent(bool, &'a str, bool),

    VariableStart(bool),
    VariableEnd(bool),
    TagStart(bool),
    TagEnd(bool),
    // (start, end) of ws - never exposed to the parser
    Comment(bool, bool),
    Ident(&'a str),

    String(&'a str),
    Integer(i64),
    Float(f64),

    // math
    Mul,
    Div,
    FloorDiv,
    Mod,
    Plus,
    Minus,
    Power,

    // logic
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,

    // specific to Tera
    Tilde,
    Pipe,
    Assign,

    // Rest
    Dot,
    Comma,
    Colon,
    Bang,
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
}

impl<'a> fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Content(s) => write!(f, "CONTENT({:?})", s),
            Token::RawContent(ws_start, s, ws_end) => {
                write!(f, "RAW_CONTENT({}, {:?}, {})", ws_start, s, ws_end)
            }
            Token::VariableStart(ws) => write!(f, "VARIABLE_START({})", ws),
            Token::VariableEnd(ws) => write!(f, "VARIABLE_END({})", ws),
            Token::TagStart(ws) => write!(f, "TAG_START({})", ws),
            Token::TagEnd(ws) => write!(f, "TAG_END({})", ws),
            Token::Comment(start, end) => write!(f, "COMMENT({}, {})", start, end),
            Token::Ident(i) => write!(f, "IDENT({})", i),
            Token::String(s) => write!(f, "STRING({:?})", s),
            Token::Integer(i) => write!(f, "INTEGER({:?})", i),
            Token::Float(v) => write!(f, "FLOAT({:?})", v),
            Token::Plus => write!(f, "PLUS"),
            Token::Minus => write!(f, "MINUS"),
            Token::Mul => write!(f, "MUL"),
            Token::Div => write!(f, "DIV"),
            Token::FloorDiv => write!(f, "FLOORDIV"),
            Token::Power => write!(f, "POWER"),
            Token::Mod => write!(f, "MOD"),
            Token::Bang => write!(f, "BANG"),
            Token::Dot => write!(f, "DOT"),
            Token::Comma => write!(f, "COMMA"),
            Token::Colon => write!(f, "COLON"),
            Token::Tilde => write!(f, "TILDE"),
            Token::Assign => write!(f, "ASSIGN"),
            Token::Pipe => write!(f, "PIPE"),
            Token::Equal => write!(f, "EQ"),
            Token::NotEqual => write!(f, "NE"),
            Token::GreaterThan => write!(f, "GT"),
            Token::GreaterThanOrEqual => write!(f, "GTE"),
            Token::LessThan => write!(f, "LT"),
            Token::LessThanOrEqual => write!(f, "LTE"),
            Token::LeftBracket => write!(f, "LEFT_BRACKET"),
            Token::RightBracket => write!(f, "RIGHT_BRACKET"),
            Token::LeftParen => write!(f, "LEFT_PAREN"),
            Token::RightParen => write!(f, "RIGHT_PAREN"),
            Token::LeftBrace => write!(f, "LEFT_BRACE"),
            Token::RightBrace => write!(f, "RIGHT_BRACE"),
        }
    }
}

fn basic_tokenize(input: &str) -> impl Iterator<Item = Result<(Token<'_>, Span), Error>> {
    let mut rest = input;
    let mut stack = vec![State::Template];
    let mut current_line = 1;
    let mut current_col = 0;
    let mut current_byte = 0;
    let mut errored = false;

    macro_rules! syntax_error {
        ($message:expr, $span:expr) => {{
            errored = true;
            let mut err = Error::new(ErrorKind::SyntaxError, $message);
            err.set_span($span);
            return Some(Err(err));
        }};
    }

    macro_rules! loc {
        () => {
            (current_line, current_col, current_byte)
        };
    }

    macro_rules! make_span {
        ($start:expr) => {{
            let (start_line, start_col, start_byte) = $start;
            Span {
                start_line,
                start_col,
                end_line: current_line,
                end_col: current_col,
                range: start_byte..current_byte,
            }
        }};
    }

    macro_rules! advance {
        ($num_bytes:expr) => {{
            let (skipped, new_rest) = rest.split_at($num_bytes);
            for c in skipped.chars() {
                current_byte += 1;
                match c {
                    '\n' => {
                        current_line += 1;
                        current_col = 0;
                    }
                    _ => current_col += 1,
                }
            }
            rest = new_rest;
            skipped
        }};
    }

    macro_rules! check_ws_start {
        () => {{
            if rest.as_bytes().get(2) == Some(&b'-') {
                advance!(3);
                true
            } else {
                advance!(2);
                false
            }
        }};
    }

    macro_rules! lex_number {
        ($is_negative:expr) => {{
            let start_loc = loc!();
            let mut is_float = false;
            let num_len = rest
                .as_bytes()
                .iter()
                .take_while(|&&c| {
                    if !is_float && c == b'.' {
                        is_float = true;
                        true
                    } else {
                        c.is_ascii_digit()
                    }
                })
                .count();
            let num = advance!(num_len);
            if is_float {
                return Some(Ok((
                    Token::Float(match num.parse::<f64>() {
                        Ok(val) => val * if $is_negative { -1.0 } else { 1.0 },
                        Err(_) => syntax_error!("Invalid float", make_span!(start_loc)),
                    }),
                    make_span!(start_loc),
                )));
            } else {
                return Some(Ok((
                    Token::Integer(match num.parse::<i64>() {
                        Ok(val) => val * if $is_negative { -1 } else { 1 },
                        Err(_) => syntax_error!("Invalid Integer", make_span!(start_loc)),
                    }),
                    make_span!(start_loc),
                )));
            }
        }};
    }

    macro_rules! lex_string {
        ($delim:expr) => {{
            let start_loc = loc!();
            let str_len = rest
                .as_bytes()
                .iter()
                .skip(1)
                .take_while(|&&c| c != $delim)
                .count();
            if rest.as_bytes().get(str_len + 1) != Some(&$delim) {
                syntax_error!(
                    &format!(
                        "String opened with `{0}` is missing its closing `{0}`",
                        $delim as char
                    ),
                    make_span!(start_loc)
                )
            }
            let s = advance!(str_len + 2);
            return Some(Ok((
                Token::String(&s[1..s.len() - 1]),
                make_span!(start_loc),
            )));
        }};
    }

    std::iter::from_fn(move || loop {
        if rest.is_empty() | errored {
            return None;
        }

        let start_loc = loc!();

        match stack.last() {
            Some(State::Template) => {
                match rest.get(..2) {
                    Some("{{") => {
                        let ws = check_ws_start!();
                        stack.push(State::Variable);
                        return Some(Ok((Token::VariableStart(ws), make_span!(start_loc))));
                    }
                    Some("{%") => {
                        // If we have a `{% raw %}` block, we ignore everything until we see a `{% endraw %}`
                        // while still respecting whitespace
                        let ws = check_ws_start!();

                        if let Some((mut offset, end_ws_start_tag)) = skip_tag(&rest, "raw") {
                            let body_start_offset = offset;
                            // Then we see whether we find the start of the tag
                            while let Some(block) = memstr(&rest.as_bytes()[offset..], b"{%") {
                                let body_end_offset = offset;
                                offset += block + 2;
                                // Check if the tag starts with a {%- so we know we need to end trim the body
                                let start_ws_end_tag =
                                    rest.as_bytes().get(offset + 1) == Some(&b'-');
                                if let Some((endraw, ws_end)) = skip_tag(&rest[offset..], "endraw")
                                {
                                    let mut result = &rest[body_start_offset..body_end_offset];
                                    // Then we trim the inner body of the raw tag as needed directly here
                                    if end_ws_start_tag {
                                        result = result.trim_start();
                                    }
                                    if start_ws_end_tag {
                                        result = result.trim_end();
                                    }
                                    advance!(offset + endraw);
                                    return Some(Ok((
                                        Token::RawContent(ws, result, ws_end),
                                        make_span!(start_loc),
                                    )));
                                }
                            }
                            syntax_error!("unexpected end of raw block", make_span!(start_loc));
                        }

                        stack.push(State::Tag);
                        return Some(Ok((Token::TagStart(ws), make_span!(start_loc))));
                    }
                    Some("{#") => {
                        let ws_start = check_ws_start!();
                        if let Some(comment_end) = memstr(rest.as_bytes(), b"#}") {
                            let ws_end = rest.as_bytes().get(comment_end - 1) == Some(&b'-');
                            advance!(comment_end + 2);
                            return Some(Ok((
                                Token::Comment(ws_start, ws_end),
                                make_span!(start_loc),
                            )));
                        } else {
                            syntax_error!(
                                "Closing comment tag `#}` not found",
                                make_span!(start_loc)
                            );
                        }
                    }
                    _ => {}
                }

                let text = match find_start_marker(rest) {
                    Some(start) => advance!(start),
                    None => advance!(rest.len()),
                };
                return Some(Ok((Token::Content(text), make_span!(start_loc))));
            }
            Some(State::Variable) | Some(State::Tag) => {
                // Whitespaces are ignored in there
                match rest
                    .as_bytes()
                    .iter()
                    .position(|&x| !x.is_ascii_whitespace())
                {
                    Some(0) => {} // we got something to parse
                    Some(offset) => {
                        advance!(offset); // ignoring some ws
                        continue;
                    }
                    None => {
                        advance!(rest.len());
                        continue;
                    }
                }

                // First we check if we are the end of a tag/variable, safe unwrap
                match stack.last().unwrap() {
                    State::Tag => {
                        if let Some("-%}") = rest.get(..3) {
                            stack.pop();
                            advance!(3);
                            return Some(Ok((Token::TagEnd(true), make_span!(start_loc))));
                        }
                        if let Some("%}") = rest.get(..2) {
                            stack.pop();
                            advance!(2);
                            return Some(Ok((Token::TagEnd(false), make_span!(start_loc))));
                        }
                    }
                    State::Variable => {
                        if let Some("-}}") = rest.get(..3) {
                            stack.pop();
                            advance!(3);
                            return Some(Ok((Token::VariableEnd(true), make_span!(start_loc))));
                        }
                        if let Some("}}") = rest.get(..2) {
                            stack.pop();
                            advance!(2);
                            return Some(Ok((Token::VariableEnd(false), make_span!(start_loc))));
                        }
                    }
                    _ => unreachable!(),
                }

                // Then the longer operators
                let op = match rest.as_bytes().get(..2) {
                    Some(b"//") => Some(Token::FloorDiv),
                    Some(b"**") => Some(Token::Power),
                    Some(b"==") => Some(Token::Equal),
                    Some(b"!=") => Some(Token::NotEqual),
                    Some(b">=") => Some(Token::GreaterThanOrEqual),
                    Some(b"<=") => Some(Token::LessThanOrEqual),
                    _ => None,
                };
                if let Some(op) = op {
                    advance!(2);
                    return Some(Ok((op, make_span!(start_loc))));
                }

                // Then the rest of the ops, strings and numbers
                // strings and numbers will get returned inside the match so only operators are returned
                let op = match rest.as_bytes().get(0) {
                    Some(b'+') => Some(Token::Plus),
                    Some(b'-') => {
                        if rest.as_bytes().get(1).map_or(false, |x| x.is_ascii_digit()) {
                            advance!(1);
                            lex_number!(true);
                        }
                        Some(Token::Minus)
                    }
                    Some(b'*') => Some(Token::Mul),
                    Some(b'/') => Some(Token::Div),
                    Some(b'%') => Some(Token::Mod),
                    Some(b'!') => Some(Token::Bang),
                    Some(b'.') => Some(Token::Dot),
                    Some(b',') => Some(Token::Comma),
                    Some(b':') => Some(Token::Colon),
                    Some(b'~') => Some(Token::Tilde),
                    Some(b'|') => Some(Token::Pipe),
                    Some(b'=') => Some(Token::Assign),
                    Some(b'>') => Some(Token::GreaterThan),
                    Some(b'<') => Some(Token::LessThan),
                    Some(b'(') => Some(Token::LeftParen),
                    Some(b')') => Some(Token::RightParen),
                    Some(b'[') => Some(Token::LeftBracket),
                    Some(b']') => Some(Token::RightBracket),
                    Some(b'{') => Some(Token::LeftBrace),
                    Some(b'}') => Some(Token::RightBrace),
                    Some(b'\'') => lex_string!(b'\''),
                    Some(b'"') => lex_string!(b'"'),
                    Some(b'`') => lex_string!(b'`'),
                    Some(c) if c.is_ascii_digit() => lex_number!(false),
                    _ => None,
                };
                if let Some(op) = op {
                    advance!(1);
                    return Some(Ok((op, make_span!(start_loc))));
                }

                // Lastly, idents
                let ident_len = rest
                    .as_bytes()
                    .iter()
                    .enumerate()
                    .take_while(|&(idx, &c)| {
                        if c == b'_' {
                            true
                        } else if idx == 0 {
                            c.is_ascii_alphabetic()
                        } else {
                            c.is_ascii_alphanumeric()
                        }
                    })
                    .count();
                if ident_len > 0 {
                    let ident = advance!(ident_len);
                    return Some(Ok((Token::Ident(ident), make_span!(start_loc))));
                }

                syntax_error!("Unexpected character", make_span!(start_loc));
            }
            None => unreachable!("Lexer should never be in that state"),
        }
    })
}

/// Automatically removes whitespace around blocks when asked.
fn whitespace_filter<'a, I: Iterator<Item = Result<(Token<'a>, Span), Error>>>(
    iter: I,
) -> impl Iterator<Item = Result<(Token<'a>, Span), Error>> {
    let mut iter = iter.peekable();
    let mut remove_leading_ws = false;

    macro_rules! handle_content_tokens {
        ($data:expr, $span:expr, $remove_leading_ws: expr) => {{
            if remove_leading_ws {
                remove_leading_ws = false;
                $data = $data.trim_start();
            }
            if $remove_leading_ws {
                remove_leading_ws = $remove_leading_ws;
            }

            if matches!(
                iter.peek(),
                Some(Ok((Token::VariableStart(true), _)))
                    | Some(Ok((Token::TagStart(true), _)))
                    | Some(Ok((Token::RawContent(true, _, _), _)))
            ) {
                $data = $data.trim_end();
            }

            Some(Ok((Token::Content($data), $span)))
        }};
    };

    std::iter::from_fn(move || match iter.next() {
        Some(Ok((Token::Content(mut data), span))) => {
            handle_content_tokens!(data, span, false)
        }
        Some(Ok((Token::RawContent(_, mut data, ws_end), span))) => {
            handle_content_tokens!(data, span, ws_end)
        }
        rv @ Some(Ok((Token::VariableEnd(true), _)))
        | rv @ Some(Ok((Token::TagStart(true), _))) => {
            remove_leading_ws = true;
            rv
        }
        Some(Ok((Token::Comment(_, true), span))) => {
            remove_leading_ws = true;
            // Empty content nodes will get removed by the parser
            Some(Ok((Token::Content(""), span)))
        }
        other => {
            remove_leading_ws = false;
            other
        }
    })
}

pub(crate) fn tokenize(input: &str) -> impl Iterator<Item = Result<(Token<'_>, Span), Error>> {
    whitespace_filter(basic_tokenize(input))
}
