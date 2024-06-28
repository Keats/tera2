use crate::errors::ReportError;

fn get_line_starts(source: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(source.match_indices('\n').map(|(i, _)| i + 1))
        .collect()
}

pub fn generate_report(
    error: &ReportError,
    filename: &str,
    source: &str,
    err_type: &str,
) -> String {
    let line_starts: Vec<_> = get_line_starts(source);
    let start_line = error.span.start_line;
    let start_col = error.span.start_col;
    let spacing = " ";
    let line = if start_line == line_starts.len() {
        &source[line_starts[start_line - 1]..]
    } else {
        &source[line_starts[start_line - 1]..line_starts[start_line]]
    }
    .trim_end_matches('\n');
    let mut underline = String::with_capacity(100);
    let underline_offset = if start_col > 0 { start_col - 1 } else { 0 };
    for c in line.chars().take(underline_offset) {
        match c {
            '\t' => underline.push('\t'),
            _ => underline.push(' '),
        }
    }
    // TODO: push variable amount of - depending on end_col-start_col
    underline.push_str(" ^---");
    let message = &error.message;

    format!(
        "{spacing}{err_type} --> [{filename}:{start_line}:{start_col}]\n\
         {spacing} |\n\
         {start_line} | {line}\n\
         {spacing} | {underline}\n\
         {spacing} = {message}"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::Span;

    #[test]
    fn can_get_line_starts() {
        let source = "foo\nbar\r\n\nbaz";
        let line_starts = get_line_starts(source);
        assert_eq!(
            line_starts,
            [
                0,  // "foo\n"
                4,  // "bar\r\n"
                9,  // ""
                10, // "baz"
            ],
        );
    }

    #[test]
    fn can_render_syntax_error() {
        let source = "foo\nbar\r\n\nbaz";
        let err = ReportError::new(
            "Cannot bar the foo".to_string(),
            &Span {
                start_line: 1,
                start_col: 0,
                end_line: 1,
                end_col: 3,
                range: 4..7,
            },
        );
        let out = generate_report(&err, "test.html", source, "Syntax error");
        println!("{out}");
        assert_eq!(
            out,
            r#" Syntax error --> [test.html:1:0]
  |
1 | foo
  |  ^---
  = Cannot bar the foo"#
        );
    }
}
