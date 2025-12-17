use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::{LazyLock, RwLock};

use regex::Regex;
use tera::{Kwargs, State, TeraResult, Test};

static STRIPTAGS_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(<!--.*?-->|<[^>]*>)").unwrap());

static SPACELESS_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r">\s+<").unwrap());

pub fn striptags<'a>(val: &'a str, _: Kwargs, _: &'a State) -> Cow<'a, str> {
    STRIPTAGS_RE.replace_all(val, "")
}

pub fn spaceless<'a>(val: &'a str, _: Kwargs, _: &'a State) -> Cow<'a, str> {
    SPACELESS_RE.replace_all(val, "><")
}

#[derive(Debug, Default)]
pub struct Matching {
    cache: RwLock<HashMap<String, Regex>>,
}

impl Matching {
    fn get_or_create_regex(&self, pattern: &str) -> TeraResult<Regex> {
        if let Some(r) = self.cache.read().unwrap().get(pattern) {
            return Ok(r.clone());
        }

        let mut cache = self.cache.write().unwrap();

        let regex = match Regex::new(pattern) {
            Ok(regex) => regex,
            Err(e) => return Err(tera::Error::message(format!("Invalid regex: {e}"))),
        };

        cache.insert(String::from(pattern), regex.clone());
        Ok(regex)
    }
}

impl Test<&str, TeraResult<bool>> for Matching {
    fn call(&self, val: &str, kwargs: Kwargs, _: &State) -> TeraResult<bool> {
        let pat = kwargs.must_get::<&str>("pat")?;
        let regex = self.get_or_create_regex(pat)?;
        Ok(regex.is_match(val))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use tera::Context;
    use tera::value::Map;

    #[test]
    fn test_striptags() {
        let tests = vec![
            (
                r"<b>Joel</b> <button>is</button> a <span>slug</span>",
                "Joel is a slug",
            ),
            (
                r#"<p>just a small   \n <a href="x"> example</a> link</p>\n<p>to a webpage</p><!-- <p>and some commented stuff</p> -->"#,
                r#"just a small   \n  example link\nto a webpage"#,
            ),
            (
                r"<p>See: &#39;&eacute; is an apostrophe followed by e acute</p>",
                r"See: &#39;&eacute; is an apostrophe followed by e acute",
            ),
            (r"<adf>a", "a"),
            (r"</adf>a", "a"),
            (r"<asdf><asdf>e", "e"),
            (r"hi, <f x", "hi, <f x"),
            ("234<235, right?", "234<235, right?"),
            ("a4<a5 right?", "a4<a5 right?"),
            ("b7>b2!", "b7>b2!"),
            ("</fe", "</fe"),
            ("<x>b<y>", "b"),
            (r#"a<p a >b</p>c"#, "abc"),
            (r#"d<a:b c:d>e</p>f"#, "def"),
            (
                r#"<strong>foo</strong><a href="http://example.com">bar</a>"#,
                "foobar",
            ),
        ];
        for (input, expected) in tests {
            let ctx = Context::new();
            let state = State::new(&ctx);
            let res = striptags(input, Kwargs::default(), &state);
            assert_eq!(expected, res);
        }
    }

    #[test]
    fn test_spaceless() {
        let tests = vec![
            ("<p>\n<a>test</a>\r\n </p>", "<p><a>test</a></p>"),
            ("<p>\n<a> </a>\r\n </p>", "<p><a></a></p>"),
            ("<p> </p>", "<p></p>"),
            ("<p> <a>", "<p><a>"),
            ("<p> test</p>", "<p> test</p>"),
            ("<p>\r\n</p>", "<p></p>"),
        ];
        for (input, expected) in tests {
            let ctx = Context::new();
            let state = State::new(&ctx);
            let res = spaceless(input, Kwargs::default(), &state);
            assert_eq!(expected, res);
        }
    }

    #[test]
    fn test_matching() {
        let inputs = vec![
            ("abc", "b", true),
            ("abc", "^b$", false),
            ("Hello, World!", r"(?i)(hello\W\sworld\W)", true),
            ("The date was 2018-06-28", r"\d{4}-\d{2}-\d{2}$", true),
        ];

        for (input, pat, expected) in inputs {
            let matching = Matching::default();
            let mut map = Map::new();
            map.insert("pat".into(), pat.into());
            let kwargs = Kwargs::new(Arc::new(map));
            let ctx = Context::new();
            let res = matching.call(input, kwargs, &State::new(&ctx)).unwrap();
            assert_eq!(expected, res);
        }
    }
}
