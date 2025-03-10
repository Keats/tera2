use percent_encoding::{percent_encode, AsciiSet, NON_ALPHANUMERIC};
use tera::{Kwargs, State};

/// https://url.spec.whatwg.org/#fragment-percent-encode-set
const FRAGMENT_ENCODE_SET: &AsciiSet = &percent_encoding::CONTROLS
    .add(b' ')
    .add(b'"')
    .add(b'<')
    .add(b'>')
    .add(b'`');

/// https://url.spec.whatwg.org/#path-percent-encode-set
const PATH_ENCODE_SET: &AsciiSet = &FRAGMENT_ENCODE_SET.add(b'#').add(b'?').add(b'{').add(b'}');

/// https://url.spec.whatwg.org/#userinfo-percent-encode-set
const USERINFO_ENCODE_SET: &AsciiSet = &PATH_ENCODE_SET
    .add(b'/')
    .add(b':')
    .add(b';')
    .add(b'=')
    .add(b'@')
    .add(b'[')
    .add(b'\\')
    .add(b']')
    .add(b'^')
    .add(b'|');

/// Same as Python quote
/// https://github.com/python/cpython/blob/da27d9b9dc44913ffee8f28d9638985eaaa03755/Lib/urllib/parse.py#L787
/// with `/` not escaped
const PYTHON_ENCODE_SET: &AsciiSet = &USERINFO_ENCODE_SET
    .remove(b'/')
    .add(b':')
    .add(b'?')
    .add(b'#')
    .add(b'[')
    .add(b']')
    .add(b'@')
    .add(b'!')
    .add(b'$')
    .add(b'&')
    .add(b'\'')
    .add(b'(')
    .add(b')')
    .add(b'*')
    .add(b'+')
    .add(b',')
    .add(b';')
    .add(b'=');

/// Percent-encodes reserved URI characters
pub fn urlencode(val: &str, _: Kwargs, _: &State) -> String {
    percent_encode(val.as_bytes(), PYTHON_ENCODE_SET).to_string()
}

/// Percent-encodes reserved URI characters
pub fn urlencode_strict(val: &str, _: Kwargs, _: &State) -> String {
    percent_encode(val.as_bytes(), NON_ALPHANUMERIC).to_string()
}
