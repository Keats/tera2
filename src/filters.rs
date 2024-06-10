use std::collections::BTreeSet;
use std::fmt::Write;
use std::sync::Arc;

use crate::args::{ArgFromValue, Kwargs};
use crate::errors::{Error, TeraResult};
use crate::value::FunctionResult;
use crate::Value;

/// The filter function type definition
pub trait Filter<Arg, Res>: Sync + Send + 'static {
    /// The filter function type definition
    fn call(&self, value: Arg, kwargs: Kwargs) -> Res;

    /// Whether the current filter's output should be treated as safe, defaults to `false`
    /// Only needs to be defined if the filter returns a string
    fn is_safe(&self) -> bool {
        false
    }
}

impl<Func, Arg, Res> Filter<Arg, Res> for Func
where
    Func: Fn(Arg, Kwargs) -> Res + Sync + Send + 'static,
    Arg: for<'a> ArgFromValue<'a>,
    Res: FunctionResult,
{
    fn call(&self, value: Arg, kwargs: Kwargs) -> Res {
        (self)(value, kwargs)
    }
}

type FilterFunc = dyn Fn(&Value, Kwargs) -> TeraResult<Value> + Sync + Send + 'static;

#[derive(Clone)]
pub(crate) struct StoredFilter(Arc<FilterFunc>);

impl StoredFilter {
    pub fn new<Func, Arg, Res>(f: Func) -> Self
    where
        Func: Filter<Arg, Res> + for<'a> Filter<<Arg as ArgFromValue<'a>>::Output, Res>,
        Arg: for<'a> ArgFromValue<'a>,
        Res: FunctionResult,
    {
        let closure = move |arg: &Value, kwargs| -> TeraResult<Value> {
            f.call(Arg::from_value(arg)?, kwargs).into_result()
        };

        StoredFilter(Arc::new(closure))
    }

    pub fn call(&self, arg: &Value, kwargs: Kwargs) -> TeraResult<Value> {
        (self.0)(arg, kwargs)
    }
}

pub(crate) fn safe(val: &str, _: Kwargs) -> Value {
    Value::safe_string(val)
}

pub(crate) fn default(val: Value, kwargs: Kwargs) -> TeraResult<Value> {
    let default_val = kwargs.must_get::<Value>("value")?;

    match val {
        Value::Undefined => Ok(default_val),
        _ => Ok(val),
    }
}

pub(crate) fn upper(val: &str, _: Kwargs) -> String {
    val.to_uppercase()
}

pub(crate) fn lower(val: &str, _: Kwargs) -> String {
    val.to_lowercase()
}

pub(crate) fn trim(val: &str, kwargs: Kwargs) -> TeraResult<String> {
    if let Some(pat) = kwargs.get::<&str>("pat")? {
        Ok(val
            .trim_start_matches(pat)
            .trim_end_matches(pat)
            .to_string())
    } else {
        Ok(val.trim().to_string())
    }
}

pub(crate) fn trim_start(val: &str, kwargs: Kwargs) -> TeraResult<String> {
    if let Some(pat) = kwargs.get::<&str>("pat")? {
        Ok(val.trim_start_matches(pat).to_string())
    } else {
        Ok(val.trim_start().to_string())
    }
}

pub(crate) fn trim_end(val: &str, kwargs: Kwargs) -> TeraResult<String> {
    if let Some(pat) = kwargs.get::<&str>("pat")? {
        Ok(val.trim_end_matches(pat).to_string())
    } else {
        Ok(val.trim_end().to_string())
    }
}

pub(crate) fn replace(val: &str, kwargs: Kwargs) -> TeraResult<String> {
    let from = kwargs.must_get::<&str>("from")?;
    let to = kwargs.must_get::<&str>("to")?;

    Ok(val.replace(from, to))
}

/// Uppercase the first char and lowercase the rest.
pub(crate) fn capitalize(val: &str, _: Kwargs) -> String {
    let mut chars = val.chars();
    match chars.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase(),
    }
}

/// Uppercase the first letter of each word
pub(crate) fn title(val: &str, _: Kwargs) -> String {
    let mut res = String::with_capacity(val.len());
    let mut capitalize = true;
    for c in val.chars() {
        if c.is_ascii_punctuation() || c.is_whitespace() {
            res.push(c);
            // Special case the apostrophe so that it doesn't mess up the English 's etc
            if c != '\'' {
                capitalize = true;
            }
        } else if capitalize {
            write!(res, "{}", c.to_uppercase()).unwrap();
            capitalize = false;
        } else {
            write!(res, "{}", c.to_lowercase()).unwrap();
        }
    }
    res
}

pub(crate) fn str(val: Value, _: Kwargs) -> String {
    format!("{val}")
}

/// Converts a Value into an int. It defaults to a base of `10` but can be changed.
pub(crate) fn int(val: Value, kwargs: Kwargs) -> TeraResult<Value> {
    let base = kwargs.get::<u32>("base")?.unwrap_or(10);

    let handle_f64 = |v: f64| {
        if v.fract() == 0.0 {
            Ok((v as i128).into())
        } else {
            Err(Error::message(format!(
                "The float {v} would have to be truncated to convert to an int"
            )))
        }
    };

    match val {
        Value::String(s, _) => {
            let s = s.trim();
            let s = match base {
                2 => s.trim_start_matches("0b"),
                8 => s.trim_start_matches("0o"),
                16 => s.trim_start_matches("0x"),
                _ => s,
            };
            match i128::from_str_radix(s, base) {
                Ok(v) => Ok(v.into()),
                Err(_) => {
                    if s.contains('.') {
                        match s.parse::<f64>() {
                            Ok(f) => handle_f64(f),
                            Err(_) => Err(Error::message(format!(
                                "The string `{s}` cannot be converted to an int in base {base}"
                            ))),
                        }
                    } else {
                        Err(Error::message(format!(
                            "The string `{s}` cannot be converted to an int in base {base}"
                        )))
                    }
                }
            }
        }
        Value::U64(v) => Ok(v.into()),
        Value::I64(v) => Ok(v.into()),
        Value::I128(v) => Ok(v.into()),
        Value::U128(v) => Ok(v.into()),
        Value::F64(v) => handle_f64(v),
        _ => Err(Error::message(format!(
            "Value of type {} cannot be converted to an int",
            val.name()
        ))),
    }
}

pub(crate) fn float(val: Value, _: Kwargs) -> TeraResult<f64> {
    match val {
        Value::String(s, _) => {
            let s = s.trim();
            if let Ok(num) = s.parse::<f64>() {
                Ok(num)
            } else {
                Err(Error::message(format!(
                    "The string `{s}` cannot be converted to a float"
                )))
            }
        }
        _ => {
            if let Some(num) = val.as_number() {
                Ok(num.as_float())
            } else {
                Err(Error::message(format!(
                    "Value of type {} cannot be converted to a float",
                    val.name()
                )))
            }
        }
    }
}

pub(crate) fn length(val: Value, _: Kwargs) -> TeraResult<usize> {
    match val.len() {
        Some(v) => Ok(v),
        None => Err(Error::message(format!(
            "Value of type {} has no length",
            val.name()
        ))),
    }
}

pub(crate) fn reverse(val: Value, _: Kwargs) -> TeraResult<Value> {
    val.reverse()
}

pub(crate) fn split(val: &str, kwargs: Kwargs) -> TeraResult<Value> {
    let pat = kwargs.must_get::<&str>("pat")?;
    Ok(val
        .split(pat)
        .map(Into::into)
        .collect::<Vec<Value>>()
        .into())
}

pub(crate) fn abs(val: Value, _: Kwargs) -> TeraResult<Value> {
    match val {
        Value::U64(_) | Value::U128(_) => Ok(val),
        Value::F64(v) => Ok(v.abs().into()),
        Value::I64(v) => match v.checked_abs() {
            Some(v) => Ok(v.into()),
            None => Ok((v as i128).abs().into()),
        },
        Value::I128(v) => match v.checked_abs() {
            Some(v) => Ok(v.into()),
            None => Err(Error::message(format!(
                "Errored while getting absolute value: it is i128::MIN value."
            ))),
        },
        _ => Err(Error::message(format!(
            "This filter can only be used on a number, received `{}`.",
            val.name()
        ))),
    }
}

pub(crate) fn round(val: f64, kwargs: Kwargs) -> TeraResult<Value> {
    let method = kwargs.get::<&str>("method")?;
    let precision = kwargs.get::<i32>("precision")?.unwrap_or_default();
    let multiplier = if precision == 0 {
        1.0
    } else {
        10.0_f64.powi(precision)
    };

    match method {
        Some("ceil") => Ok(((multiplier * val).ceil() / multiplier).into()),
        Some("floor") => Ok(((multiplier * val).floor() / multiplier).into()),
        None => Ok(((multiplier * val).round() / multiplier).into()),
        Some(m) => Err(Error::message(format!(
            "Invalid argument for `method`: {m}. \
                Only `ceil` and `floor` are allowed. \
                Do not fill this parameter if you want a classic round."
        ))),
    }
}

/// Returns the first element of an array. Null if the array is empty
/// and errors if the value is not an array
pub(crate) fn first(val: Vec<Value>, _: Kwargs) -> TeraResult<Value> {
    Ok(val.first().cloned().unwrap_or(Value::Null))
}

/// Returns the last element of an array. Null if the array is empty
/// and errors if the value is not an array
pub(crate) fn last(val: Vec<Value>, _: Kwargs) -> TeraResult<Value> {
    Ok(val.last().cloned().unwrap_or(Value::Null))
}

/// Returns the nth element of an array. Null if there isn't an element at that index.
/// and errors if the value is not an array
pub(crate) fn nth(val: Vec<Value>, kwargs: Kwargs) -> TeraResult<Value> {
    let n = kwargs.must_get::<usize>("n")?;
    Ok(val.into_iter().nth(n).unwrap_or(Value::Null))
}

/// Joins the elements
pub(crate) fn join(val: Vec<Value>, kwargs: Kwargs) -> TeraResult<String> {
    let sep = kwargs.get::<&str>("sep")?.unwrap_or("");
    Ok(val
        .into_iter()
        .map(|x| format!("{x}"))
        .collect::<Vec<_>>()
        .join(sep))
}

/// Slice the array
/// Use the `start` argument to define where to start (inclusive, default to `0`)
/// and `end` argument to define where to stop (exclusive, default to the length of the array)
/// `start` and `end` are 0-indexed
pub(crate) fn slice(val: Vec<Value>, kwargs: Kwargs) -> TeraResult<Value> {
    if val.is_empty() {
        return Ok(Value::Array(Arc::new(Vec::new())));
    }

    let get_index = |i| {
        if i >= 0 {
            i as usize
        } else {
            (val.len() as isize + i) as usize
        }
    };
    let start = get_index(kwargs.get::<isize>("start")?.unwrap_or_default());
    let mut end = get_index(
        kwargs
            .get::<isize>("end")?
            .unwrap_or_else(|| val.len() as isize),
    );
    if end > val.len() {
        end = val.len();
    }
    // Not an error, but returns an empty Vec
    if start >= end {
        return Ok(Value::Array(Arc::new(Vec::new())));
    }
    Ok(val[start..end].into())
}

pub(crate) fn unique(val: Vec<Value>, _: Kwargs) -> Vec<Value> {
    let mut seen = BTreeSet::new();
    let mut res = Vec::with_capacity(val.len());

    for v in val {
        if !seen.contains(&v) {
            seen.insert(v.clone());
            res.push(v);
        }
    }

    res
}

/// Map retrieves an attribute from a list of objects.
/// The 'attribute' argument specifies what to retrieve.
pub(crate) fn map(val: Vec<Value>, kwargs: Kwargs) -> TeraResult<Vec<Value>> {
    let attribute = kwargs.must_get::<&str>("attribute")?;
    // TODO: allow passing a filter/test name to apply to every element?
    // TODO: allow passing a default value when it's undefined
    Ok(val
        .into_iter()
        .map(|x| x.get_from_path(attribute))
        .collect::<Vec<_>>())
}

// TODO: missing from array sort, group_by, filter, map
// TODO: handle error for wrong arguments in the interpreter
// TODO: add indent after making sure it's good. Tests could be insta for easy viz
// TODO: allow accessing some state (context, filters, tests etc) from filter (3rd arg)

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Map;

    #[test]
    fn test_upper() {
        assert_eq!(upper("hello", Kwargs::default()), "HELLO");
    }

    #[test]
    fn test_lower() {
        assert_eq!(lower("HELLO", Kwargs::default()), "hello");
    }

    #[test]
    fn test_trim() {
        assert_eq!(trim("  hello  ", Kwargs::default()).unwrap(), "hello");
        let mut map = Map::new();
        map.insert("pat".into(), "$".into());
        assert_eq!(
            trim("$ hello $", Kwargs::new(Arc::new(map))).unwrap(),
            " hello "
        );
    }

    #[test]
    fn test_trim_start() {
        assert_eq!(
            trim_start("  hello  ", Kwargs::default()).unwrap(),
            "hello  "
        );
        let mut map = Map::new();
        map.insert("pat".into(), "$".into());
        assert_eq!(
            trim_start("$ hello $", Kwargs::new(Arc::new(map))).unwrap(),
            " hello $"
        );
    }

    #[test]
    fn test_trim_end() {
        assert_eq!(trim_end("  hello  ", Kwargs::default()).unwrap(), "  hello");
        let mut map = Map::new();
        map.insert("pat".into(), "$".into());
        assert_eq!(
            trim_end("$ hello $", Kwargs::new(Arc::new(map))).unwrap(),
            "$ hello "
        );
    }

    #[test]
    fn test_replace() {
        let mut map = Map::new();
        map.insert("from".into(), "$".into());
        map.insert("to".into(), "€".into());
        assert_eq!(
            replace("$ hello $", Kwargs::new(Arc::new(map))).unwrap(),
            "€ hello €"
        );
    }

    #[test]
    fn test_capitalize() {
        assert_eq!(capitalize("HELLO", Kwargs::default()), "Hello");
    }

    #[test]
    fn test_title() {
        let tests = vec![
            ("foo bar", "Foo Bar"),
            ("foo\tbar", "Foo\tBar"),
            ("foo  bar", "Foo  Bar"),
            ("f bar f", "F Bar F"),
            ("foo-bar", "Foo-Bar"),
            ("FOO\tBAR", "Foo\tBar"),
            ("foo (bar)", "Foo (Bar)"),
            ("foo (bar) ", "Foo (Bar) "),
            ("foo {bar}", "Foo {Bar}"),
            ("foo [bar]", "Foo [Bar]"),
            ("foo <bar>", "Foo <Bar>"),
            ("  foo  bar", "  Foo  Bar"),
            ("\tfoo\tbar\t", "\tFoo\tBar\t"),
            ("foo bar ", "Foo Bar "),
            ("foo bar\t", "Foo Bar\t"),
            ("foo's bar", "Foo's Bar"),
        ];
        for (input, expected) in tests {
            assert_eq!(title(input, Kwargs::default()), expected);
        }
    }

    #[test]
    fn test_str() {
        assert_eq!(str((2.1).into(), Kwargs::default()), "2.1");
        assert_eq!(str(2.into(), Kwargs::default()), "2");
        assert_eq!(str(true.into(), Kwargs::default()), "true");
        assert_eq!(str(vec![1, 2, 3].into(), Kwargs::default()), "[1, 2, 3]");
        let mut map = Map::new();
        map.insert("hello".into(), "world".into());
        map.insert("other".into(), 2.into());
        assert_eq!(
            str(map.into(), Kwargs::default()),
            "{hello: 'world', other: 2}"
        );
    }

    #[test]
    fn test_int() {
        // String to int
        let tests: Vec<(&str, i64)> = vec![
            ("0", 0),
            ("-5", -5),
            ("9223372036854775807", i64::max_value()),
            ("1.00", 1),
        ];
        for (input, expected) in tests {
            assert_eq!(
                int(input.into(), Kwargs::default()).unwrap(),
                expected.into()
            );
        }

        let mut map = Map::new();
        map.insert("base".into(), 2.into());
        assert_eq!(
            int("0b1010".into(), Kwargs::new(Arc::new(map))).unwrap(),
            10.into()
        );

        // We don't do anything in that case
        assert_eq!(
            int((-5 as i128).into(), Kwargs::default()).unwrap(),
            (-5 as i128).into()
        );

        // Can't convert without truncating
        assert!(int(1.12.into(), Kwargs::default()).is_err());

        // Doesn't make sense
        assert!(int("hello".into(), Kwargs::default()).is_err());
        assert!(int(vec![1, 2].into(), Kwargs::default()).is_err());
    }

    #[test]
    fn test_float() {
        assert_eq!(float("1".into(), Kwargs::default()).unwrap(), 1.0.into());
        assert_eq!(
            float("3.14".into(), Kwargs::default()).unwrap(),
            3.14.into()
        );
        assert_eq!(float(1.into(), Kwargs::default()).unwrap(), 1.0.into());
        // noop
        assert_eq!(float(1.12.into(), Kwargs::default()).unwrap(), 1.12.into());
        // Doesn't make sense
        assert!(float("hello".into(), Kwargs::default()).is_err());
        assert!(float(vec![1, 2].into(), Kwargs::default()).is_err());
    }

    #[test]
    fn test_abs() {
        assert_eq!(abs(1.into(), Kwargs::default()).unwrap(), 1.into());
        assert_eq!(abs((-1i64).into(), Kwargs::default()).unwrap(), 1.into());
        assert_eq!(abs((-1.0).into(), Kwargs::default()).unwrap(), (1.0).into());
        assert!(abs(i128::MIN.into(), Kwargs::default()).is_err());
        assert!(abs("hello".into(), Kwargs::default()).is_err());
    }

    #[test]
    fn test_round() {
        assert_eq!(round((2.1).into(), Kwargs::default()).unwrap(), 2.into());

        let mut map = Map::new();
        map.insert("method".into(), "ceil".into());
        assert_eq!(
            round((2.1).into(), Kwargs::new(Arc::new(map))).unwrap(),
            3.into()
        );

        let mut map = Map::new();
        map.insert("method".into(), "floor".into());
        assert_eq!(
            round((2.9).into(), Kwargs::new(Arc::new(map))).unwrap(),
            2.into()
        );

        let mut map = Map::new();
        map.insert("precision".into(), 2.into());
        assert_eq!(
            round((2.245).into(), Kwargs::new(Arc::new(map))).unwrap(),
            (2.25).into()
        );
    }

    #[test]
    fn test_slice() {
        let v: Vec<Value> = vec![1, 2, 3, 4, 5].into_iter().map(Into::into).collect();

        let inputs = vec![
            ((Some(1), None), vec![2, 3, 4, 5]),
            ((None, Some(2)), vec![1, 2]),
            ((Some(1), Some(2)), vec![2]),
            ((None, Some(-2)), vec![1, 2, 3]),
            ((None, None), vec![1, 2, 3, 4, 5]),
            ((Some(3), Some(1)), vec![]),
            ((Some(9), None), vec![]),
        ];

        for ((start, end), expected) in inputs {
            let mut map = Map::new();
            if let Some(s) = start {
                map.insert("start".into(), s.into());
            }
            if let Some(s) = end {
                map.insert("end".into(), s.into());
            }
            assert_eq!(
                slice(v.clone(), Kwargs::new(Arc::new(map))).unwrap(),
                Value::from(expected)
            );
        }
    }
}
