use std::collections::BTreeSet;
use std::fmt::Write;
use std::sync::Arc;

use crate::args::{ArgFromValue, Kwargs};
use crate::errors::{Error, TeraResult};
use crate::value::number::Number;
use crate::value::{Key, Map};
use crate::vm::state::State;
use crate::{HashMap, Value};

pub trait TestResult {
    fn into_result(self) -> TeraResult<bool>;
}

impl TestResult for TeraResult<bool> {
    fn into_result(self) -> TeraResult<bool> {
        self
    }
}

impl TestResult for bool {
    fn into_result(self) -> TeraResult<bool> {
        Ok(self)
    }
}

/// The test function type definition
pub trait Test<Arg, Res>: Sync + Send + 'static {
    fn call(&self, value: Arg, kwargs: Kwargs, state: &State) -> Res;
}

impl<Func, Arg, Res> Test<Arg, Res> for Func
where
    Func: Fn(Arg, Kwargs, &State) -> Res + Sync + Send + 'static,
    Arg: for<'a> ArgFromValue<'a>,
    Res: TestResult,
{
    fn call(&self, value: Arg, kwargs: Kwargs, state: &State) -> Res {
        (self)(value, kwargs, state)
    }
}

type TestFunc = dyn Fn(&Value, Kwargs, &State) -> TeraResult<bool> + Sync + Send + 'static;

#[derive(Clone)]
pub(crate) struct StoredTest(Arc<TestFunc>);

impl StoredTest {
    pub fn new<Func, Arg, Res>(f: Func) -> Self
    where
        Func: Test<Arg, Res> + for<'a> Test<<Arg as ArgFromValue<'a>>::Output, Res>,
        Arg: for<'a> ArgFromValue<'a>,
        Res: TestResult,
    {
        let closure = move |arg: &Value, kwargs, state: &State| -> TeraResult<bool> {
            f.call(Arg::from_value(arg)?, kwargs, state).into_result()
        };

        StoredTest(Arc::new(closure))
    }

    pub fn call(&self, arg: &Value, kwargs: Kwargs, state: &State) -> TeraResult<bool> {
        (self.0)(arg, kwargs, state)
    }
}

pub(crate) fn is_string(val: &Value, _: Kwargs, _: &State) -> bool {
    matches!(val, Value::String(..))
}

pub(crate) fn is_number(val: &Value, _: Kwargs, _: &State) -> bool {
    val.is_number()
}

pub(crate) fn is_map(val: &Value, _: Kwargs, _: &State) -> bool {
    matches!(val, Value::Map(..))
}

pub(crate) fn is_array(val: &Value, _: Kwargs, _: &State) -> bool {
    matches!(val, Value::Array(..))
}

pub(crate) fn is_null(val: &Value, _: Kwargs, _: &State) -> bool {
    matches!(val, Value::Null)
}

// TODO: check whether we need that
pub(crate) fn is_undefined(val: &Value, _: Kwargs, _: &State) -> bool {
    matches!(val, Value::Null)
}

pub(crate) fn is_integer(val: &Value, _: Kwargs, _: &State) -> bool {
    if let Some(num) = val.as_number() {
        num.is_integer()
    } else {
        false
    }
}

pub(crate) fn is_float(val: &Value, _: Kwargs, _: &State) -> bool {
    if let Some(num) = val.as_number() {
        num.is_float()
    } else {
        false
    }
}

pub(crate) fn is_odd(val: Number, _: Kwargs, _: &State) -> TeraResult<bool> {
    match val {
        Number::Integer(u) => Ok(u % 2 != 0),
        Number::Float(u) => Err(Error::message(format!(
            "Value `{u}` is a float; cannot determine if it's odd"
        ))),
    }
}

pub(crate) fn is_even(val: Number, _: Kwargs, _: &State) -> TeraResult<bool> {
    match val {
        Number::Integer(u) => Ok(u % 2 == 0),
        Number::Float(u) => Err(Error::message(format!(
            "Value `{u}` is a float; cannot determine if it's even"
        ))),
    }
}

pub(crate) fn is_divisible_by(val: Number, kwargs: Kwargs, _: &State) -> TeraResult<bool> {
    let divisor = kwargs.must_get::<i128>("divisor")?;
    if divisor == 0 {
        return Ok(false);
    }
    match val {
        Number::Integer(u) => Ok(u % divisor == 0),
        Number::Float(u) => Err(Error::message(format!(
            "Value `{u}` is a float; cannot determine if it's even"
        ))),
    }
}

pub(crate) fn is_starting_with(val: &str, kwargs: Kwargs, _: &State) -> TeraResult<bool> {
    let pat = kwargs.must_get::<&str>("pat")?;
    Ok(val.starts_with(pat))
}

pub(crate) fn is_ending_with(val: &str, kwargs: Kwargs, _: &State) -> TeraResult<bool> {
    let pat = kwargs.must_get::<&str>("pat")?;
    Ok(val.ends_with(pat))
}

pub(crate) fn is_containing(val: &Value, kwargs: Kwargs, _: &State) -> TeraResult<bool> {
    let pat = kwargs.must_get::<&Value>("pat")?;
    match val {
        Value::String(v, _) => {
            let s = <&str as ArgFromValue>::from_value(&pat)?;
            Ok(v.contains(s))
        }
        Value::Array(v) => Ok(v.contains(&pat)),
        Value::Map(v) => {
            let s = <&str as ArgFromValue>::from_value(&pat)?;
            Ok(v.contains_key(&Key::Str(s)))
        }
        _ => Err(Error::message(format!(
            "Value `{val}` is not a container; cannot check for containment"
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_do_divisible_by() {
        // TODO
    }
}
