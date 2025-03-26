use serde::Deserialize;
use std::borrow::Cow;
use std::sync::Arc;

use crate::errors::{Error, TeraResult};
use crate::value::number::Number;
use crate::value::{Key, Map};
use crate::Value;

pub trait ArgFromValue<'k> {
    type Output;

    fn from_value(value: &'k Value) -> TeraResult<Self::Output>;
}

// TODO: Need to add 2 error types: one for unsupported ops and one for missing args: same one maybe?
// TODO: add impl for all primitives/things that can be in value and maybe for Value as well?
// TODO: write types for filters/fns/tests using Kwargs and make sure it's ergonomic to handle errors

macro_rules! impl_for_literal {
    ($ty:ident, {
        $($pat:pat $(if $if_expr:expr)? => $expr:expr,)*
    }) => {
        impl TryFrom<Value> for $ty {
            type Error = Error;

            fn try_from(value: Value) -> Result<Self, Self::Error> {
                let res = match value {
                    $($pat $(if $if_expr)? => TryFrom::try_from($expr).ok(),)*
                    _ => None
                };

                res.ok_or_else(|| Error::invalid_arg_type(stringify!($ty), value.name()))
            }
        }

        impl<'k> ArgFromValue<'k> for $ty {
            type Output = Self;
            fn from_value(value: &Value) -> Result<Self, Error> {
                TryFrom::try_from(value.clone())
            }
        }
    }
}
macro_rules! impl_for_int {
    ($ty:ident) => {
        impl_for_literal!($ty, {
            Value::I64(v) => v,
            Value::I128(v) => v,
            Value::U64(v) => v,
            Value::U128(v) => v,
            Value::F64(v) if (v == v as i64 as f64) => v as i64,
        });
    }
}
impl_for_int!(u8);
impl_for_int!(u16);
impl_for_int!(u32);
impl_for_int!(u64);
impl_for_int!(u128);
impl_for_int!(usize);
impl_for_int!(i8);
impl_for_int!(i16);
impl_for_int!(i32);
impl_for_int!(i64);
impl_for_int!(i128);
impl_for_int!(isize);

impl_for_literal!(bool, {
    Value::Bool(b) => b,
});

// TODO: test when value doesn't fit in f32
impl_for_literal!(f32, {
    Value::I64(b) => b as f32,
    Value::I128(b) => b as f32,
    Value::U64(b) => b as f32,
    Value::U128(b) => b as f32,
    Value::F64(b) => b as f32,
});
impl_for_literal!(f64, {
    Value::I64(b) => b as f64,
    Value::I128(b) => b as f64,
    Value::U64(b) => b as f64,
    Value::U128(b) => b as f64,
    Value::F64(b) => b,
});

impl<'k> ArgFromValue<'k> for String {
    type Output = String;

    fn from_value(value: &'k Value) -> TeraResult<Self::Output> {
        Ok(format!("{value}"))
    }
}

impl<'k> ArgFromValue<'k> for &str {
    type Output = &'k str;

    fn from_value(value: &'k Value) -> TeraResult<Self::Output> {
        value
            .as_str()
            .ok_or_else(|| Error::invalid_arg_type("&str", value.name()))
    }
}

impl<'k> ArgFromValue<'k> for Cow<'_, str> {
    type Output = Cow<'k, str>;

    fn from_value(value: &'k Value) -> TeraResult<Self::Output> {
        match value {
            Value::String(s, _) => Ok(Cow::Borrowed(s)),
            _ => Ok(Cow::Owned(format!("{value}"))),
        }
    }
}

impl<'k> ArgFromValue<'k> for &Value {
    type Output = &'k Value;

    fn from_value(value: &'k Value) -> TeraResult<Self::Output> {
        Ok(value)
    }
}

impl<'k> ArgFromValue<'k> for Value {
    type Output = Value;

    fn from_value(value: &'k Value) -> TeraResult<Self::Output> {
        Ok(value.clone())
    }
}

impl<'k> ArgFromValue<'k> for Number {
    type Output = Number;

    fn from_value(value: &'k Value) -> TeraResult<Self::Output> {
        value
            .as_number()
            .ok_or_else(|| Error::invalid_arg_type("Number", value.name()))
    }
}

impl<'k> ArgFromValue<'k> for Map {
    type Output = Map;

    fn from_value(value: &'k Value) -> TeraResult<Self::Output> {
        value
            .as_map()
            .cloned()
            .ok_or_else(|| Error::invalid_arg_type("Map", value.name()))
    }
}

impl<'k, T: ArgFromValue<'k, Output = T>> ArgFromValue<'k> for Vec<T> {
    type Output = Vec<T>;

    fn from_value(value: &'k Value) -> TeraResult<Self::Output> {
        match value {
            Value::Array(arr) => {
                let mut res = Vec::with_capacity(arr.len());
                for v in arr.iter() {
                    res.push(T::from_value(v)?);
                }
                Ok(res)
            }
            _ => Err(Error::invalid_arg_type("Vec<Value>", value.name())),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Kwargs {
    values: Arc<Map>,
}

impl Kwargs {
    pub fn new(map: Arc<Map>) -> Self {
        Self { values: map }
    }

    pub fn deserialize<'a, T: Deserialize<'a>>(&'a self) -> TeraResult<T> {
        T::deserialize(&Value::Map(self.values.clone()))
            .map_err(|_| Error::message("Failed to deserialize"))
    }

    pub fn get<'k, T>(&'k self, key: &'k str) -> TeraResult<Option<T>>
    where
        T: ArgFromValue<'k, Output = T>,
    {
        match self.values.get(&Key::Str(key)) {
            Some(v) => T::from_value(v).map(|v| Some(v)),
            None => Ok(None),
        }
    }

    pub fn must_get<'k, T>(&'k self, key: &'k str) -> TeraResult<T>
    where
        T: ArgFromValue<'k, Output = T>,
    {
        if let Some(v) = self.get(key)? {
            Ok(v)
        } else {
            Err(Error::missing_arg(key))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_get_kwarg_with_type() {
        #[derive(Debug, Deserialize)]
        struct Data {
            hello: String,
            num: f64,
        }

        let mut map = Map::new();
        map.insert("hello".into(), Value::from("world"));
        map.insert("num".into(), Value::from(1.1));
        let kwargs = Kwargs::new(Arc::new(map));
        assert_eq!(kwargs.get("hello").unwrap(), Some("world"));
        assert_eq!(kwargs.get("num").unwrap(), Some(1.1));
        assert_eq!(kwargs.get::<i64>("unknown").unwrap(), None);

        let data: Data = kwargs.deserialize().unwrap();
        assert_eq!(data.num, 1.1);
        assert_eq!(data.hello, "world");
    }
}
