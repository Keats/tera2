use std::sync::Arc;

use crate::errors::{Error, TeraResult};
use crate::value::Map;
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

                res.ok_or_else(|| panic!("TODO: {}, {}", value.name(), stringify!($ty)))
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

impl<'k> ArgFromValue<'k> for &str {
    type Output = &'k str;

    fn from_value(value: &'k Value) -> TeraResult<Self::Output> {
        value
            .as_str()
            .ok_or_else(|| panic!("TODO: {}, {}", value.name(), "&str"))
    }
}

impl<'k> ArgFromValue<'k> for &Value {
    type Output = &'k Value;

    fn from_value(value: &'k Value) -> TeraResult<Self::Output> {
        Ok(value)
    }
}

#[derive(Debug, Clone)]
pub struct Kwargs {
    values: Arc<Map>,
}

impl Kwargs {
    pub(crate) fn new(values: Arc<Map>) -> Self {
        Self { values }
    }

    // pub fn set_filter_name("")

    // pub fn get<'k, T>(&'k self, key: &'k str) -> TeraResult<T>
    // where
    //     T: ArgFromValue<'k, Output = T>,
    // {
    //     T::from_value(self.values.get(&Key::Str(key)))
    // }
}

impl Default for Kwargs {
    fn default() -> Self {
        Self {
            values: Default::default(),
        }
    }
}
