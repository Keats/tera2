use serde::Serialize;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;
use std::sync::Arc;

mod key;
mod ser;
mod utils;

use key::Key;

#[derive(Debug, Clone)]
pub enum Value {
    // TODO: differentiate Undefined and Null?
    Null,
    Bool(bool),
    U64(u64),
    I64(i64),
    F64(f64),
    U128(u128),
    I128(i128),
    // TODO: do we need char?
    Char(char),
    Array(Arc<Vec<Value>>),
    Bytes(Arc<Vec<u8>>),
    String(Arc<String>),
    // TODO: order preserving feature via indexmap
    // TODO: string interning?
    // TODO: change the hash alg feature
    Map(Arc<HashMap<Key, Value>>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Null => Ok(()),
            Value::Bool(v) => write!(f, "{v}"),
            Value::U64(v) => write!(f, "{v}"),
            Value::I64(v) => write!(f, "{v}"),
            Value::F64(v) => write!(f, "{v}"),
            Value::U128(v) => write!(f, "{v}"),
            Value::I128(v) => write!(f, "{v}"),
            Value::Char(v) => write!(f, "{v}"),
            Value::Array(v) => {
                write!(f, "[")?;
                for (i, elem) in v.iter().enumerate() {
                    if i > 0 && i != v.len() {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                write!(f, "]")
            }
            Value::Bytes(v) => write!(f, "{}", String::from_utf8_lossy(v)),
            Value::String(v) => write!(f, "{v}"),
            Value::Map(v) => {
                write!(f, "{{")?;
                for (i, (key, value)) in v.iter().enumerate() {
                    if i > 0 && i != v.len() {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {value}")?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // First the easy ones
            (Value::Null, Value::Null) => true,
            (Value::Bool(v), Value::Bool(v2)) => v == v2,
            (Value::Char(v), Value::Char(v2)) => v == v2,
            (Value::Array(v), Value::Array(v2)) => v == v2,
            (Value::Bytes(v), Value::Bytes(v2)) => v == v2,
            (Value::String(v), Value::String(v2)) => v == v2,
            (Value::Map(v), Value::Map(v2)) => v == v2,
            // Then the numbers
            // First if there's a float we need to convert to float
            (Value::F64(v), _) => Some(*v) == other.as_f64(),
            (_, Value::F64(v)) => Some(*v) == self.as_f64(),
            // Then integers
            (Value::U64(_), _)
            | (Value::I64(_), _)
            | (Value::U128(_), _)
            | (Value::I128(_), _)
            | (_, Value::U64(_))
            | (_, Value::I64(_))
            | (_, Value::U128(_))
            | (_, Value::I128(_)) => self.as_i128() == other.as_i128(),
            (_, _) => false,
        }
    }
}

impl Eq for Value {}

impl Value {
    pub fn from_serializable<T: Serialize>(value: &T) -> Value {
        Serialize::serialize(value, ser::ValueSerializer).unwrap()
    }

    fn as_i128(&self) -> Option<i128> {
        match self {
            Value::U64(v) => Some(*v as i128),
            Value::I64(v) => Some(*v as i128),
            // TODO: in theory this cannot necessarily fit in i128
            Value::U128(v) => Some(*v as i128),
            Value::I128(v) => Some(*v as i128),
            _ => None,
        }
    }

    fn as_f64(&self) -> Option<f64> {
        // TODO: make sure we only cast to f64 if the value can fit in it
        match self {
            Value::U64(v) => Some(*v as f64),
            Value::I64(v) => Some(*v as f64),
            Value::F64(v) => Some(*v),
            Value::U128(v) => Some(*v as f64),
            Value::I128(v) => Some(*v as f64),
            _ => None,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Bool(v) => *v,
            Value::U64(v) => *v != 0,
            Value::I64(v) => *v != 0,
            Value::F64(v) => *v != 0.0,
            Value::U128(v) => *v != 0,
            Value::I128(v) => *v != 0,
            Value::Char(v) => v != &'\x00',
            Value::Array(v) => !v.is_empty(),
            Value::Bytes(v) => !v.is_empty(),
            Value::String(v) => !v.is_empty(),
            Value::Map(v) => !v.is_empty(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        let val = Value::from_serializable(&vec![1, 2, 3]);
        println!("{val}");
        assert!(false);
    }
}
