use serde::{Serialize, Serializer};
use std::borrow::Cow;
use std::cmp::Ordering;
#[cfg(not(feature = "preserve_order"))]
use std::collections::HashMap;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::fmt::Formatter;
use std::sync::Arc;

#[cfg(feature = "preserve_order")]
use indexmap::IndexMap;
use serde::ser::{SerializeMap, SerializeSeq};

mod key;
pub(crate) mod number;
mod ser;
mod utils;

use crate::errors::{Error, TeraResult};
use crate::value::number::Number;
pub use key::Key;

#[cfg(not(feature = "preserve_order"))]
pub(crate) type Map = HashMap<Key, Value>;

#[cfg(feature = "preserve_order")]
pub(crate) type Map = IndexMap<Key, Value>;

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
    // TODO: string interning?
    // TODO: change the hash alg feature? rustc-hash/ahash after testing
    Map(Arc<Map>),
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

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            // First the easy ones
            (Value::Null, Value::Null) => Some(Ordering::Equal),
            (Value::Bool(v), Value::Bool(v2)) => v.partial_cmp(v2),
            (Value::Char(v), Value::Char(v2)) => v.partial_cmp(v2),
            (Value::Array(v), Value::Array(v2)) => v.partial_cmp(v2),
            (Value::Bytes(v), Value::Bytes(v2)) => v.partial_cmp(v2),
            (Value::String(v), Value::String(v2)) => v.partial_cmp(v2),
            // Then the numbers
            // First if there's a float we need to convert to float
            (Value::F64(v), _) => v.partial_cmp(&other.as_f64()?),
            (_, Value::F64(v)) => v.partial_cmp(&self.as_f64()?),
            // Then integers
            (Value::U64(_), _)
            | (Value::I64(_), _)
            | (Value::U128(_), _)
            | (Value::I128(_), _)
            | (_, Value::U64(_))
            | (_, Value::I64(_))
            | (_, Value::U128(_))
            | (_, Value::I128(_)) => self.as_i128()?.partial_cmp(&other.as_i128()?),
            (_, _) => None,
        }
    }
}

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
            Value::I128(v) => Some(*v),
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

    fn as_number(&self) -> Option<Number> {
        match self {
            Value::U64(v) => Some(Number::Integer(*v as i128)),
            Value::I64(v) => Some(Number::Integer(*v as i128)),
            Value::F64(v) => Some(Number::Float(*v)),
            Value::U128(v) => Some(Number::Integer(*v as i128)),
            Value::I128(v) => Some(Number::Integer(*v)),
            _ => None,
        }
    }

    fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
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

    pub(crate) fn as_key(&self) -> TeraResult<Key> {
        let key = match self {
            Value::Bool(v) => Key::Bool(*v),
            Value::U64(v) => Key::U64(*v),
            Value::I64(v) => Key::I64(*v),
            Value::Char(v) => Key::Char(*v),
            // TODO: not ideal to clone the actual string.
            Value::String(v) => Key::String(Cow::Owned(v.as_str().to_string())),
            _ => return Err(Error::message(format!("Not a valid key type"))),
        };
        Ok(key)
    }

    pub(crate) fn contains(&self, needle: &Value) -> TeraResult<bool> {
        match self {
            Value::Array(arr) => Ok(arr.contains(needle)),
            Value::String(s) => {
                if let Some(needle_str) = needle.as_str() {
                    Ok(s.contains(needle_str))
                } else {
                    Ok(false)
                }
            }
            Value::Map(m) => Ok(m.contains_key(&needle.as_key()?)),
            _ => Err(Error::message(format!(
                "Cannot check containment on this kind of value"
            ))),
        }
    }

    /// When doing hello.name, name is the attr
    pub(crate) fn get_attr(&self, attr: &str) -> Option<Value> {
        match self {
            Value::Map(m) => m.get(&attr.into()).cloned(),
            _ => None
        }
    }

    /// When doing hello[0], hello[name] etc, item is the value in the brackets
    pub(crate) fn get_item(&self, item: Value) -> Option<Value> {
        match self {
            Value::Map(m) => {
                let key = item.as_key().expect("TODO: error handling");
                m.get(&key).cloned()
            },
            Value::Array(arr) => {
                let idx = item.as_i128().expect("TODO: error handling");
                arr.get(idx as usize).cloned()
            },
            _ => None
        }
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        match self {
            Value::Null => serializer.serialize_unit(),
            Value::Bool(b) => serializer.serialize_bool(*b),
            Value::U64(u) => serializer.serialize_u64(*u),
            Value::I64(i) => serializer.serialize_i64(*i),
            Value::F64(f) => serializer.serialize_f64(*f),
            Value::U128(u) => serializer.serialize_u128(*u),
            Value::I128(i) => serializer.serialize_i128(*i),
            Value::Char(c) => serializer.serialize_char(*c),
            Value::Bytes(b) => serializer.serialize_bytes(b),
            Value::String(s) => serializer.serialize_str(s),
            Value::Array(arr) => {
                let mut seq = serializer.serialize_seq(Some(arr.len()))?;
                for val in arr.iter() {
                    seq.serialize_element(val)?;
                }
                seq.end()
            }
            Value::Map(map) => {
                let mut m = serializer.serialize_map(Some(map.len()))?;
                for (key, val) in map.iter() {
                    m.serialize_entry(key, val)?;
                }
                m.end()
            }
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::String(Arc::new(value.to_owned()))
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(Arc::new(value))
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::U64(value as u64)
    }
}

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Value::U64(value)
    }
}

impl From<u128> for Value {
    fn from(value: u128) -> Self {
        Value::U128(value)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::I64(value as i64)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::I64(value)
    }
}

impl From<i128> for Value {
    fn from(value: i128) -> Self {
        Value::I128(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::F64(value)
    }
}


impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(value: Vec<T>) -> Self {
        Value::Array(Arc::new(value.into_iter().map(|v| v.into()).collect()))
    }
}

impl<T: Into<Value>> From<BTreeSet<T>> for Value {
    fn from(value: BTreeSet<T>) -> Self {
        Value::Array(Arc::new(value.into_iter().map(|v| v.into()).collect()))
    }
}

impl<K: Into<Key>, T: Into<Value>> From<HashMap<K, T>> for Value {
    fn from(input: HashMap<K, T>) -> Self {
        let mut map = Map::with_capacity(input.len());
        for (key, value) in input {
            map.insert(key.into(), value.into());
        }
        Value::Map(Arc::new(map))
    }
}

impl<K: Into<Key>, T: Into<Value>> From<BTreeMap<K, T>> for Value {
    fn from(input: BTreeMap<K, T>) -> Self {
        let mut map = Map::with_capacity(input.len());
        for (key, value) in input {
            map.insert(key.into(), value.into());
        }
        Value::Map(Arc::new(map))
    }
}

#[cfg(feature = "preserve_order")]
impl<K: Into<Key>, T: Into<Value>> From<IndexMap<K, T>> for Value {
    fn from(input: IndexMap<K, T>) -> Self {
        let mut map = Map::with_capacity(input.len());
        for (key, value) in input {
            map.insert(key.into(), value.into());
        }
        Value::Map(Arc::new(map))
    }
}