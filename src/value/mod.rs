use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt;
use std::fmt::Formatter;
use std::sync::Arc;

#[cfg(feature = "preserve_order")]
use indexmap::IndexMap;
use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};

mod key;
pub(crate) mod number;
mod ser;
mod utils;

use crate::errors::{Error, TeraResult};
use crate::value::number::Number;
use crate::{escape_html, HashMap};
pub use key::Key;

#[cfg(not(feature = "preserve_order"))]
pub type Map = HashMap<Key<'static>, Value>;

#[cfg(feature = "preserve_order")]
pub type Map = IndexMap<Key<'static>, Value>;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum StringKind {
    Normal,
    Safe,
}

#[derive(Debug, Clone)]
pub enum Value {
    Undefined,
    Null,
    Bool(bool),
    U64(u64),
    I64(i64),
    F64(f64),
    U128(u128),
    I128(i128),
    Array(Arc<Vec<Value>>),
    // TODO: do we want bytes? Do we assume they are utf8?
    Bytes(Arc<Vec<u8>>),
    // TODO: string interning?
    String(Arc<str>, StringKind),
    Map(Arc<Map>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Undefined => Ok(()),
            Value::Null => Ok(()),
            Value::Bool(v) => write!(f, "{v}"),
            Value::U64(v) => write!(f, "{v}"),
            Value::I64(v) => write!(f, "{v}"),
            Value::F64(v) => write!(f, "{v}"),
            Value::U128(v) => write!(f, "{v}"),
            Value::I128(v) => write!(f, "{v}"),
            Value::Array(v) => {
                let mut it = v.iter();
                write!(f, "[")?;
                // First value
                if let Some(elem) = it.next() {
                    write!(f, "{elem}")?;
                }
                // Every other value
                for elem in it {
                    write!(f, ", {elem}")?;
                }
                write!(f, "]")
            }
            Value::Bytes(v) => write!(f, "{}", String::from_utf8_lossy(v)),
            Value::String(v, kind) => {
                if matches!(kind, StringKind::Safe) {
                    f.write_str(v)
                } else {
                    f.write_str(&escape_html(v))
                }
            }
            Value::Map(v) => {
                let mut key_val: Box<_> = v.iter().collect();
                // Keys are sorted to have deterministic output
                // TODO: Consider using sort_unstable_by_key
                key_val.sort_by_key(|elem| elem.0);
                let mut it = key_val.iter();
                write!(f, "{{")?;
                // First value
                if let Some((key, value)) = it.next() {
                    write!(f, "{key}: {value}")?;
                }
                // Every other value
                for (key, value) in it {
                    write!(f, ", {key}: {value}")?;
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
            (Value::Undefined, Value::Undefined) => true,
            (Value::Null, Value::Null) => true,
            (Value::Bool(v), Value::Bool(v2)) => v == v2,
            (Value::Array(v), Value::Array(v2)) => v == v2,
            (Value::Bytes(v), Value::Bytes(v2)) => v == v2,
            // TODO: should string kind be used for partialeq? They might be equal now but
            // different later if one needs to be escape
            (Value::String(v, _), Value::String(v2, _)) => v == v2,
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
            (Value::Undefined, Value::Undefined) => Some(Ordering::Equal),
            (Value::Null, Value::Null) => Some(Ordering::Equal),
            (Value::Bool(v), Value::Bool(v2)) => v.partial_cmp(v2),
            (Value::Array(v), Value::Array(v2)) => v.partial_cmp(v2),
            (Value::Bytes(v), Value::Bytes(v2)) => v.partial_cmp(v2),
            (Value::String(v, _), Value::String(v2, _)) => v.partial_cmp(v2),
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
    pub fn from_serializable<T: Serialize + ?Sized>(value: &T) -> Value {
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

    pub(crate) fn is_number(&self) -> bool {
        matches!(
            self,
            Value::U64(..) | Value::I64(..) | Value::F64(..) | Value::U128(..) | Value::I128(..)
        )
    }

    fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s, _) => Some(s),
            _ => None,
        }
    }

    pub fn as_map(&self) -> Option<&Map> {
        match self {
            Value::Map(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_map(self) -> Option<Arc<Map>> {
        match self {
            Value::Map(s) => Some(s),
            _ => None,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Undefined => false,
            Value::Null => false,
            Value::Bool(v) => *v,
            Value::U64(v) => *v != 0,
            Value::I64(v) => *v != 0,
            Value::F64(v) => *v != 0.0,
            Value::U128(v) => *v != 0,
            Value::I128(v) => *v != 0,
            Value::Array(v) => !v.is_empty(),
            Value::Bytes(v) => !v.is_empty(),
            Value::String(v, _) => !v.is_empty(),
            Value::Map(v) => !v.is_empty(),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        match self {
            Value::Array(v) => v.is_empty(),
            Value::Bytes(v) => v.is_empty(),
            Value::String(v, _) => v.is_empty(),
            Value::Map(v) => v.is_empty(),
            _ => false,
        }
    }

    pub(crate) fn can_be_iterated_on(&self) -> bool {
        matches!(
            self,
            Value::Map(..) | Value::Array(..) | Value::Bytes(..) | Value::String(..)
        )
    }

    pub(crate) fn as_key(&self) -> TeraResult<Key<'static>> {
        let key = match self {
            Value::Bool(v) => Key::Bool(*v),
            Value::U64(v) => Key::U64(*v),
            Value::I64(v) => Key::I64(*v),
            Value::String(v, _) => Key::String(v.clone()),
            _ => return Err(Error::message("Not a valid key type".to_string())),
        };
        Ok(key)
    }

    pub(crate) fn contains(&self, needle: &Value) -> TeraResult<bool> {
        match self {
            Value::Array(arr) => Ok(arr.contains(needle)),
            Value::String(s, _) => {
                if let Some(needle_str) = needle.as_str() {
                    Ok(s.contains(needle_str))
                } else {
                    Ok(false)
                }
            }
            // If they needle cannot index a map, then it can contain it
            Value::Map(m) => match &needle.as_key() {
                Ok(k) => Ok(m.contains_key(k)),
                Err(_) => Ok(false),
            },
            _ => Err(Error::message(
                format!("`in` cannot be used on a container of type `{}`. It can only be used on arrays, strings and map/structs", self.name()),
            )),
        }
    }

    /// When doing hello.name, name is the attr
    pub(crate) fn get_attr(&self, attr: &str) -> Value {
        match self {
            Value::Map(m) => m.get(&Key::Str(attr)).cloned().unwrap_or(Value::Undefined),
            _ => Value::Undefined,
        }
    }

    /// When doing hello[0], hello[name] etc, item is the value in the brackets
    pub(crate) fn get_item(&self, item: Value) -> TeraResult<Value> {
        match self {
            Value::Map(m) => {
                match item.as_key() {
                    Ok(k) => Ok(m.get(&k).cloned().unwrap_or(Value::Undefined)),
                    Err(_) => Err(Error::message(
                        format!("`{}` cannot be a key of a map/struct: only be integers, bool or strings are allowed", item.name()),
                    ))
                }
            }
            Value::Array(arr) => {
                match item.as_i128() {
                    Some(idx) => Ok(arr.get(idx as usize).cloned().unwrap_or(Value::Undefined)),
                    None =>  Err(Error::message(
                        format!("Array indices can only be integers, not `{}`.", item.name()),
                    ))
                }
            }
            _ => Ok(Value::Undefined),
        }
    }

    /// Returns a string name for the current enum member.
    /// Used in error messages
    pub(crate) fn name(&self) -> &'static str {
        match self {
            Value::Undefined => "undefined",
            Value::Null => "null",
            Value::Bool(_) => "bool",
            Value::U64(_) => "u64",
            Value::I64(_) => "i64",
            Value::F64(_) => "f64",
            Value::U128(_) => "u128",
            Value::I128(_) => "i128",
            Value::Array(_) => "array",
            Value::Bytes(_) => "bytes",
            Value::String(_, _) => "string",
            Value::Map(_) => "map/struct",
        }
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Value::Null | Value::Undefined => serializer.serialize_unit(),
            Value::Bool(b) => serializer.serialize_bool(*b),
            Value::U64(u) => serializer.serialize_u64(*u),
            Value::I64(i) => serializer.serialize_i64(*i),
            Value::F64(f) => serializer.serialize_f64(*f),
            Value::U128(u) => serializer.serialize_u128(*u),
            Value::I128(i) => serializer.serialize_i128(*i),
            Value::Bytes(b) => serializer.serialize_bytes(b),
            Value::String(s, _) => serializer.serialize_str(s),
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
        Value::String(Arc::from(value), StringKind::Normal)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(Arc::from(value), StringKind::Normal)
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

impl From<Key<'static>> for Value {
    fn from(value: Key<'static>) -> Self {
        match value {
            Key::Bool(b) => Value::Bool(b),
            Key::U64(u) => Value::U64(u),
            Key::I64(i) => Value::I64(i),
            Key::String(s) => Value::String(Arc::from(s), StringKind::Normal),
            Key::Str(s) => Value::String(Arc::from(s), StringKind::Normal),
        }
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

impl<K: Into<Key<'static>>, T: Into<Value>> From<HashMap<K, T>> for Value {
    fn from(input: HashMap<K, T>) -> Self {
        let mut map = Map::with_capacity(input.len());
        for (key, value) in input {
            map.insert(key.into(), value.into());
        }
        Value::Map(Arc::new(map))
    }
}

impl<K: Into<Key<'static>>, T: Into<Value>> From<BTreeMap<K, T>> for Value {
    fn from(input: BTreeMap<K, T>) -> Self {
        let mut map = Map::with_capacity(input.len());
        for (key, value) in input {
            map.insert(key.into(), value.into());
        }
        Value::Map(Arc::new(map))
    }
}

#[cfg(feature = "preserve_order")]
impl<K: Into<Key<'static>>, T: Into<Value>> From<IndexMap<K, T>> for Value {
    fn from(input: IndexMap<K, T>) -> Self {
        let mut map = Map::with_capacity(input.len());
        for (key, value) in input {
            map.insert(key.into(), value.into());
        }
        Value::Map(Arc::new(map))
    }
}
