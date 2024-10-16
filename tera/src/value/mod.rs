use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt;
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};

mod de;
mod key;
pub(crate) mod number;
mod ser;
mod utils;

use crate::errors::{Error, TeraResult};
use crate::value::number::Number;
use crate::HashMap;
pub use key::Key;

#[cfg(not(feature = "preserve_order"))]
pub type Map = HashMap<Key<'static>, Value>;

#[cfg(feature = "preserve_order")]
pub type Map = indexmap::IndexMap<Key<'static>, Value>;

#[inline]
pub(crate) fn format_map(map: &Map, f: &mut impl std::io::Write) -> std::io::Result<()> {
    let mut key_val: Box<_> = map.iter().collect();
    // Keys are sorted to have deterministic output if preserve_order is not used
    if cfg!(not(feature = "preserve_order")) {
        key_val.sort_by_key(|elem| elem.0);
    }
    f.write_all(b"{")?;
    for (idx, (key, value)) in key_val.iter().enumerate() {
        if idx > 0 {
            f.write_all(b", ")?;
        }
        if let Some(v) = key.as_str() {
            write!(f, "{v:?}")?
        } else {
            key.format(f)?;
        }

        f.write_all(b": ")?;
        match value {
            Value::String(v, _) => write!(f, "{v:?}")?,
            _ => value.format(f)?,
        }
    }
    f.write_all(b"}")
}

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
        let mut out = Vec::new();
        let res = self.format(&mut out);
        if res.is_err() {
            return Err(fmt::Error);
        }
        write!(
            f,
            "{}",
            std::str::from_utf8(&out).expect("valid utf-8 in display")
        )
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

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        if let Some(res) = self.partial_cmp(other) {
            return res;
        }

        // Nonsensical hardcoded ordering, don't compare arrays and integers...
        Ordering::Greater
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Undefined | Value::Null => 0.hash(state),
            Value::Bool(v) => v.hash(state),
            Value::U64(_) | Value::I64(_) | Value::U128(_) | Value::I128(_) | Value::F64(_) => {
                self.as_number().hash(state)
            }
            Value::Bytes(v) => v.hash(state),
            Value::String(v, _) => v.hash(state),
            Value::Array(v) => v.hash(state),
            Value::Map(v) => v.iter().for_each(|(k, v)| {
                k.hash(state);
                v.hash(state);
            }),
        }
    }
}

impl Value {
    pub(crate) fn format(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        match self {
            Value::Null | Value::Undefined => Ok(()),
            Value::Bool(v) => f.write_all(if *v { b"true" } else { b"false" }),
            Value::Bytes(v) => f.write_all(v),
            Value::String(v, _) => f.write_all(v.as_bytes()),
            Value::Array(v) => {
                f.write_all(b"[")?;

                for (idx, elem) in v.iter().enumerate() {
                    if idx > 0 {
                        f.write_all(b", ")?;
                    }

                    match elem {
                        Value::String(v, _) => write!(f, "{v:?}")?,
                        _ => elem.format(f)?,
                    }
                }
                f.write_all(b"]")
            }
            Value::Map(v) => format_map(v, f),
            Value::F64(v) => {
                // We could use ryu to print floats but it doesn't match the output from
                // the std so tests become annoying.
                write!(f, "{v}")
            }
            Value::U64(v) => {
                #[cfg(feature = "no_fmt")]
                {
                    let mut buf = itoa::Buffer::new();
                    f.write_all(buf.format(*v).as_bytes())
                }
                #[cfg(not(feature = "no_fmt"))]
                write!(f, "{v}")
            }
            Value::I64(v) => {
                #[cfg(feature = "no_fmt")]
                {
                    let mut buf = itoa::Buffer::new();
                    f.write_all(buf.format(*v).as_bytes())
                }
                #[cfg(not(feature = "no_fmt"))]
                write!(f, "{v}")
            }
            Value::U128(v) => {
                #[cfg(feature = "no_fmt")]
                {
                    let mut buf = itoa::Buffer::new();
                    f.write_all(buf.format(*v).as_bytes())
                }
                #[cfg(not(feature = "no_fmt"))]
                write!(f, "{v}")
            }
            Value::I128(v) => {
                #[cfg(feature = "no_fmt")]
                {
                    let mut buf = itoa::Buffer::new();
                    f.write_all(buf.format(*v).as_bytes())
                }
                #[cfg(not(feature = "no_fmt"))]
                write!(f, "{v}")
            }
        }
    }

    pub fn from_serializable<T: Serialize + ?Sized>(value: &T) -> Value {
        Serialize::serialize(value, ser::ValueSerializer).unwrap()
    }

    pub fn safe_string(val: &str) -> Value {
        Value::String(Arc::from(val), StringKind::Safe)
    }

    pub(crate) fn as_i128(&self) -> Option<i128> {
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

    pub(crate) fn as_number(&self) -> Option<Number> {
        // TODO: this might be problematic to convert u128 to i128
        // We should probably expand the Number Enum
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

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s, _) => Some(s),
            _ => None,
        }
    }

    #[inline]
    pub fn is_safe(&self) -> bool {
        match self {
            Value::String(_, kind) => *kind == StringKind::Safe,
            Value::Array(_) | Value::Map(_) | Value::Bytes(_) => false,
            _ => true,
        }
    }

    #[inline]
    pub fn mark_safe(self) -> Self {
        match self {
            Value::String(s, _) => Value::String(s, StringKind::Safe),
            _ => self,
        }
    }

    pub fn as_map(&self) -> Option<&Map> {
        match self {
            Value::Map(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_vec(&self) -> Option<&Vec<Value>> {
        match self {
            Value::Array(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_map(self) -> Option<Arc<Map>> {
        match self {
            Value::Map(s) => Some(s),
            _ => None,
        }
    }

    /// Returns the Value at the given path, or Undefined if there's nothing there.
    pub fn get_from_path(&self, path: &str) -> Value {
        if self == &Value::Undefined || self == &Value::Null {
            return self.clone();
        }

        let mut res = self.clone();

        for elem in path.split('.') {
            match elem.parse::<usize>() {
                Ok(idx) => match res {
                    Value::Array(arr) => {
                        if let Some(v) = arr.get(idx) {
                            res = v.clone();
                        } else {
                            return Value::Undefined;
                        }
                    }
                    _ => return Value::Undefined,
                },
                Err(_) => match res {
                    Value::Map(map) => {
                        if let Some(v) = map.get(&Key::Str(elem)) {
                            res = v.clone();
                        } else {
                            return Value::Undefined;
                        }
                    }
                    _ => return Value::Undefined,
                },
            }
        }

        res
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

    pub fn len(&self) -> Option<usize> {
        match self {
            Value::Map(v) => Some(v.len()),
            Value::Array(v) => Some(v.len()),
            Value::Bytes(v) => Some(v.len()),
            Value::String(v, _) => Some(v.chars().count()),
            _ => None,
        }
    }

    pub fn reverse(&self) -> TeraResult<Value> {
        match self {
            Value::Array(v) => {
                let mut rev = (**v).clone();
                rev.reverse();
                Ok(Self::from(rev))
            }
            Value::Bytes(v) => Ok(Self::from(v.iter().rev().copied().collect::<Vec<_>>())),
            Value::String(v, _) => Ok(Self::from(String::from_iter(v.chars().rev()))),
            _ => Err(Error::message(format!(
                "Value of type {} cannot be reversed",
                self.name()
            ))),
        }
    }

    // TODO: do we need that?
    // pub(crate) fn is_empty(&self) -> bool {
    //     match self {
    //         Value::Array(v) => v.is_empty(),
    //         Value::Bytes(v) => v.is_empty(),
    //         Value::String(v, _) => v.is_empty(),
    //         Value::Map(v) => v.is_empty(),
    //         _ => false,
    //     }
    // }

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
                    Some(idx) => {
                        let correct_idx = if idx < 0 {
                            arr.len() as i128 + idx
                        } else {
                            idx
                        } as usize;
                        Ok(arr.get(correct_idx).cloned().unwrap_or(Value::Undefined))
                    },
                    None =>  Err(Error::message(
                        format!("Array indices can only be integers, not `{}`.", item.name()),
                    ))
                }
            }
            _ => Ok(Value::Undefined),
        }
    }

    pub(crate) fn slice(
        &self,
        start: Option<i128>,
        end: Option<i128>,
        step: Option<i128>,
    ) -> TeraResult<Value> {
        let step = step.unwrap_or(1);
        let reverse = step == -1;

        let get_actual_idx = |size: i128, param: Option<i128>| -> i128 {
            if let Some(p) = param {
                if p < 0 {
                    size + p
                } else {
                    p
                }
            } else {
                size
            }
        };

        match self {
            Value::Array(arr) => {
                let mut input = Vec::with_capacity(arr.len());
                let mut out = Vec::with_capacity(arr.len());
                let start = get_actual_idx(arr.len() as i128, start);
                let end = get_actual_idx(arr.len() as i128, end);

                for item in arr.iter() {
                    input.push(item.clone());
                }

                if reverse {
                    input.reverse();
                }

                for (idx, item) in input.into_iter().enumerate() {
                    if (idx as i128) >= start && (idx as i128) < end {
                        out.push(item);
                    }
                }

                Ok(out.into())
            }
            Value::String(s, kind) => {
                let mut out = Vec::with_capacity(s.len());

                #[cfg(feature = "unicode")]
                let mut input: Vec<&str> = unic_segment::Graphemes::new(&*s).collect();
                #[cfg(not(feature = "unicode"))]
                let mut input: Vec<char> = s.chars().collect();

                let start = get_actual_idx(input.len() as i128, start);
                let end = get_actual_idx(input.len() as i128, end);

                if reverse {
                    input.reverse();
                }

                for (idx, item) in input.iter().enumerate() {
                    if (idx as i128) >= start && (idx as i128) < end {
                        out.push(*item);
                    }
                }

                #[cfg(feature = "unicode")]
                {
                    Ok(Value::String(Arc::from(out.join("")), *kind))
                }

                #[cfg(not(feature = "unicode"))]
                {
                    Ok(Value::String(
                        Arc::from(String::from_iter(out).as_str()),
                        *kind,
                    ))
                }
            }
            _ => Err(Error::message(format!(
                "Slicing can only be used on arrays or strings, not on `{}`.",
                self.name()
            ))),
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

impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Value::U64(value as u64)
    }
}

impl From<i8> for Value {
    fn from(value: i8) -> Self {
        Value::I64(value as i64)
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

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        Value::U64(value as u64)
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

impl From<isize> for Value {
    fn from(value: isize) -> Self {
        Value::I64(value as i64)
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
            Key::String(s) => Value::String(s, StringKind::Normal),
            Key::Str(s) => Value::String(Arc::from(s), StringKind::Normal),
        }
    }
}

impl From<&[Value]> for Value {
    fn from(value: &[Value]) -> Self {
        Value::Array(Arc::new(value.to_vec()))
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
impl<K: Into<Key<'static>>, T: Into<Value>> From<indexmap::IndexMap<K, T>> for Value {
    fn from(input: indexmap::IndexMap<K, T>) -> Self {
        let mut map = Map::with_capacity(input.len());
        for (key, value) in input {
            map.insert(key.into(), value.into());
        }
        Value::Map(Arc::new(map))
    }
}

// TODO: move somewhere else
pub trait FunctionResult {
    fn into_result(self) -> TeraResult<Value>;
}

impl<I: Into<Value>> FunctionResult for TeraResult<I> {
    fn into_result(self) -> TeraResult<Value> {
        self.map(Into::into)
    }
}

impl<I: Into<Value>> FunctionResult for I {
    fn into_result(self) -> TeraResult<Value> {
        Ok(self.into())
    }
}
