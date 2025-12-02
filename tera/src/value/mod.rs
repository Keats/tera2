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
        match &value.inner {
            ValueInner::String(smart_str) => write!(f, "{:?}", smart_str.as_str())?,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Undefined,
    Null,
    Bool,
    U64,
    I64,
    U128,
    I128,
    F64,
    String,
    Array,
    Map,
    Bytes,
}

/// Smart string with embedded StringKind for memory efficiency.
/// Inline storage for strings ≤21 chars, Arc for longer strings.
#[derive(Clone)]
pub(crate) enum SmartString {
    Small {
        len: u8,
        kind: StringKind,
        data: [u8; 21],
    },
    Large(Arc<str>, StringKind),
}

impl SmartString {
    fn new(s: &str, kind: StringKind) -> Self {
        if s.len() <= 21 {
            let mut data = [0; 21];
            data[..s.len()].copy_from_slice(s.as_bytes());
            Self::Small {
                len: s.len() as u8,
                kind,
                data,
            }
        } else {
            Self::Large(Arc::from(s), kind)
        }
    }

    pub(crate) fn as_str(&self) -> &str {
        match self {
            Self::Small { len, data, .. } => {
                // SAFETY: We know this is valid UTF-8 since we constructed it from a &str
                unsafe { std::str::from_utf8_unchecked(&data[..*len as usize]) }
            }
            Self::Large(s, _) => s,
        }
    }

    pub(crate) fn len(&self) -> usize {
        match self {
            Self::Small { len, .. } => *len as usize,
            Self::Large(s, _) => s.len(),
        }
    }

    pub(crate) fn kind(&self) -> StringKind {
        match self {
            Self::Small { kind, .. } => *kind,
            Self::Large(_, kind) => *kind,
        }
    }

    pub(crate) fn mark_safe(self) -> Self {
        match self {
            Self::Small { len, data, .. } => Self::Small {
                len,
                kind: StringKind::Safe,
                data,
            },
            Self::Large(s, _) => Self::Large(s, StringKind::Safe),
        }
    }

    /// Get string content as Arc<str>, cloning only for small strings
    pub(crate) fn into_arc_str(self) -> Arc<str> {
        match self {
            Self::Small { .. } => Arc::from(self.as_str()),
            Self::Large(arc, _) => arc,
        }
    }
}

impl fmt::Display for SmartString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Debug for SmartString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self)
    }
}

// Internal implementation - can optimize freely
#[derive(Debug, Clone)]
pub(crate) enum ValueInner {
    Undefined,
    Null,
    Bool(bool),
    U64(u64),
    I64(i64),
    F64(f64),
    // Box large integers since they are not used very often
    U128(Box<u128>),
    I128(Box<i128>),
    // SmartString includes whether a string is safe or not
    String(SmartString),
    Array(Arc<Vec<Value>>),
    Map(Arc<Map>),
    Bytes(Arc<Vec<u8>>),
}

#[derive(Clone)]
pub struct Value {
    pub(crate) inner: ValueInner,
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

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.inner)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (&self.inner, &other.inner) {
            // First the easy ones
            (ValueInner::Undefined, ValueInner::Undefined) => true,
            (ValueInner::Null, ValueInner::Null) => true,
            (ValueInner::Bool(v), ValueInner::Bool(v2)) => v == v2,
            (ValueInner::Array(v), ValueInner::Array(v2)) => v == v2,
            (ValueInner::Bytes(v), ValueInner::Bytes(v2)) => v == v2,
            // TODO: should string kind be used for partialeq? They might be equal now but
            // different later if one needs to be escape
            (ValueInner::String(v), ValueInner::String(v2)) => v.as_str() == v2.as_str(),
            (ValueInner::Map(v), ValueInner::Map(v2)) => v == v2,
            // Then the numbers
            // First if there's a float we need to convert to float
            (ValueInner::F64(v), _) => Some(*v) == other.as_f64(),
            (_, ValueInner::F64(v)) => Some(*v) == self.as_f64(),
            // Then integers
            (ValueInner::U64(_), _)
            | (ValueInner::I64(_), _)
            | (ValueInner::U128(_), _)
            | (ValueInner::I128(_), _)
            | (_, ValueInner::U64(_))
            | (_, ValueInner::I64(_))
            | (_, ValueInner::U128(_))
            | (_, ValueInner::I128(_)) => self.as_i128() == other.as_i128(),
            (_, _) => false,
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (&self.inner, &other.inner) {
            // First the easy ones
            (ValueInner::Undefined, ValueInner::Undefined) => Some(Ordering::Equal),
            (ValueInner::Null, ValueInner::Null) => Some(Ordering::Equal),
            (ValueInner::Bool(v), ValueInner::Bool(v2)) => v.partial_cmp(v2),
            (ValueInner::Array(v), ValueInner::Array(v2)) => v.partial_cmp(v2),
            (ValueInner::Bytes(v), ValueInner::Bytes(v2)) => v.partial_cmp(v2),
            (ValueInner::String(v), ValueInner::String(v2)) => v.as_str().partial_cmp(v2.as_str()),
            // Then the numbers
            // First if there's a float we need to convert to float
            (ValueInner::F64(v), _) => v.partial_cmp(&other.as_f64()?),
            (_, ValueInner::F64(v)) => v.partial_cmp(&self.as_f64()?),

            // Then integers
            (ValueInner::U64(_), _)
            | (ValueInner::I64(_), _)
            | (ValueInner::U128(_), _)
            | (ValueInner::I128(_), _)
            | (_, ValueInner::U64(_))
            | (_, ValueInner::I64(_))
            | (_, ValueInner::U128(_))
            | (_, ValueInner::I128(_)) => self.as_i128()?.partial_cmp(&other.as_i128()?),
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
        match &self.inner {
            ValueInner::Undefined | ValueInner::Null => 0.hash(state),
            ValueInner::Bool(v) => v.hash(state),
            ValueInner::U64(_)
            | ValueInner::I64(_)
            | ValueInner::U128(_)
            | ValueInner::I128(_)
            | ValueInner::F64(_) => self.as_number().hash(state),
            ValueInner::Bytes(v) => v.hash(state),
            ValueInner::String(v) => v.as_str().hash(state),
            ValueInner::Array(v) => v.hash(state),
            ValueInner::Map(v) => v.iter().for_each(|(k, v)| {
                k.hash(state);
                v.hash(state);
            }),
        }
    }
}

impl Value {
    pub fn null() -> Self {
        Value {
            inner: ValueInner::Null,
        }
    }

    pub fn undefined() -> Self {
        Value {
            inner: ValueInner::Undefined,
        }
    }

    pub fn kind(&self) -> ValueKind {
        match &self.inner {
            ValueInner::Undefined => ValueKind::Undefined,
            ValueInner::Null => ValueKind::Null,
            ValueInner::Bool(_) => ValueKind::Bool,
            ValueInner::U64(_) => ValueKind::U64,
            ValueInner::I64(_) => ValueKind::I64,
            ValueInner::F64(_) => ValueKind::F64,
            ValueInner::U128(_) => ValueKind::U128,
            ValueInner::I128(_) => ValueKind::I128,
            ValueInner::String(_) => ValueKind::String,
            ValueInner::Array(_) => ValueKind::Array,
            ValueInner::Map(_) => ValueKind::Map,
            ValueInner::Bytes(_) => ValueKind::Bytes,
        }
    }

    // Type checks
    pub fn is_undefined(&self) -> bool {
        matches!(self.kind(), ValueKind::Undefined)
    }
    pub fn is_null(&self) -> bool {
        matches!(self.kind(), ValueKind::Null)
    }
    pub fn is_bool(&self) -> bool {
        matches!(self.kind(), ValueKind::Bool)
    }
    pub fn is_string(&self) -> bool {
        matches!(self.kind(), ValueKind::String)
    }
    pub fn is_i128(&self) -> bool {
        matches!(self.kind(), ValueKind::I128)
    }
    pub fn is_u128(&self) -> bool {
        matches!(self.kind(), ValueKind::U128)
    }
    pub fn is_array(&self) -> bool {
        matches!(self.kind(), ValueKind::Array)
    }
    pub fn is_map(&self) -> bool {
        matches!(self.kind(), ValueKind::Map)
    }

    pub(crate) fn format(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        match &self.inner {
            ValueInner::Null | ValueInner::Undefined => Ok(()),
            ValueInner::Bool(v) => f.write_all(if *v { b"true" } else { b"false" }),
            ValueInner::Bytes(v) => f.write_all(v),
            ValueInner::String(v) => f.write_all(v.as_str().as_bytes()),
            ValueInner::Array(v) => {
                f.write_all(b"[")?;

                for (idx, elem) in v.iter().enumerate() {
                    if idx > 0 {
                        f.write_all(b", ")?;
                    }

                    match &elem.inner {
                        ValueInner::String(v) => write!(f, "{:?}", v.as_str())?,
                        _ => elem.format(f)?,
                    }
                }
                f.write_all(b"]")
            }
            ValueInner::Map(v) => format_map(v, f),
            ValueInner::F64(v) => {
                // We could use ryu to print floats but it doesn't match the output from
                // the std so tests become annoying.
                write!(f, "{v}")
            }
            ValueInner::U64(v) => {
                #[cfg(feature = "no_fmt")]
                {
                    let mut buf = itoa::Buffer::new();
                    f.write_all(buf.format(*v).as_bytes())
                }
                #[cfg(not(feature = "no_fmt"))]
                write!(f, "{v}")
            }
            ValueInner::I64(v) => {
                #[cfg(feature = "no_fmt")]
                {
                    let mut buf = itoa::Buffer::new();
                    f.write_all(buf.format(*v).as_bytes())
                }
                #[cfg(not(feature = "no_fmt"))]
                write!(f, "{v}")
            }
            ValueInner::U128(v) => {
                #[cfg(feature = "no_fmt")]
                {
                    let mut buf = itoa::Buffer::new();
                    f.write_all(buf.format(**v).as_bytes())
                }
                #[cfg(not(feature = "no_fmt"))]
                write!(f, "{}", **v)
            }
            ValueInner::I128(v) => {
                #[cfg(feature = "no_fmt")]
                {
                    let mut buf = itoa::Buffer::new();
                    f.write_all(buf.format(**v).as_bytes())
                }
                #[cfg(not(feature = "no_fmt"))]
                write!(f, "{}", **v)
            }
        }
    }

    pub fn from_serializable<T: Serialize + ?Sized>(value: &T) -> Value {
        Serialize::serialize(value, ser::ValueSerializer).unwrap()
    }

    pub fn normal_string(val: &str) -> Value {
        Value {
            inner: ValueInner::String(SmartString::new(val, StringKind::Normal)),
        }
    }

    pub fn safe_string(val: &str) -> Value {
        Value {
            inner: ValueInner::String(SmartString::new(val, StringKind::Safe)),
        }
    }

    pub(crate) fn as_i128(&self) -> Option<i128> {
        match &self.inner {
            ValueInner::U64(v) => Some(*v as i128),
            ValueInner::I64(v) => Some(*v as i128),
            // TODO: in theory this cannot necessarily fit in i128
            ValueInner::U128(v) => Some(**v as i128),
            ValueInner::I128(v) => Some(**v),
            _ => None,
        }
    }

    pub(crate) fn as_f64(&self) -> Option<f64> {
        // TODO: make sure we only cast to f64 if the value can fit in it
        match &self.inner {
            ValueInner::U64(v) => Some(*v as f64),
            ValueInner::I64(v) => Some(*v as f64),
            ValueInner::F64(v) => Some(*v),
            ValueInner::U128(v) => Some(**v as f64),
            ValueInner::I128(v) => Some(**v as f64),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<Number> {
        // TODO: this might be problematic to convert u128 to i128
        // We should probably expand the Number Enum
        match &self.inner {
            ValueInner::U64(v) => Some(Number::Integer(*v as i128)),
            ValueInner::I64(v) => Some(Number::Integer(*v as i128)),
            ValueInner::F64(v) => Some(Number::Float(*v)),
            ValueInner::U128(v) => Some(Number::Integer(**v as i128)),
            ValueInner::I128(v) => Some(Number::Integer(**v)),
            _ => None,
        }
    }

    pub(crate) fn is_number(&self) -> bool {
        matches!(
            &self.inner,
            ValueInner::U64(..)
                | ValueInner::I64(..)
                | ValueInner::F64(..)
                | ValueInner::U128(..)
                | ValueInner::I128(..)
        )
    }

    pub fn as_str(&self) -> Option<&str> {
        match &self.inner {
            ValueInner::String(s) => Some(s.as_str()),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match &self.inner {
            ValueInner::Bool(b) => Some(*b),
            _ => None,
        }
    }

    #[inline]
    pub fn is_safe(&self) -> bool {
        match &self.inner {
            ValueInner::String(s) => s.kind() == StringKind::Safe,
            ValueInner::Array(_) | ValueInner::Map(_) | ValueInner::Bytes(_) => false,
            _ => true,
        }
    }

    #[inline]
    pub fn mark_safe(self) -> Self {
        match self.inner {
            ValueInner::String(s) => Value {
                inner: ValueInner::String(s.mark_safe()),
            },
            _ => self,
        }
    }

    pub fn as_map(&self) -> Option<&Map> {
        match &self.inner {
            ValueInner::Map(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_vec(&self) -> Option<&Vec<Value>> {
        match &self.inner {
            ValueInner::Array(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_bytes(&self) -> Option<&[u8]> {
        match &self.inner {
            ValueInner::Bytes(s) => Some(s),
            _ => None,
        }
    }

    pub fn into_map(self) -> Option<Arc<Map>> {
        match self.inner {
            ValueInner::Map(s) => Some(s),
            _ => None,
        }
    }

    /// Returns the Value at the given path, or Undefined if there's nothing there.
    pub fn get_from_path(&self, path: &str) -> Value {
        if matches!(&self.inner, ValueInner::Undefined | ValueInner::Null) {
            return self.clone();
        }

        let mut res = self.clone();

        for elem in path.split('.') {
            match elem.parse::<usize>() {
                Ok(idx) => match &res.inner {
                    ValueInner::Array(arr) => {
                        if let Some(v) = arr.get(idx) {
                            res = v.clone();
                        } else {
                            return Value {
                                inner: ValueInner::Undefined,
                            };
                        }
                    }
                    _ => {
                        return Value {
                            inner: ValueInner::Undefined,
                        }
                    }
                },
                Err(_) => match &res.inner {
                    ValueInner::Map(map) => {
                        if let Some(v) = map.get(&Key::Str(elem)) {
                            res = v.clone();
                        } else {
                            return Value {
                                inner: ValueInner::Undefined,
                            };
                        }
                    }
                    _ => {
                        return Value {
                            inner: ValueInner::Undefined,
                        }
                    }
                },
            }
        }

        res
    }

    pub fn is_truthy(&self) -> bool {
        match &self.inner {
            ValueInner::Undefined => false,
            ValueInner::Null => false,
            ValueInner::Bool(v) => *v,
            ValueInner::U64(v) => *v != 0,
            ValueInner::I64(v) => *v != 0,
            ValueInner::F64(v) => *v != 0.0,
            ValueInner::U128(v) => **v != 0,
            ValueInner::I128(v) => **v != 0,
            ValueInner::Array(v) => !v.is_empty(),
            ValueInner::Bytes(v) => !v.is_empty(),
            ValueInner::String(v) => !v.as_str().is_empty(),
            ValueInner::Map(v) => !v.is_empty(),
        }
    }

    pub fn len(&self) -> Option<usize> {
        match &self.inner {
            ValueInner::Map(v) => Some(v.len()),
            ValueInner::Array(v) => Some(v.len()),
            ValueInner::Bytes(v) => Some(v.len()),
            ValueInner::String(v) => Some(v.as_str().chars().count()),
            _ => None,
        }
    }

    pub fn reverse(&self) -> TeraResult<Value> {
        match &self.inner {
            ValueInner::Array(v) => {
                let mut rev = (**v).clone();
                rev.reverse();
                Ok(Self::from(rev))
            }
            ValueInner::Bytes(v) => Ok(Self::from(v.iter().rev().copied().collect::<Vec<_>>())),
            ValueInner::String(v) => Ok(Self::from(String::from_iter(v.as_str().chars().rev()))),
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
            &self.inner,
            ValueInner::Map(..)
                | ValueInner::Array(..)
                | ValueInner::Bytes(..)
                | ValueInner::String(..)
        )
    }

    pub(crate) fn as_key(&self) -> TeraResult<Key<'static>> {
        let key = match &self.inner {
            ValueInner::Bool(v) => Key::Bool(*v),
            ValueInner::U64(v) => Key::U64(*v),
            ValueInner::I64(v) => Key::I64(*v),
            ValueInner::String(v) => Key::String(Arc::from(v.as_str())),
            _ => return Err(Error::message("Not a valid key type".to_string())),
        };
        Ok(key)
    }

    pub(crate) fn contains(&self, needle: &Value) -> TeraResult<bool> {
        match &self.inner {
            ValueInner::Array(arr) => Ok(arr.contains(needle)),
            ValueInner::String(s) => {
                if let Some(needle_str) = needle.as_str() {
                    Ok(s.as_str().contains(needle_str))
                } else {
                    Ok(false)
                }
            }
            // If they needle cannot index a map, then it can contain it
            ValueInner::Map(m) => match &needle.as_key() {
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
        match &self.inner {
            ValueInner::Map(m) => m.get(&Key::Str(attr)).cloned().unwrap_or(Value {
                inner: ValueInner::Undefined,
            }),
            _ => Value {
                inner: ValueInner::Undefined,
            },
        }
    }

    /// When doing hello[0], hello[name] etc, item is the value in the brackets
    pub(crate) fn get_item(&self, item: Value) -> TeraResult<Value> {
        match &self.inner {
            ValueInner::Map(m) => {
                match item.as_key() {
                    Ok(k) => Ok(m.get(&k).cloned().unwrap_or(Value { inner: ValueInner::Undefined })),
                    Err(_) => Err(Error::message(
                        format!("`{}` cannot be a key of a map/struct: only be integers, bool or strings are allowed", item.name()),
                    ))
                }
            }
            ValueInner::Array(arr) => {
                match item.as_i128() {
                    Some(idx) => {
                        let correct_idx = if idx < 0 {
                            arr.len() as i128 + idx
                        } else {
                            idx
                        } as usize;
                        Ok(arr.get(correct_idx).cloned().unwrap_or(Value { inner: ValueInner::Undefined }))
                    },
                    None =>  Err(Error::message(
                        format!("Array indices can only be integers, not `{}`.", item.name()),
                    ))
                }
            }
            _ => Ok(Value { inner: ValueInner::Undefined }),
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

        match &self.inner {
            ValueInner::Array(arr) => {
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
            ValueInner::String(s) => {
                let kind = s.kind();
                let mut out = Vec::with_capacity(s.len());

                #[cfg(feature = "unicode")]
                let mut input: Vec<&str> = unic_segment::Graphemes::new(s.as_str()).collect();
                #[cfg(not(feature = "unicode"))]
                let mut input: Vec<char> = s.as_str().chars().collect();

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
                    Ok(Value {
                        inner: ValueInner::String(SmartString::new(&out.join(""), kind)),
                    })
                }

                #[cfg(not(feature = "unicode"))]
                {
                    Ok(Value {
                        inner: ValueInner::String(SmartString::new(&String::from_iter(out), kind)),
                    })
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
    pub fn name(&self) -> &'static str {
        match &self.inner {
            ValueInner::Undefined => "undefined",
            ValueInner::Null => "null",
            ValueInner::Bool(_) => "bool",
            ValueInner::U64(_) => "u64",
            ValueInner::I64(_) => "i64",
            ValueInner::F64(_) => "f64",
            ValueInner::U128(_) => "u128",
            ValueInner::I128(_) => "i128",
            ValueInner::Array(_) => "array",
            ValueInner::Bytes(_) => "bytes",
            ValueInner::String(_) => "string",
            ValueInner::Map(_) => "map/struct",
        }
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match &self.inner {
            ValueInner::Null | ValueInner::Undefined => serializer.serialize_unit(),
            ValueInner::Bool(b) => serializer.serialize_bool(*b),
            ValueInner::U64(u) => serializer.serialize_u64(*u),
            ValueInner::I64(i) => serializer.serialize_i64(*i),
            ValueInner::F64(f) => serializer.serialize_f64(*f),
            ValueInner::U128(u) => serializer.serialize_u128(**u),
            ValueInner::I128(i) => serializer.serialize_i128(**i),
            ValueInner::Bytes(b) => serializer.serialize_bytes(b),
            ValueInner::String(s) => serializer.serialize_str(s.as_str()),
            ValueInner::Array(arr) => {
                let mut seq = serializer.serialize_seq(Some(arr.len()))?;
                for val in arr.iter() {
                    seq.serialize_element(val)?;
                }
                seq.end()
            }
            ValueInner::Map(map) => {
                let mut m = serializer.serialize_map(Some(map.len()))?;
                for (key, val) in map.iter() {
                    m.serialize_entry(key, val)?;
                }
                m.end()
            }
        }
    }
}

impl From<ValueInner> for Value {
    fn from(inner: ValueInner) -> Self {
        Value { inner }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value {
            inner: ValueInner::Bool(value),
        }
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value {
            inner: ValueInner::String(SmartString::new(value, StringKind::Normal)),
        }
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value {
            inner: ValueInner::String(SmartString::new(&value, StringKind::Normal)),
        }
    }
}

impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Value {
            inner: ValueInner::U64(value as u64),
        }
    }
}

impl From<i8> for Value {
    fn from(value: i8) -> Self {
        Value {
            inner: ValueInner::I64(value as i64),
        }
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value {
            inner: ValueInner::U64(value as u64),
        }
    }
}

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        Value {
            inner: ValueInner::U64(value),
        }
    }
}

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        Value {
            inner: ValueInner::U64(value as u64),
        }
    }
}

impl From<u128> for Value {
    fn from(value: u128) -> Self {
        Value {
            inner: ValueInner::U128(Box::new(value)),
        }
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value {
            inner: ValueInner::I64(value as i64),
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value {
            inner: ValueInner::I64(value),
        }
    }
}

impl From<isize> for Value {
    fn from(value: isize) -> Self {
        Value {
            inner: ValueInner::I64(value as i64),
        }
    }
}

impl From<i128> for Value {
    fn from(value: i128) -> Self {
        Value {
            inner: ValueInner::I128(Box::new(value)),
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value {
            inner: ValueInner::F64(value),
        }
    }
}

impl From<Key<'static>> for Value {
    fn from(value: Key<'static>) -> Self {
        match value {
            Key::Bool(b) => Value {
                inner: ValueInner::Bool(b),
            },
            Key::U64(u) => Value {
                inner: ValueInner::U64(u),
            },
            Key::I64(i) => Value {
                inner: ValueInner::I64(i),
            },
            Key::String(s) => Value {
                inner: ValueInner::String(SmartString::new(&s, StringKind::Normal)),
            },
            Key::Str(s) => Value {
                inner: ValueInner::String(SmartString::new(s, StringKind::Normal)),
            },
        }
    }
}

impl From<&[Value]> for Value {
    fn from(value: &[Value]) -> Self {
        Value {
            inner: ValueInner::Array(Arc::new(value.to_vec())),
        }
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(value: Vec<T>) -> Self {
        Value {
            inner: ValueInner::Array(Arc::new(value.into_iter().map(|v| v.into()).collect())),
        }
    }
}

impl<T: Into<Value>> From<BTreeSet<T>> for Value {
    fn from(value: BTreeSet<T>) -> Self {
        Value {
            inner: ValueInner::Array(Arc::new(value.into_iter().map(|v| v.into()).collect())),
        }
    }
}

impl<K: Into<Key<'static>>, T: Into<Value>> From<HashMap<K, T>> for Value {
    fn from(input: HashMap<K, T>) -> Self {
        let mut map = Map::with_capacity(input.len());
        for (key, value) in input {
            map.insert(key.into(), value.into());
        }
        Value {
            inner: ValueInner::Map(Arc::new(map)),
        }
    }
}

impl<K: Into<Key<'static>>, T: Into<Value>> From<BTreeMap<K, T>> for Value {
    fn from(input: BTreeMap<K, T>) -> Self {
        let mut map = Map::with_capacity(input.len());
        for (key, value) in input {
            map.insert(key.into(), value.into());
        }
        Value {
            inner: ValueInner::Map(Arc::new(map)),
        }
    }
}

#[cfg(feature = "preserve_order")]
impl<K: Into<Key<'static>>, T: Into<Value>> From<indexmap::IndexMap<K, T>> for Value {
    fn from(input: indexmap::IndexMap<K, T>) -> Self {
        let mut map = Map::with_capacity(input.len());
        for (key, value) in input {
            map.insert(key.into(), value.into());
        }
        Value {
            inner: ValueInner::Map(Arc::new(map)),
        }
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
