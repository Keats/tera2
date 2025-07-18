use crate::Value;
use serde::{Serialize, Serializer};
use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};
use std::sync::Arc;

/// The key of anything looking like a hashmap (struct/hashmaps)
#[derive(Debug, Clone)]
pub enum Key<'a> {
    Bool(bool),
    U64(u64),
    I64(i64),
    String(Arc<str>),
    Str(&'a str),
}

impl<'a> Key<'a> {
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Key::String(s) => Some(s),
            Key::Str(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_value(&self) -> Value {
        match self {
            Key::Bool(b) => Value::from(*b),
            Key::U64(b) => Value::from(*b),
            Key::I64(b) => Value::from(*b),
            Key::String(b) => Value::normal_string(b.as_ref()),
            Key::Str(b) => Value::normal_string(*b),
        }
    }

    pub(crate) fn format(&self, f: &mut impl std::io::Write) -> std::io::Result<()> {
        match self {
            Key::Bool(v) => f.write_all(if *v { b"true" } else { b"false" }),
            Key::String(v) => f.write_all(v.as_bytes()),
            Key::Str(v) => f.write_all(v.as_bytes()),
            #[cfg(feature = "no_fmt")]
            Key::U64(v) => {
                let mut buf = itoa::Buffer::new();
                f.write_all(buf.format(*v).as_bytes())
            }
            #[cfg(feature = "no_fmt")]
            Key::I64(v) => {
                let mut buf = itoa::Buffer::new();
                f.write_all(buf.format(*v).as_bytes())
            }
            #[cfg(not(feature = "no_fmt"))]
            _ => write!(f, "{self}"),
        }
    }
}

impl<'a> PartialEq for Key<'a> {
    fn eq(&self, other: &Self) -> bool {
        if let (Some(a), Some(b)) = (self.as_str(), other.as_str()) {
            a.eq(b)
        } else {
            self.as_value().eq(&other.as_value())
        }
    }
}

impl<'a> Eq for Key<'a> {}

impl<'a> PartialOrd for Key<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for Key<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        if let (Some(a), Some(b)) = (self.as_str(), other.as_str()) {
            a.cmp(b)
        } else {
            self.as_value().cmp(&other.as_value())
        }
    }
}

impl<'a> Hash for Key<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if let Some(s) = self.as_str() {
            s.hash(state)
        } else {
            self.as_value().hash(state)
        }
    }
}

impl<'a> fmt::Display for Key<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Key::Bool(v) => write!(f, "{v}"),
            Key::U64(v) => write!(f, "{v}"),
            Key::I64(v) => write!(f, "{v}"),
            Key::Str(v) => write!(f, "{v}"),
            Key::String(v) => write!(f, "{v}"),
        }
    }
}

impl<'a> Serialize for Key<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Key::Bool(b) => serializer.serialize_bool(*b),
            Key::U64(u) => serializer.serialize_u64(*u),
            Key::I64(i) => serializer.serialize_i64(*i),
            Key::String(s) => serializer.serialize_str(s),
            Key::Str(s) => serializer.serialize_str(s),
        }
    }
}

impl From<&'static str> for Key<'static> {
    fn from(value: &'static str) -> Self {
        Key::Str(value)
    }
}

impl From<String> for Key<'static> {
    fn from(value: String) -> Self {
        Key::String(Arc::from(value))
    }
}

impl<'a> From<Cow<'a, str>> for Key<'static> {
    fn from(value: Cow<'a, str>) -> Self {
        match value {
            Cow::Borrowed(s) => Key::String(Arc::from(s)),
            Cow::Owned(s) => Key::String(Arc::from(s)),
        }
    }
}
