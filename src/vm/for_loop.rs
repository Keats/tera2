use crate::value::{Key, StringKind};
use crate::Value;

use serde::ser::SerializeStruct;
use serde::{Serialize, Serializer};
use std::collections::HashMap;
use std::sync::Arc;

// TODO: perf improvements, less to_string

/// Enumerates on the types of values to be iterated, scalars and pairs
#[derive(Debug)]
pub enum ForLoopValues {
    /// Values for an array style iteration
    Array(std::vec::IntoIter<Value>),
    Bytes(std::vec::IntoIter<u8>),
    // TODO: use unic-segment as a feature and use graphemes rather than char
    /// Values for a per-character iteration on a string
    String(std::vec::IntoIter<char>),
    /// Values for an object style iteration
    Object(std::vec::IntoIter<(Key, Value)>),
}

impl PartialEq for ForLoopValues {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Array(_), Self::Array(_))
            | (Self::Bytes(_), Self::Bytes(_))
            | (Self::String(_), Self::String(_))
            | (Self::Object(_), Self::Object(_)) => true,
            _ => false,
        }
    }
}

impl ForLoopValues {
    #[inline(always)]
    pub fn pop_front(&mut self) -> (Value, Value) {
        match self {
            ForLoopValues::Array(a) => (Value::Null, a.next().unwrap()),
            ForLoopValues::Bytes(a) => (Value::Null, Value::U64(a.next().unwrap() as u64)),
            ForLoopValues::String(a) => (
                Value::Null,
                Value::String(Arc::new(a.next().unwrap().to_string()), StringKind::Normal),
            ),
            ForLoopValues::Object(a) => {
                let (key, value) = a.next().unwrap();
                (key.into(), value)
            }
        }
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        match self {
            ForLoopValues::Array(a) => a.len(),
            ForLoopValues::Bytes(a) => a.len(),
            ForLoopValues::String(a) => a.len(),
            ForLoopValues::Object(a) => a.len(),
        }
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Loop {
    index: usize,
    index0: usize,
    first: bool,
    last: bool,
    length: usize,
}

impl Loop {
    fn advance(&mut self) {
        self.index += 1;
        self.index0 += 1;
        self.first = false;
        self.last = self.index == self.length;
    }
}

impl Serialize for Loop {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("Loop", 5)?;
        s.serialize_field("index", &self.index)?;
        s.serialize_field("index0", &self.index0)?;
        s.serialize_field("first", &self.first)?;
        s.serialize_field("last", &self.last)?;
        s.serialize_field("length", &self.length)?;
        s.end()
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct ForLoop {
    values: ForLoopValues,
    loop_data: Loop,
    pub(crate) end_ip: usize,
    context: HashMap<String, Value>,
    value_name: Option<String>,
    key_name: Option<String>,
    current_values: (Value, Value),
}

impl ForLoop {
    pub fn new(is_key_value: bool, container: Value) -> Self {
        // Either both is_key_value and is_map are true, or false, else error
        if is_key_value != matches!(container, Value::Map(_)) {
            todo!("Error");
        }

        // TODO: keep an iterator instead of that thing
        let values = match container {
            Value::Map(map) => {
                let vals: Vec<_> = map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                ForLoopValues::Object(vals.into_iter())
            }
            Value::String(s, _) => {
                let chars: Vec<_> = s.chars().collect();
                ForLoopValues::String(chars.into_iter())
            }
            Value::Bytes(b) => {
                let bytes: Vec<_> = b.iter().copied().collect();
                // TODO: add tests to loops on bytes
                ForLoopValues::Bytes(bytes.into_iter())
            }
            Value::Array(arr) => {
                let vals: Vec<_> = arr.iter().cloned().collect();
                ForLoopValues::Array(vals.into_iter())
            }
            _ => todo!("handle error"),
        };

        let length = values.len();
        let loop_data = Loop {
            index: 1,
            index0: 0,
            first: true,
            last: length == 0,
            length,
        };

        Self {
            values,
            loop_data,
            end_ip: 0,
            context: HashMap::new(),
            value_name: None,
            key_name: None,
            current_values: (Value::Null, Value::Null),
        }
    }

    pub(crate) fn store_local(&mut self, name: &str) {
        if self.value_name.is_none() {
            self.value_name = Some(name.to_string());
        } else if self.key_name.is_none() {
            self.key_name = Some(name.to_string());
        }
    }

    /// Advance the counter only after the end ip has been set (eg we start incrementing only from the
    /// second time we see the loop)
    pub(crate) fn advance(&mut self) {
        if self.end_ip == 0 {
            self.current_values = self.values.pop_front();
        } else {
            self.loop_data.advance();
            self.context.clear();
            self.current_values = self.values.pop_front();
        }
    }

    #[inline(always)]
    pub(crate) fn is_over(&self) -> bool {
        self.values.is_empty()
            || (self.end_ip != 0 && self.loop_data.index0 == self.loop_data.length - 1)
    }

    pub(crate) fn iterated(&self) -> bool {
        self.loop_data.index0 > 0
    }

    pub(crate) fn clear_context(&mut self) {
        self.context.clear();
    }

    pub(crate) fn store(&mut self, name: &str, value: Value) {
        self.context.insert(name.to_string(), value);
    }

    pub(crate) fn get(&self, name: &str) -> Option<Value> {
        // Special casing the loop variable
        if name == "loop" {
            return Some(Value::from_serializable(&self.loop_data));
        }

        if self.value_name.as_deref() == Some(name) {
            return Some(self.current_values.1.clone());
        }

        if self.key_name.as_deref() == Some(name) {
            return Some(self.current_values.0.clone());
        }

        self.context.get(name).cloned()
    }
}
