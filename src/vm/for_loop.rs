use crate::value::{Key, StringKind};
use crate::Value;

use serde::ser::SerializeStruct;
use serde::{Serialize, Serializer};
use std::collections::{BTreeMap, VecDeque};
use std::sync::Arc;

// TODO: perf improvements, less to_string

/// Enumerates on the types of values to be iterated, scalars and pairs
#[derive(Debug, PartialEq)]
pub enum ForLoopValues {
    /// Values for an array style iteration
    Array(VecDeque<Value>),
    Bytes(VecDeque<u8>),
    // TODO: use unic-segment as a feature and use graphemes rather than char
    /// Values for a per-character iteration on a string
    String(VecDeque<char>),
    /// Values for an object style iteration
    Object(VecDeque<(Key, Value)>),
}

impl ForLoopValues {
    #[inline(always)]
    pub fn pop_front(&mut self) -> (Value, Value) {
        match self {
            ForLoopValues::Array(a) => (Value::Null, a.pop_front().unwrap()),
            ForLoopValues::Bytes(a) => (Value::Null, Value::U64(a.pop_front().unwrap() as u64)),
            ForLoopValues::String(a) => (
                Value::Null,
                Value::String(
                    Arc::new(a.pop_front().unwrap().to_string()),
                    StringKind::Normal,
                ),
            ),
            ForLoopValues::Object(a) => {
                let (key, value) = a.pop_front().unwrap();
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
    context: BTreeMap<String, Value>,
    value_name: Option<String>,
    key_name: Option<String>,
    current_values: (Value, Value),
}

impl ForLoop {
    pub fn new(is_key_value: bool, container: Value) -> Self {
        // TODO: keep an iterator instead of that thing
        let values = match container {
            Value::Map(map) => {
                if !is_key_value {
                    todo!("Error");
                }
                let mut vals = VecDeque::with_capacity(map.len());
                for (key, val) in map.as_ref() {
                    vals.push_back((key.clone(), val.clone()));
                }
                ForLoopValues::Object(vals)
            }
            Value::String(s, _) => {
                if is_key_value {
                    todo!("Error");
                }

                let chars: VecDeque<_> = s.chars().collect();
                ForLoopValues::String(chars)
            }
            Value::Bytes(b) => {
                if is_key_value {
                    todo!("Error")
                }
                let mut bytes = VecDeque::with_capacity(b.len());
                for a in b.iter() {
                    bytes.push_back(*a);
                }
                // TODO: add tests to loops on bytes
                ForLoopValues::Bytes(bytes)
            }
            Value::Array(arr) => {
                if is_key_value {
                    todo!("Error");
                }
                let mut vals = VecDeque::with_capacity(arr.len());
                for a in arr.iter() {
                    vals.push_back(a.clone());
                }
                ForLoopValues::Array(vals)
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
            context: BTreeMap::new(),
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
            return;
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
