use crate::value::Key;
use crate::Value;

use serde::ser::SerializeStruct;
use serde::{Serialize, Serializer};
use std::collections::BTreeMap;
use std::sync::Arc;

/// Enumerates on the types of values to be iterated, scalars and pairs
#[derive(Debug)]
pub enum ForLoopValues {
    /// Values for an array style iteration
    Array(Arc<Vec<Value>>),
    // TODO: use unic-segment as a feature and use graphemes rather than char
    /// Values for a per-character iteration on a string
    String(Vec<char>),
    /// Values for an object style iteration
    Object(Vec<(Key, Value)>),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub(crate) struct ForLoop {
    current_idx: usize,
    is_key_value: bool,
    values: ForLoopValues,
    length: usize,
    loop_data: Loop,
    pub(crate) end_ip: usize,
    /// Values set in the loop, including the key and value
    context: BTreeMap<String, Value>,
    value_set: bool,
}

impl ForLoop {
    pub fn new(is_key_value: bool, container: Value) -> Self {
        let mut length = 0;
        let values = match container {
            Value::Map(map) => {
                if !is_key_value {
                    todo!("Error");
                }
                let mut vals = Vec::with_capacity(map.len());
                length = map.len();
                for (key, val) in map.as_ref() {
                    vals.push((key.clone(), val.clone()));
                }
                ForLoopValues::Object(vals)
            }
            Value::String(s) => {
                if is_key_value {
                    todo!("Error");
                }

                let chars: Vec<_> = s.chars().collect();
                length = chars.len();
                ForLoopValues::String(chars)
            }
            Value::Array(arr) => {
                if is_key_value {
                    todo!("Error");
                }
                length = arr.len();
                ForLoopValues::Array(arr)
            }
            _ => todo!("handle error"),
        };

        let loop_data = Loop {
            index: 1,
            index0: 0,
            first: true,
            last: length == 0,
            length,
        };
        let mut context = BTreeMap::new();
        context.insert("loop".to_string(), Value::from_serializable(&loop_data));

        Self {
            is_key_value,
            values,
            length,
            loop_data,
            end_ip: 0,
            current_idx: 0,
            context,
            value_set: false,
        }
    }

    pub(crate) fn store_local(&mut self, name: &str) {
        let (key, value) = self.get();
        if self.value_set {
            self.context.insert(name.to_string(), key);
        } else {
            self.context.insert(name.to_string(), value);
            self.value_set = true;
        }
    }

    /// Advance the counter only after the end ip has been set (eg we start incrementing only from the
    /// second time we see the loop)
    pub(crate) fn advance(&mut self) {
        if self.end_ip != 0 {
            self.current_idx += 1;
            self.value_set = false;
            self.loop_data.advance();
            self.context.clear();
            // TODO: not great to have to serialize it every loop even if we don't use it...
            self.context.insert(
                "loop".to_string(),
                Value::from_serializable(&self.loop_data),
            );
        }
    }

    pub(crate) fn is_over(&self) -> bool {
        self.end_ip != 0 && self.current_idx == self.loop_data.length - 1
    }

    pub(crate) fn store(&mut self, name: &str, value: Value) {
        self.context.insert(name.to_string(), value);
    }

    pub(crate) fn get(&self) -> (Value, Value) {
        match &self.values {
            ForLoopValues::Array(arr) => (Value::Null, arr[self.current_idx].clone()),
            ForLoopValues::String(s) => (Value::Null, Value::Char(s[self.current_idx].clone())),
            ForLoopValues::Object(obj) => {
                let (key, value) = obj[self.current_idx].clone();
                (key.into(), value)
            }
        }
    }

    pub(crate) fn get_by_name(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.context.get(name) {
            Some(val.clone())
        } else {
            None
        }
    }
}
