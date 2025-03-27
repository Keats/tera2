use crate::value::Key;
#[cfg(not(feature = "unicode"))]
use crate::value::StringKind;
use crate::{HashMap, Value};

#[cfg(not(feature = "unicode"))]
use std::sync::Arc;

/// Enumerates on the types of values to be iterated, scalars and pairs
#[derive(Debug)]
pub enum ForLoopValues {
    /// Values for an array style iteration
    Array(std::vec::IntoIter<Value>),
    Bytes(std::vec::IntoIter<u8>),
    #[cfg(feature = "unicode")]
    Graphemes(std::vec::IntoIter<Value>),
    /// Values for a per-character iteration on a string
    #[cfg(not(feature = "unicode"))]
    String(std::vec::IntoIter<char>),
    /// Values for an object style iteration
    Object(std::vec::IntoIter<(Key<'static>, Value)>),
}

impl ForLoopValues {
    #[inline(always)]
    pub fn pop_front(&mut self) -> (Value, Value) {
        match self {
            ForLoopValues::Array(a) => (Value::Null, a.next().unwrap()),
            ForLoopValues::Bytes(a) => (Value::Null, Value::U64(a.next().unwrap() as u64)),
            #[cfg(not(feature = "unicode"))]
            ForLoopValues::String(a) => (
                Value::Null,
                Value::String(Arc::from(a.next().unwrap().to_string()), StringKind::Normal),
            ),
            #[cfg(feature = "unicode")]
            ForLoopValues::Graphemes(a) => (Value::Null, a.next().unwrap()),
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
            #[cfg(not(feature = "unicode"))]
            ForLoopValues::String(a) => a.len(),
            #[cfg(feature = "unicode")]
            ForLoopValues::Graphemes(a) => a.len(),
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

#[derive(Debug)]
pub(crate) struct ForLoop {
    values: ForLoopValues,
    loop_data: Loop,
    pub(crate) end_ip: usize,
    pub(crate) context: HashMap<String, Value>,
    value_name: Option<String>,
    key_name: Option<String>,
    current_values: (Value, Value),
}

impl ForLoop {
    pub fn new(container: Value) -> Self {
        let values = match container {
            Value::Map(map) => {
                let vals: Vec<_> = map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                ForLoopValues::Object(vals.into_iter())
            }
            Value::String(s, _) => {
                #[cfg(feature = "unicode")]
                {
                    let graphemes: Vec<&str> = unic_segment::Graphemes::new(&*s).collect();
                    let graphemes: Vec<_> = graphemes.into_iter().map(|x| Value::from(x)).collect();
                    ForLoopValues::Graphemes(graphemes.into_iter())
                }

                #[cfg(not(feature = "unicode"))]
                {
                    let chars: Vec<_> = s.chars().collect();
                    ForLoopValues::String(chars.into_iter())
                }
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
            _ => unreachable!("Should be handled in the interpreter"),
        };

        let length = values.len();
        let loop_data = Loop {
            index: 1,
            index0: 0,
            first: true,
            last: length == 1,
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

    pub(crate) fn store(&mut self, name: &str, value: Value) {
        self.context.insert(name.to_string(), value);
    }

    pub(crate) fn get(&self, name: &str) -> Option<Value> {
        // Special casing the loop variable
        match name {
            "__tera_loop_index" => Some(Value::U64(self.loop_data.index as u64)),
            "__tera_loop_index0" => Some(Value::U64(self.loop_data.index0 as u64)),
            "__tera_loop_first" => Some(Value::Bool(self.loop_data.first)),
            "__tera_loop_last" => Some(Value::Bool(self.loop_data.last)),
            "__tera_loop_length" => Some(Value::U64(self.loop_data.length as u64)),
            _ => {
                if self.value_name.as_deref() == Some(name) {
                    return Some(self.current_values.1.clone());
                }

                if self.key_name.as_deref() == Some(name) {
                    return Some(self.current_values.0.clone());
                }

                self.context.get(name).cloned()
            }
        }
    }
}
