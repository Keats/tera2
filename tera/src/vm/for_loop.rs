use crate::value::{Key, Map, ValueInner};
use crate::{HashMap, Value};
use std::sync::Arc;

/// Lazy iterator for for-loop values that only clones the current item
#[derive(Debug)]
pub(crate) enum ForLoopIterator {
    Array {
        arr: Arc<Vec<Value>>,
        index: usize,
        len: usize,
    },
    Map {
        map: Arc<Map>,
        keys: Vec<Key<'static>>,
        index: usize,
    },
    String {
        content: Arc<str>,
        char_indices: Vec<usize>,
        index: usize,
    },
    Bytes {
        bytes: Arc<Vec<u8>>,
        index: usize,
        len: usize,
    },
    #[cfg(feature = "unicode")]
    Graphemes {
        content: Arc<str>,
        grapheme_indices: Vec<usize>,
        index: usize,
    },
}

impl Iterator for ForLoopIterator {
    type Item = (Option<Value>, Value);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ForLoopIterator::Array { arr, index, len } => {
                if *index < *len {
                    let value = arr[*index].clone();
                    *index += 1;
                    Some((None, value))
                } else {
                    None
                }
            }

            ForLoopIterator::Map { map, keys, index } => {
                if *index < keys.len() {
                    let key = &keys[*index];
                    let value = map[key].clone();
                    let key_value = key.clone().into();
                    *index += 1;
                    // Key is present only for maps
                    Some((Some(key_value), value))
                } else {
                    None
                }
            }

            ForLoopIterator::String {
                content,
                char_indices,
                index,
            } => {
                if *index < char_indices.len() {
                    let start = char_indices[*index];
                    let end = char_indices
                        .get(*index + 1)
                        .copied()
                        .unwrap_or(content.len());

                    let char_str = &content[start..end];
                    let value = Value::from(char_str);
                    *index += 1;
                    Some((None, value))
                } else {
                    None
                }
            }

            ForLoopIterator::Bytes { bytes, index, len } => {
                if *index < *len {
                    let value = Value::from(bytes[*index] as u64);
                    *index += 1;
                    Some((None, value))
                } else {
                    None
                }
            }

            #[cfg(feature = "unicode")]
            ForLoopIterator::Graphemes {
                content,
                grapheme_indices,
                index,
            } => {
                if *index < grapheme_indices.len() {
                    let start = grapheme_indices[*index];
                    let end = grapheme_indices
                        .get(*index + 1)
                        .copied()
                        .unwrap_or(content.len());

                    let grapheme_str = &content[start..end];
                    let value = Value::from(grapheme_str);
                    *index += 1;
                    Some((None, value))
                } else {
                    None
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = match self {
            ForLoopIterator::Array { index, len, .. } => len - index,
            ForLoopIterator::Map { keys, index, .. } => keys.len() - index,
            ForLoopIterator::String {
                char_indices,
                index,
                ..
            } => char_indices.len() - index,
            ForLoopIterator::Bytes { index, len, .. } => len - index,
            #[cfg(feature = "unicode")]
            ForLoopIterator::Graphemes {
                grapheme_indices,
                index,
                ..
            } => grapheme_indices.len() - index,
        };
        (remaining, Some(remaining))
    }
}

/// Create a lazy iterator for for-loop iteration that only clones the current item
pub(crate) fn create_for_loop_iterator(value: &Value) -> Option<ForLoopIterator> {
    match &value.inner {
        ValueInner::Array(arr) => Some(ForLoopIterator::Array {
            arr: Arc::clone(arr),
            index: 0,
            len: arr.len(),
        }),

        ValueInner::Map(map) => {
            let keys: Vec<_> = map.keys().cloned().collect();
            Some(ForLoopIterator::Map {
                map: Arc::clone(map),
                keys,
                index: 0,
            })
        }

        ValueInner::String(smart_str, _) => {
            let content = smart_str.clone().into_arc_str();
            #[cfg(feature = "unicode")]
            {
                let grapheme_indices: Vec<_> = unic_segment::Graphemes::new(&content)
                    .map(|_| 0) // We'll compute actual indices
                    .collect();
                let mut indices = Vec::with_capacity(grapheme_indices.len());
                let mut pos = 0;
                for grapheme in unic_segment::Graphemes::new(&content) {
                    indices.push(pos);
                    pos += grapheme.len();
                }
                Some(ForLoopIterator::Graphemes {
                    content,
                    grapheme_indices: indices,
                    index: 0,
                })
            }
            #[cfg(not(feature = "unicode"))]
            {
                let char_indices: Vec<_> = content.char_indices().map(|(i, _)| i).collect();
                Some(ForLoopIterator::String {
                    content,
                    char_indices,
                    index: 0,
                })
            }
        }

        ValueInner::Bytes(bytes) => Some(ForLoopIterator::Bytes {
            bytes: Arc::clone(bytes),
            index: 0,
            len: bytes.len(),
        }),

        _ => None,
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
    #[inline(always)]
    fn advance(&mut self) {
        self.index += 1;
        self.index0 += 1;
        self.first = false;
        self.last = self.index == self.length;
    }
}

#[derive(Debug)]
pub(crate) struct ForLoop {
    iterator: ForLoopIterator,
    loop_data: Loop,
    pub(crate) end_ip: usize,
    pub(crate) context: HashMap<String, Value>,
    value_name: Option<String>,
    key_name: Option<String>,
    current_values: (Option<Value>, Value),
}

impl ForLoop {
    pub fn new(container: Value) -> Self {
        let iterator = create_for_loop_iterator(&container)
            .expect("Should only be called on iterable values");

        let length = iterator.size_hint().1.unwrap_or(0);
        let loop_data = Loop {
            index: 1,
            index0: 0,
            first: true,
            last: length == 1,
            length,
        };

        Self {
            iterator,
            loop_data,
            end_ip: 0,
            context: HashMap::new(),
            value_name: None,
            key_name: None,
            current_values: (None, Value::null()),
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
    #[inline(always)]
    pub(crate) fn advance(&mut self) {
        if let Some((key, value)) = self.iterator.next() {
            self.current_values = (key, value);
            if self.end_ip != 0 {
                self.loop_data.advance();
                self.context.clear();
            }
        }
    }

    #[inline(always)]
    pub(crate) fn is_over(&self) -> bool {
        self.iterator.size_hint().0 == 0
            || (self.end_ip != 0 && self.loop_data.index0 >= self.loop_data.length)
    }

    pub(crate) fn iterated(&self) -> bool {
        self.loop_data.index0 > 0
    }

    pub(crate) fn store(&mut self, name: &str, value: Value) {
        self.context.insert(name.to_string(), value);
    }

    #[inline(always)]
    pub(crate) fn get(&self, name: &str) -> Option<Value> {
        // Special casing the loop variable
        match name {
            "__tera_loop_index" => Some(Value::from(self.loop_data.index as u64)),
            "__tera_loop_index0" => Some(Value::from(self.loop_data.index0 as u64)),
            "__tera_loop_first" => Some(Value::from(self.loop_data.first)),
            "__tera_loop_last" => Some(Value::from(self.loop_data.last)),
            "__tera_loop_length" => Some(Value::from(self.loop_data.length as u64)),
            _ => {
                if self.value_name.as_deref() == Some(name) {
                    return Some(self.current_values.1.clone());
                }

                if self.key_name.as_deref() == Some(name) {
                    return self
                        .current_values
                        .0
                        .clone()
                        .or_else(|| Some(Value::null()));
                }

                self.context.get(name).cloned()
            }
        }
    }
}
