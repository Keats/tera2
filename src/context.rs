use std::collections::BTreeMap;

use serde::Serialize;

use crate::value::Value;

/// The struct that holds the context of a template rendering.
///
/// Light wrapper around a `BTreeMap` for easier insertions of Serializable
/// values
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Context {
    pub(crate) data: BTreeMap<String, Value>,
}

impl Context {
    /// Initializes an empty context
    pub fn new() -> Self {
        Self::default()
    }

    /// Converts the `val` parameter to `Value` and insert it into the context.
    ///
    /// Panics if the serialization fails.
    ///
    /// ```rust
    /// # use tera::Context;
    /// let mut context = tera::Context::new();
    /// context.insert("number_users", &42);
    /// ```
    pub fn insert<S: Into<String>, T: Serialize>(&mut self, key: S, val: &T) {
        self.data.insert(key.into(), Value::from_serializable(val));
    }

    /// In case you already have a `Value` you want to insert, we are going to clone it.
    pub fn insert_value<S: Into<String>>(&mut self, key: S, val: &Value) {
        self.data.insert(key.into(), val.clone());
    }

    /// In case you already have a `Value` you want to insert and are ok with moving it inside the
    /// context
    pub fn move_value<S: Into<String>>(&mut self, key: S, val: Value) {
        self.data.insert(key.into(), val);
    }

    /// Remove a key from the context, returning the value at the key if the key was previously inserted into the context.
    pub fn remove(&mut self, index: &str) -> Option<Value> {
        self.data.remove(index)
    }

    /// Appends the data of the `source` parameter to `self`, overwriting existing keys.
    /// The source context will be dropped.
    ///
    /// ```rust
    /// # use tera::Context;
    /// let mut target = Context::new();
    /// target.insert("a", &1);
    /// target.insert("b", &2);
    /// let mut source = Context::new();
    /// source.insert("b", &3);
    /// source.insert("d", &4);
    /// target.extend(source);
    /// ```
    pub fn extend(&mut self, mut source: Context) {
        self.data.append(&mut source.data);
    }
}
