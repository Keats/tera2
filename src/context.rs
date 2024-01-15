use std::collections::BTreeMap;

use serde::Serialize;

use crate::value::Value;

/// The struct that holds the context of a template rendering.
///
/// Light wrapper around a `BTreeMap` for easier insertions of Serializable
/// values
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Context {
    // TODO: have keys be Cow<'static, str>?
    pub(crate) data: BTreeMap<String, Value>,
}

impl Context {
    /// Initializes an empty context
    pub fn new() -> Self {
        Self::default()
    }

    /// Converts the `val` parameter to `Value` and insert it into the context.
    ///
    /// ```rust
    /// # use tera::Context;
    /// let mut context = tera::Context::new();
    /// context.insert("number_users", &42);
    /// ```
    pub fn insert<S: Into<String>, T: Serialize + ?Sized>(&mut self, key: S, val: &T) {
        self.data.insert(key.into(), Value::from_serializable(val));
    }

    /// In case you already have a `Value` you want to insert
    pub fn insert_value<S: Into<String>>(&mut self, key: S, val: Value) {
        self.data.insert(key.into(), val);
    }

    /// Remove a key from the context, returning the value at the key if the key was previously inserted into the context.
    pub fn remove(&mut self, key: &str) -> Option<Value> {
        self.data.remove(key)
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

    /// Checks if a value exists for given key.
    pub fn contains_key(&self, key: &str) -> bool {
        self.data.contains_key(key)
    }

    /// Returns the value at the given key
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.data.get(key)
    }
}

/// Creates a context from key value pairs
///
/// Example:
/// ```rust
/// let ctx = context! {
///     name => "Brian",
///     age => &24
/// };
/// ```
/// Expands to:
/// ```
/// let ctx = {
///     let mut context = Context::new();
///     context.insert("name", "Brian");
///     context.insert("age", &24);
///     context
/// };
///
#[macro_export]
macro_rules! context {
    (
        $(
            $key:ident $(=> $value:expr)? $(,)*
        )*
    ) => {
        {
            let mut context = Context::new();
            $(
                context.insert(stringify!($key), $($value)?);
            )*
            context
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn context_macro_builder() {
        let left = context! {
            foo => "Bar",
            con => &69
        };

        let mut right = Context::new();
        right.insert("foo", "Bar");
        right.insert("con", &69);

        assert_eq!(left, right);
    }

    #[test]
    fn context_tests() {
        let ctx = context! {
            name => "John Doe",
            age => &42,
        };

        assert_eq!(
            "{\"age\":42,\"name\":\"John Doe\"}".to_string(),
            ctx.to_json().unwrap()
        );
        assert_eq!(ctx.contains_key("age"), true);
        assert_eq!(ctx.get("age"), Some(&Value::I64(42)));
    }
}

/// Creates a context from key value pairs
///
/// Example:
/// ```rust
/// let ctx = context! {
///     name => "Brian",
///     age => &24
/// };
/// ```
/// Expands to:
/// ```
/// let ctx = {
///     let mut context = Context::new();
///     context.insert("name", "Brian");
///     context.insert("age", &24);
///     context
/// };
///
#[macro_export]
macro_rules! context {
    (
        $(
            $key:ident $(=> $value:expr)? $(,)*
        )*
    ) => {
        {
            let mut context = Context::new();
            $(
                context.insert(stringify!($key), $($value)?);
            )*
            context
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn context_macro_builder() {
        let left = context! {
            foo => "Bar",
            con => &69
        };

        let mut right = Context::new();
        right.insert("foo", "Bar");
        right.insert("con", &69);

        assert_eq!(left, right);
    }
}
