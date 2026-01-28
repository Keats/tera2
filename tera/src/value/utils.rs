use serde::{de, ser};
use std::fmt;

#[derive(Debug)]
pub struct SerializationFailed(pub String);
impl fmt::Display for SerializationFailed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "serialization failed: {}", self.0)
    }
}
impl std::error::Error for SerializationFailed {}
impl ser::Error for SerializationFailed {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        SerializationFailed(msg.to_string())
    }
}

#[derive(Debug)]
pub struct DeserializationFailed(pub String);
impl fmt::Display for DeserializationFailed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "deserialization failed: {}", self.0)
    }
}
impl std::error::Error for DeserializationFailed {}
impl de::Error for DeserializationFailed {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        DeserializationFailed(msg.to_string())
    }
}
