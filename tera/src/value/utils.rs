use serde::{de, ser};
use std::fmt;

#[derive(Debug)]
pub struct SerializationFailed;
impl fmt::Display for SerializationFailed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "serialization failed")
    }
}
impl std::error::Error for SerializationFailed {}
impl ser::Error for SerializationFailed {
    #[track_caller]
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        panic!("{}", msg)
    }
}

#[derive(Debug)]
pub struct DeserializationFailed;
impl fmt::Display for DeserializationFailed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "deserialization failed")
    }
}
impl std::error::Error for DeserializationFailed {}
impl de::Error for DeserializationFailed {
    #[track_caller]
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        panic!("{}", msg)
    }
}
