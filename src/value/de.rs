use crate::value::utils::DeserializationFailed;
use crate::Value;
use serde::de::{self, Visitor};
use serde::forward_to_deserialize_any;

pub struct ValueDeserializer {
    value: Value,
}

impl ValueDeserializer {
    pub fn from_value(value: Value) -> Self {
        Self { value }
    }
}

impl<'de> de::Deserializer<'de> for ValueDeserializer {
    type Error = DeserializationFailed;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Bool(v) => visitor.visit_bool(v),
            Value::I64(v) => visitor.visit_i64(v),
            Value::U64(v) => visitor.visit_u64(v),
            Value::I128(v) => visitor.visit_i128(v),
            Value::U128(v) => visitor.visit_u128(v),
            Value::F64(v) => visitor.visit_f64(v),
            Value::String(v, _) => visitor.visit_str(&v),
            Value::Bytes(v) => visitor.visit_bytes(&v),
            Value::Undefined | Value::Null => visitor.visit_unit(),
            Value::Array(v) => visitor.visit_seq(de::value::SeqDeserializer::new(
                v.iter().map(|v| ValueDeserializer::from_value(v.clone())),
            )),
            Value::Map(v) => {
                visitor.visit_map(de::value::MapDeserializer::new(v.iter().map(|(k, v)| {
                    (
                        ValueDeserializer::from_value(k.as_value()),
                        ValueDeserializer::from_value(v.clone()),
                    )
                })))
            }
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Undefined | Value::Null => visitor.visit_unit(),
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string unit
        seq bytes byte_buf map unit_struct
        tuple_struct struct tuple ignored_any identifier newtype_struct
    }
}

impl<'de> de::IntoDeserializer<'de, DeserializationFailed> for ValueDeserializer {
    type Deserializer = ValueDeserializer;

    fn into_deserializer(self) -> ValueDeserializer {
        self
    }
}

impl<'de> de::Deserializer<'de> for Value {
    type Error = DeserializationFailed;

    fn deserialize_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        ValueDeserializer::from_value(self).deserialize_any(visitor)
    }

    fn deserialize_option<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        ValueDeserializer::from_value(self).deserialize_option(visitor)
    }

    fn deserialize_newtype_struct<V: de::Visitor<'de>>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        ValueDeserializer::from_value(self).deserialize_newtype_struct(name, visitor)
    }

    fn deserialize_enum<V: de::Visitor<'de>>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        ValueDeserializer::from_value(self).deserialize_enum(name, variants, visitor)
    }

    forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string unit
        seq bytes byte_buf map unit_struct
        tuple_struct struct tuple ignored_any identifier
    }
}

impl<'de, 'v> de::Deserializer<'de> for &'v Value {
    type Error = DeserializationFailed;

    fn deserialize_any<V: de::Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        ValueDeserializer::from_value(self.clone()).deserialize_any(visitor)
    }

    forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string unit
        seq bytes byte_buf map unit_struct
        tuple_struct struct tuple ignored_any identifier
        option enum newtype_struct
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;
    use serde_derive::Serialize;

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct Content {
        text: String,
        num_likes: u64,
        published: bool,
    }

    #[test]
    fn test_deser() {
        let instance = Content {
            text: "hello".to_string(),
            num_likes: 10,
            published: true,
        };
        let val = Value::from_serializable(&instance);
        let out = Content::deserialize(val).unwrap();
        assert_eq!(out, instance)
    }
}
