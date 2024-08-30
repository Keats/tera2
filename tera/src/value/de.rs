use crate::value::utils::DeserializationFailed;
use crate::Value;
use serde::de::{self, Unexpected, Visitor};
use serde::{forward_to_deserialize_any, Deserializer};

#[derive(Debug)]
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
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let (variant, params) = match self.value {
            Value::Map(m) => {
                if let Some((k, v)) = m.iter().next() {
                    (k.as_value(), Some(v.clone()))
                } else {
                    return Err(de::Error::invalid_value(
                        Unexpected::Map,
                        &"map without an entry",
                    ));
                }
            }
            Value::String(_, _) => (self.value.clone(), None),
            _ => {
                return Err(de::Error::invalid_type(
                    Unexpected::Other(self.value.name()),
                    &"map or string",
                ));
            }
        };
        visitor.visit_enum(EnumDeserializer { variant, params })
    }

    forward_to_deserialize_any! {
        bool u8 u16 u32 u64 i8 i16 i32 i64 f32 f64 char str string unit
        seq bytes byte_buf map unit_struct
        tuple_struct struct tuple ignored_any identifier newtype_struct
    }
}

struct EnumDeserializer {
    variant: Value,
    params: Option<Value>,
}

impl<'de> de::EnumAccess<'de> for EnumDeserializer {
    type Error = DeserializationFailed;
    type Variant = VariantDeserializer;

    fn variant_seed<V: de::DeserializeSeed<'de>>(
        self,
        seed: V,
    ) -> Result<(V::Value, Self::Variant), Self::Error> {
        let value = seed.deserialize(self.variant)?;
        Ok((
            value,
            VariantDeserializer {
                params: self.params,
            },
        ))
    }
}

struct VariantDeserializer {
    params: Option<Value>,
}
impl<'de> de::VariantAccess<'de> for VariantDeserializer {
    type Error = DeserializationFailed;

    fn unit_variant(self) -> Result<(), Self::Error> {
        match self.params {
            Some(value) => de::Deserialize::deserialize(value),
            None => Ok(()),
        }
    }

    fn newtype_variant_seed<T: de::DeserializeSeed<'de>>(
        self,
        seed: T,
    ) -> Result<T::Value, Self::Error> {
        match self.params {
            Some(value) => seed.deserialize(value),
            None => Err(de::Error::invalid_type(
                Unexpected::UnitVariant,
                &"newtype variant",
            )),
        }
    }

    fn tuple_variant<V: Visitor<'de>>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        match self.params {
            Some(Value::Array(m)) => {
                ValueDeserializer::from_value(Value::Array(m)).deserialize_any(visitor)
            }
            _ => Err(de::Error::invalid_type(
                Unexpected::UnitVariant,
                &"tuple variant",
            )),
        }
    }

    fn struct_variant<V: Visitor<'de>>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        match self.params {
            Some(Value::Map(m)) => {
                ValueDeserializer::from_value(Value::Map(m)).deserialize_any(visitor)
            }
            _ => Err(de::Error::invalid_type(
                Unexpected::UnitVariant,
                &"struct variant",
            )),
        }
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
    enum Kind {
        Article,
        Comment(String),
        Tuple(usize, usize),
        Other { truthy: bool },
    }

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct Content {
        text: String,
        num_likes: u64,
        published: bool,
        kind: Kind,
        kind2: Kind,
        kind3: Kind,
        kind4: Kind,
    }

    #[test]
    fn test_deser() {
        let instance = Content {
            text: "hello".to_string(),
            num_likes: 10,
            published: true,
            kind: Kind::Article,
            kind2: Kind::Comment(String::new()),
            kind3: Kind::Tuple(1, 1),
            kind4: Kind::Other { truthy: true },
        };
        let val = Value::from_serializable(&instance);
        let out = Content::deserialize(val).unwrap();
        assert_eq!(out, instance)
    }
}
