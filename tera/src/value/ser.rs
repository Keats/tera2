use std::sync::Arc;

use crate::value::key::Key;
use serde::{ser, Serialize, Serializer};

use crate::value::utils::SerializationFailed;
use crate::value::{Map, SmartString, StringKind, Value, ValueInner};

pub struct ValueSerializer;

impl Serializer for ValueSerializer {
    type Ok = Value;
    type Error = SerializationFailed;

    type SerializeSeq = SerializeSeq;
    type SerializeTuple = SerializeSeq;
    type SerializeTupleStruct = SerializeSeq;
    type SerializeTupleVariant = SerializeTupleVariant;
    type SerializeMap = SerializeMap;
    type SerializeStruct = SerializeStruct;
    type SerializeStructVariant = SerializeStructVariant;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::Bool(v).into())
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::I64(v as i64).into())
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::I64(v as i64).into())
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::I64(v as i64).into())
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::I64(v).into())
    }

    fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::I128(Box::new(v)).into())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::U64(v as u64).into())
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::U64(v as u64).into())
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::U64(v as u64).into())
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::U64(v).into())
    }

    fn serialize_u128(self, v: u128) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::U128(Box::new(v)).into())
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::F64(v as f64).into())
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::F64(v).into())
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::String(SmartString::new(&v.to_string(), StringKind::Normal)).into())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::String(SmartString::new(v, StringKind::Normal)).into())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::Bytes(Arc::new(v.to_vec())).into())
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::Null.into())
    }

    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::Null.into())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::String(SmartString::new(variant, StringKind::Normal)).into())
    }

    fn serialize_newtype_struct<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error> {
        let mut map = Map::with_capacity(1);
        map.insert(Key::Str(variant), value.serialize(self)?);
        Ok(ValueInner::Map(Arc::new(map)).into())
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(SerializeSeq {
            elements: Vec::with_capacity(len.unwrap_or(0)),
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Ok(SerializeTupleVariant {
            name: variant,
            fields: Vec::with_capacity(len),
        })
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(SerializeMap {
            entries: Map::with_capacity(len.unwrap_or(0)),
            key: None,
        })
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(SerializeStruct {
            fields: Map::with_capacity(len),
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Ok(SerializeStructVariant {
            variant,
            fields: Map::with_capacity(len),
        })
    }
}

pub struct SerializeSeq {
    elements: Vec<Value>,
}

impl ser::SerializeSeq for SerializeSeq {
    type Ok = Value;
    type Error = SerializationFailed;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.elements.push(value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::Array(Arc::new(self.elements)).into())
    }
}

impl ser::SerializeTuple for SerializeSeq {
    type Ok = Value;
    type Error = SerializationFailed;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.elements.push(value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::Array(Arc::new(self.elements)).into())
    }
}

impl ser::SerializeTupleStruct for SerializeSeq {
    type Ok = Value;
    type Error = SerializationFailed;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.elements.push(value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::Array(Arc::new(self.elements)).into())
    }
}

pub struct SerializeTupleVariant {
    name: &'static str,
    fields: Vec<Value>,
}

impl ser::SerializeTupleVariant for SerializeTupleVariant {
    type Ok = Value;
    type Error = SerializationFailed;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.fields.push(value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let mut map = Map::with_capacity(1);
        map.insert(
            Key::Str(self.name),
            ValueInner::Array(Arc::new(self.fields)).into(),
        );
        Ok(ValueInner::Map(Arc::new(map)).into())
    }
}

pub struct SerializeMap {
    entries: Map,
    key: Option<Value>,
}

impl ser::SerializeMap for SerializeMap {
    type Ok = Value;
    type Error = SerializationFailed;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Self::Error> {
        let key = key.serialize(ValueSerializer)?;
        self.key = Some(key);
        Ok(())
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        let key = self.key.take().expect("missing key");
        let s = key.as_str().expect("key is not string");
        let value = value.serialize(ValueSerializer)?;
        self.entries.insert(Key::String(Arc::from(s)), value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::Map(Arc::new(self.entries)).into())
    }
}

pub struct SerializeStruct {
    fields: Map,
}

impl ser::SerializeStruct for SerializeStruct {
    type Ok = Value;
    type Error = SerializationFailed;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        let value = value.serialize(ValueSerializer)?;
        self.fields.insert(Key::Str(key), value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(ValueInner::Map(Arc::new(self.fields)).into())
    }
}

pub struct SerializeStructVariant {
    variant: &'static str,
    fields: Map,
}

impl ser::SerializeStructVariant for SerializeStructVariant {
    type Ok = Value;
    type Error = SerializationFailed;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        let value = value.serialize(ValueSerializer)?;
        self.fields.insert(Key::Str(key), value);
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        let mut map = Map::with_capacity(1);
        map.insert(
            Key::Str(self.variant),
            ValueInner::Map(Arc::new(self.fields)).into(),
        );
        Ok(ValueInner::Map(Arc::new(map)).into())
    }
}
