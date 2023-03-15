use std::collections::HashMap;
use std::sync::Arc;

mod ser;

pub enum Value {
    // TODO: differentiate undefined and None?
    Null,
    Bool(bool),
    U64(u64),
    I64(i64),
    F64(f64),
    U128(u128),
    I128(i128),
    // TODO: do we need char?
    Char(char),
    Array(Arc<Vec<Value>>),
    Bytes(Arc<Vec<u8>>),
    String(Arc<String>),
    // TODO: order preserving feature via indexmap
    // TODO: allow keys other than string?
    // TODO: string interning?
    // TODO: allow numbers as keys?
    Map(Arc<HashMap<String, Value>>),
}
