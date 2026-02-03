use tera::{Kwargs, State, TeraResult, Value};

/// Encodes a value as JSON.
/// Takes an optional `pretty` boolean argument defaulting to false.
///
/// ```text
/// {{ value | json_encode }}
/// {{ value | json_encode(pretty=true) }}
/// ```
pub fn json_encode(val: &Value, kwargs: Kwargs, _: &State) -> TeraResult<String> {
    let pretty = kwargs.get::<bool>("pretty")?.unwrap_or(false);
    let res = if pretty {
        serde_json::to_string_pretty(val)
    } else {
        serde_json::to_string(val)
    };
    res.map_err(|e| tera::Error::message(format!("Error encoding to JSON: {e}")))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use tera::value::Map;
    use tera::{Context, Kwargs, State, Value};

    #[test]
    fn test_json_encode() {
        let ctx = Context::new();
        let state = State::new(&ctx);
        let mut obj = Map::new();
        obj.insert("a".into(), Value::from(1));
        let val = Value::from(obj);
        assert_eq!(
            json_encode(&val, Kwargs::default(), &state).unwrap(),
            r#"{"a":1}"#
        );
    }

    #[test]
    fn test_json_encode_pretty() {
        let ctx = Context::new();
        let state = State::new(&ctx);
        let mut kwargs_map = Map::new();
        kwargs_map.insert("pretty".into(), true.into());
        let kwargs = Kwargs::new(Arc::new(kwargs_map));
        let mut obj = Map::new();
        obj.insert("a".into(), Value::from(1));
        let val = Value::from(obj);
        assert_eq!(
            json_encode(&val, kwargs, &state).unwrap(),
            "{\n  \"a\": 1\n}"
        );
    }

    #[test]
    fn test_register() {
        let mut tera = tera::Tera::default();
        tera.register_filter("json_encode", json_encode);
    }
}
