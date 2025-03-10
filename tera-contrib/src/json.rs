use tera::{Kwargs, State, TeraResult};

/// Percent-encodes reserved URI characters
pub fn json_encode(val: &str, kwargs: Kwargs, _: &State) -> TeraResult<String> {
    let pretty = kwargs.get::<bool>("pretty")?.unwrap_or(false);
    let res = if pretty {
        serde_json::to_string_pretty(val)
    } else {
        serde_json::to_string(val).map_err(Into::into)
    };
    res.map_err(|e| tera::Error::message(format!("Error encoding to JSON: {e}")))
}
