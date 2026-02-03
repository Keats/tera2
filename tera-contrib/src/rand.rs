use rand::Rng;
use tera::{Kwargs, State, TeraResult};

/// Get a random integer between the `start` (inclusive) and `end` (exclusive) parameters.
///
/// ```text
/// {{ get_random(start=1, end=100) }}
/// ```
pub fn get_random(kwargs: Kwargs, _: &State) -> TeraResult<i64> {
    let start = kwargs.must_get::<i64>("start")?;
    let end = kwargs.must_get::<i64>("end")?;
    Ok(rand::rng().random_range(start..end))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use tera::value::Map;
    use tera::{Context, Kwargs, State};

    #[test]
    fn test_get_random() {
        let ctx = Context::new();
        let state = State::new(&ctx);
        let mut map = Map::new();
        map.insert("start".into(), 0.into());
        map.insert("end".into(), 10.into());
        let kwargs = Kwargs::new(Arc::new(map));
        let result = get_random(kwargs, &state).unwrap();
        assert!((0..10).contains(&result));
    }

    #[test]
    fn test_register() {
        let mut tera = tera::Tera::default();
        tera.register_function("get_random", get_random);
    }
}
