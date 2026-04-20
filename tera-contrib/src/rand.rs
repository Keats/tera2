use std::hash::{DefaultHasher, Hash, Hasher};

use rand::prelude::*;
use tera::{Kwargs, State, TeraResult, Value};

/// Get a random integer between the `start` (inclusive) and `end` (exclusive) parameters.
/// Optionally takes a seed.
///
/// ```text
/// {{ get_random(start=1, end=100) }}
/// ```
pub fn get_random(kwargs: Kwargs, _: &State) -> TeraResult<i64> {
    let start = kwargs.must_get::<i64>("start")?;
    let end = kwargs.must_get::<i64>("end")?;

    match kwargs.get::<String>("seed")? {
        Some(seed) => {
            let mut h = DefaultHasher::new();
            seed.hash(&mut h);
            let mut rng = rand::rngs::StdRng::seed_from_u64(h.finish());
            Ok(rng.random_range(start..end))
        }
        None => Ok(rand::rng().random_range(start..end))
    }
}

/// Shuffles a list, optionally taking a seed for reproducible shuffling.
///
/// ```text
/// {{ list | shuffle }}
/// ```
pub fn shuffle(mut val: Vec<Value>, kwargs: Kwargs, _: &State) -> TeraResult<Vec<Value>> {
    if val.is_empty() {
        return Ok(val);
    }

    match kwargs.get::<String>("seed")? {
        Some(seed) => {
            let mut h = DefaultHasher::new();
            seed.hash(&mut h);
            let mut rng = rand::rngs::StdRng::seed_from_u64(h.finish());
            val.shuffle(&mut rng);
        }
        None => val.shuffle(&mut rand::rng()),
    }

    Ok(val)
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
        map.insert("seed".into(), 42.into());
        let kwargs = Kwargs::new(Arc::new(map));
        let result = get_random(kwargs, &state).unwrap();
        assert!((0..10).contains(&result));
        assert_eq!(result, 9);
    }

    #[test]
    fn test_shuffle() {
        let ctx = Context::new();
        let state = State::new(&ctx);
        let mut map = Map::new();
        map.insert("seed".into(), 42.into());
        let kwargs = Kwargs::new(Arc::new(map));
        let input: Vec<Value> = (0..10i64).map(Value::from).collect();
        let result = shuffle(input.clone(), kwargs, &state).unwrap();
        assert_eq!(result.len(), input.len());
        assert_ne!(result, input);
    }

    #[test]
    fn test_register() {
        let mut tera = tera::Tera::default();
        tera.register_function("get_random", get_random);
        tera.register_filter("shuffle", shuffle);
    }
}
