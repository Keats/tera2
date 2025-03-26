use rand::Rng;
use tera::{Kwargs, State, TeraResult};

pub fn get_random(kwargs: Kwargs, _: &State) -> TeraResult<i64> {
    let start = kwargs.must_get::<i64>("start")?;
    let end = kwargs.must_get::<i64>("end")?;
    Ok(rand::rng().random_range(start..end))
}
