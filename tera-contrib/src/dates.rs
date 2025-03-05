use jiff::tz::TimeZone;
use jiff::Zoned;
use tera::{Kwargs, State, TeraResult, Value};

pub(crate) fn now(kwargs: Kwargs, _: &State) -> TeraResult<Value> {
    let utc = kwargs.get::<bool>("utc")?.unwrap_or(false);
    let as_timestamp = kwargs.get::<bool>("timestamp")?.unwrap_or(false);

    let mut now = Zoned::now();
    if utc {
        now = now.with_time_zone(TimeZone::UTC);
    }

    if as_timestamp {
        Ok(Value::from(now.timestamp().as_second()))
    } else {
        Ok(Value::from(now.to_string()))
    }
}
