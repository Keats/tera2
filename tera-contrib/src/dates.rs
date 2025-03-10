use std::borrow::Cow;
use std::str::FromStr;

use jiff::civil::Date;
use jiff::fmt::temporal::DateTimeParser;
use jiff::tz::TimeZone;
use jiff::{fmt::rfc2822, fmt::strtime, Timestamp, Zoned};
use tera::{Kwargs, State, TeraResult, Value};

static PARSER: DateTimeParser = DateTimeParser::new();

pub fn now(kwargs: Kwargs, _: &State) -> TeraResult<Value> {
    let utc = kwargs.get::<bool>("utc")?.unwrap_or(false);
    let as_timestamp = kwargs.get::<bool>("timestamp")?.unwrap_or(false);

    let mut now = Zoned::now();
    if utc {
        now = now.with_time_zone(TimeZone::UTC);
    }

    if as_timestamp {
        Ok(Value::from(now.timestamp().as_second()))
    } else {
        Ok(Value::from(now.timestamp().to_string()))
    }
}

pub fn date(val: &Value, kwargs: Kwargs, _: &State) -> TeraResult<String> {
    let format = kwargs.get::<&str>("format")?.unwrap_or("%Y-%m-%d");
    let timezone = match kwargs.get::<&str>("timezone")? {
        Some(t) => match TimeZone::get(t) {
            Ok(tz) => Some(tz),
            Err(_) => return Err(tera::Error::message(format!("Unknown timezone: {t}"))),
        },
        None => None,
    };

    let zoned = match val {
        Value::String(s, _) => {
            let res = PARSER
                .parse_timestamp(&**s)
                .map(|t| t.to_zoned(timezone.clone().unwrap_or(TimeZone::UTC)))
                .or_else(|_| PARSER.parse_zoned(&**s))
                .or_else(|_| {
                    PARSER
                        .parse_datetime(&**s)
                        .and_then(|d| d.to_zoned(timezone.clone().unwrap_or(TimeZone::UTC)))
                })
                .or_else(|_| {
                    PARSER
                        .parse_date(&**s)
                        .and_then(|d| d.to_zoned(timezone.clone().unwrap_or(TimeZone::UTC)))
                });

            let mut zoned = match res {
                Ok(z) => z,
                Err(e) => {
                    return Err(tera::Error::message(format!(
                        "The string {s} cannot be parsed as a RFC3339 or RFC2822: {e}"
                    )))
                }
            };
            if let Some(tz) = timezone {
                zoned = zoned.with_time_zone(tz);
            }
            zoned
        }
        Value::I64(ts) => {
            let time = match Timestamp::new(*ts, 0) {
                Ok(t) => t,
                Err(e) => return Err(tera::Error::message(format!("Invalid timestamp: {e}"))),
            };
            time.to_zoned(timezone.unwrap_or(TimeZone::UTC))
        }
        _ => {
            return Err(tera::Error::message(format!(
                "Invalid value: the date filter can only be used on strings or i64, this is a {}",
                val.name()
            )))
        }
    };

    jiff::fmt::strtime::format(format, &zoned)
        .map_err(|e| tera::Error::message(format!("Invalid date format `{}`: {e}", format,)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use tera::value::Map;
    use tera::{Context, Kwargs, State};

    // copy from https://github.com/Keats/tera/blob/master/src/builtins/filters/common.rs#L326
    #[test]
    fn test_ok_date() {
        let inputs = vec![
            (Value::from(1482720453), None, None, "2016-12-26"),
            (
                Value::from(1482720453),
                Some("%Y-%m-%d %H:%M"),
                None,
                "2016-12-26 02:47",
            ),
            // RFC3339
            (
                Value::from("1985-04-12T23:20:50.52Z"),
                None,
                None,
                "1985-04-12",
            ),
            // timezones are preserved
            (
                Value::from("1996-12-19T16:39:57[-08:00]"),
                Some("%Y-%m-%d %z"),
                None,
                "1996-12-19 -0800",
            ),
            // simple date
            (
                Value::from("2017-03-05"),
                Some("%a, %d %b %Y %H:%M:%S %z"),
                None,
                "Sun, 05 Mar 2017 00:00:00 +0000",
            ),
            // naive datetime
            (
                Value::from("2017-03-05T00:00:00.602"),
                Some("%a, %d %b %Y %H:%M:%S"),
                None,
                "Sun, 05 Mar 2017 00:00:00",
            ),
            // with a timezone
            (
                Value::from("2019-09-19T01:48:44.581Z"),
                None,
                Some("America/New_York"),
                "2019-09-18",
            ),
            (
                Value::from(1648252203),
                None,
                Some("Europe/Berlin"),
                "2022-03-26",
            ),
        ];

        for (value, format, timezone, expected) in inputs {
            let mut map = Map::new();
            if let Some(f) = format {
                map.insert("format".into(), f.into());
            }
            if let Some(tz) = timezone {
                map.insert("timezone".into(), tz.into());
            }
            let kwargs = Kwargs::new(Arc::new(map));
            let ctx = Context::new();
            let res = date(&value, kwargs, &State::new(&ctx)).unwrap();
            assert_eq!(expected, res);
        }
    }

    #[test]
    fn test_bad_date_call() {
        let inputs = vec![
            (Value::from(1482720453), Some("%1"), None),
            (Value::from(1482720453), Some("%+S"), None),
            (
                Value::from("2019-09-19T01:48:44.581Z"),
                Some("%+S"),
                Some("Narnia"),
            ),
        ];

        for (value, format, timezone) in inputs {
            let mut map = Map::new();
            if let Some(f) = format {
                map.insert("format".into(), f.into());
            }
            if let Some(tz) = timezone {
                map.insert("timezone".into(), tz.into());
            }
            let kwargs = Kwargs::new(Arc::new(map));
            let ctx = Context::new();
            let res = date(&value, kwargs, &State::new(&ctx));
            println!("{res:?}");
            assert!(res.is_err());
        }
    }
}
