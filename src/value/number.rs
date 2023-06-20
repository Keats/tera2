use crate::errors::{Error, TeraResult};
use crate::value::Value;

/// Simpler representation of numbers
/// So operations are simpler to handle
pub(crate) enum Number {
    Integer(i128),
    Float(f64),
}

impl Number {
    pub fn is_float(&self) -> bool {
        matches!(self, Number::Float(..))
    }

    pub fn into_float(self) -> Self {
        match self {
            Number::Float(f) => Number::Float(f),
            Number::Integer(f) => Number::Float(f as f64),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            Number::Float(f) => *f,
            Number::Integer(f) => *f as f64,
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Number::Float(f) => f.is_finite() && f == &0.0,
            Number::Integer(f) => f == &0i128,
        }
    }
}

macro_rules! math {
    ($name:ident, $op_fn:ident, $sign:tt) => {
        pub(crate) fn $name(lhs: &Value, rhs: &Value) -> TeraResult<Value> {
            match (lhs.as_number(), rhs.as_number()) {
                (Some(mut left), Some(mut right)) => {
                    if left.is_float() || right.is_float() {
                        left = left.into_float();
                        right = right.into_float();
                    }

                    let val = match (left, right) {
                        (Number::Integer(a), Number::Integer(b)) => match a.$op_fn(b) {
                            Some(val) => Value::from(val),
                            None => return Err(Error::message(format!("Unable to perform {lhs} {} {rhs}", stringify!($sign)))),
                        },
                        (Number::Float(a), Number::Float(b)) => Value::from(a $sign b),
                        _ => unreachable!(),
                    };
                    Ok(val)
                }
                _ => Err(Error::message(format!(
                    "{lhs} and {rhs} need to both be numbers"
                ))),
            }
        }
    }
}

math!(add, checked_add, +);
math!(sub, checked_sub, -);
math!(mul, checked_mul, *);
math!(rem, checked_rem_euclid, %);

pub(crate) fn div(lhs: &Value, rhs: &Value) -> TeraResult<Value> {
    match (lhs.as_number(), rhs.as_number()) {
        (Some(left), Some(right)) => {
            if right.is_zero() {
                return Err(Error::message("Cannot divide by 0".to_string()));
            }

            Ok((left.as_float() / right.as_float()).into())
        }
        _ => Err(Error::message(format!(
            "{lhs} and {rhs} need to both be numbers"
        ))),
    }
}

pub(crate) fn floor_div(lhs: &Value, rhs: &Value) -> TeraResult<Value> {
    match (lhs.as_number(), rhs.as_number()) {
        (Some(mut left), Some(mut right)) => {
            if right.is_zero() {
                return Err(Error::message("Cannot divide by 0".to_string()));
            }

            if left.is_float() || right.is_float() {
                left = left.into_float();
                right = right.into_float();
            }

            let val = match (left, right) {
                (Number::Integer(a), Number::Integer(b)) => match a.checked_div_euclid(b) {
                    Some(val) => Value::from(val),
                    None => {
                        return Err(Error::message(format!("Unable to perform {lhs} // {rhs}")))
                    }
                },
                (Number::Float(a), Number::Float(b)) => Value::from(a.div_euclid(b)),
                _ => unreachable!(),
            };
            Ok(val)
        }
        _ => Err(Error::message(format!(
            "{lhs} and {rhs} need to both be numbers"
        ))),
    }
}

pub(crate) fn pow(lhs: &Value, rhs: &Value) -> TeraResult<Value> {
    match (lhs.as_number(), rhs.as_number()) {
        (Some(mut left), Some(mut right)) => {
            if left.is_float() || right.is_float() {
                left = left.into_float();
                right = right.into_float();
            }

            let val = match (left, right) {
                // TODO: check that the exponent can fit in a u32 and error otherwise?
                (Number::Integer(a), Number::Integer(b)) => match a.checked_pow(b as u32) {
                    Some(val) => Value::from(val),
                    None => {
                        return Err(Error::message(format!("Unable to perform {lhs} ** {rhs}")))
                    }
                },
                (Number::Float(a), Number::Float(b)) => Value::from(a.powf(b)),
                _ => unreachable!(),
            };
            Ok(val)
        }
        _ => Err(Error::message(format!(
            "{lhs} and {rhs} need to both be numbers"
        ))),
    }
}

pub(crate) fn negate(val: &Value) -> TeraResult<Value> {
    if let Some(num) = val.as_number() {
        let val = match num {
            Number::Float(f) => Value::from(-f),
            Number::Integer(f) => Value::from(-f),
        };
        Ok(val)
    } else {
        Err(Error::message(format!(
            "Only numbers can be negated: {:?}",
            val
        )))
    }
}
