use std::sync::Arc;

use crate::args::{ArgFromValue, Kwargs};
use crate::errors::TeraResult;
use crate::value::FunctionResult;
use crate::Value;

/// The filter function type definition
pub trait Filter<Arg, Res>: Sync + Send + 'static {
    /// The filter function type definition
    fn call(&self, value: Arg, kwargs: Kwargs) -> Res;

    /// Whether the current filter's output should be treated as safe, defaults to `false`
    /// Only needs to be defined if the filter returns a string
    fn is_safe(&self) -> bool {
        false
    }
}

impl<Func, Arg, Res> Filter<Arg, Res> for Func
where
    Func: Fn(Arg, Kwargs) -> Res + Sync + Send + 'static,
    Arg: for<'a> ArgFromValue<'a>,
    Res: FunctionResult,
{
    fn call(&self, value: Arg, kwargs: Kwargs) -> Res {
        (self)(value, kwargs)
    }
}

type FilterFunc = dyn Fn(&Value, Kwargs) -> TeraResult<Value> + Sync + Send + 'static;

pub(crate) struct StoredFilter(Arc<FilterFunc>);

impl StoredFilter {
    pub fn new<Func, Arg, Res>(f: Func) -> Self
    where
        Func: Filter<Arg, Res> + for<'a> Filter<<Arg as ArgFromValue<'a>>::Output, Res>,
        Arg: for<'a> ArgFromValue<'a>,
        Res: FunctionResult,
    {
        let closure = move |arg: &Value, kwargs| -> TeraResult<Value> {
            f.call(Arg::from_value(arg)?, kwargs).into_result()
        };

        StoredFilter(Arc::new(closure))
    }

    pub fn call(&self, arg: &Value, kwargs: Kwargs) -> TeraResult<Value> {
        (self.0)(arg, kwargs)
    }
}

fn some_filter(value: u64, _kwargs: Kwargs) -> u64 {
    value
}

fn another_filter(_value: &str, _kwargs: Kwargs) -> &'static str {
    "hello world"
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    struct Tester {
        filters: Vec<StoredFilter>,
    }

    impl Tester {
        pub fn add_filter<Func, Arg, Res>(&mut self, f: Func)
        where
            Func: Filter<Arg, Res> + for<'a> Filter<<Arg as ArgFromValue<'a>>::Output, Res>,
            Arg: for<'a> ArgFromValue<'a>,
            Res: FunctionResult,
        {
            self.filters.push(StoredFilter::new(f));
        }

        pub fn call(&self, arg: &Value, kwargs: Kwargs) -> TeraResult<Value> {
            self.filters[0].call(arg, kwargs)
        }
    }

    #[test]
    fn test_def() {
        let mut tester = Tester { filters: vec![] };
        tester.add_filter(another_filter);
        tester.add_filter(some_filter);
        let res = tester.call(&Value::from("hello"), Kwargs::default());
        println!("{res:?}");
    }
}
