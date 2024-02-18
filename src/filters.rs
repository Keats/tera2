use std::sync::Arc;

use crate::errors::TeraResult;
use crate::value::kwargs::{ArgFromValue, Kwargs};
use crate::value::FunctionResult;
use crate::Value;

/// The filter function type definition
pub trait Filter<Arg, Res>: Sync + Send + 'static {
    /// The filter function type definition
    fn call(&self, value: Arg, kwargs: Kwargs) -> Res;

    /// Whether the current filter's output should be treated as safe, defaults to `false`
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

#[derive(Clone)]
pub(crate) struct StoredFilter(Arc<FilterFunc>);

impl StoredFilter {
    pub fn new<Func, Arg, Res>(f: Func) -> Self
    where
        Func: Filter<Arg, Res>,
        Arg: for<'a> ArgFromValue<'a>,
        Res: FunctionResult,
    {
        let closure = move |arg: &Value, kwargs| -> TeraResult<Value> {
            f.call(ArgFromValue::from_value(Some(arg))?, kwargs)
                .into_result()
        };

        StoredFilter(Arc::new(closure))
    }

    pub fn call(&self, arg: &Value, kwargs: Kwargs) -> TeraResult<Value> {
        (self.0)(arg, kwargs)
    }
}

fn some_filter(value: u8, _kwargs: Kwargs) -> u8 {
    value
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    struct Tester {
        filters: Vec<StoredFilter>,
    }
    impl Tester {
        fn add_filter<Func, Arg, Res>(&mut self, f: Func)
        where
            Func: Filter<Arg, Res>,
            Arg: for<'a> ArgFromValue<'a>,
            Res: FunctionResult,
        {
            self.filters.push(StoredFilter::new(f));
        }
    }

    #[test]
    fn test_def() {
        let mut tester = Tester { filters: vec![] };
        tester.add_filter(some_filter);
    }
}
