use crate::args::Kwargs;
use crate::errors::{Error, TeraResult};
use crate::value::FunctionResult;
use crate::vm::state::State;
use crate::Value;
use std::sync::Arc;

/// The function function type definition
pub trait Function<Res>: Sync + Send + 'static {
    /// The function type definition
    fn call(&self, kwargs: Kwargs, state: &State) -> Res;

    /// Whether the current function's output should be treated as safe, defaults to `false`
    /// Only needs to be defined if the filter returns a string
    fn is_safe(&self) -> bool {
        false
    }
}

impl<Func, Res> Function<Res> for Func
where
    Func: Fn(Kwargs, &State) -> Res + Sync + Send + 'static,
    Res: FunctionResult,
{
    fn call(&self, kwargs: Kwargs, state: &State) -> Res {
        (self)(kwargs, state)
    }
}

type FunctionFunc = dyn Fn(Kwargs, &State) -> TeraResult<Value> + Sync + Send + 'static;

#[derive(Clone)]
pub(crate) struct StoredFunction(Arc<FunctionFunc>);

impl StoredFunction {
    pub fn new<Func, Res>(f: Func) -> Self
    where
        Func: Function<Res>,
        Res: FunctionResult,
    {
        let closure = move |kwargs, state: &State| -> TeraResult<Value> {
            f.call(kwargs, state).into_result()
        };

        StoredFunction(Arc::new(closure))
    }

    pub fn call(&self, kwargs: Kwargs, state: &State) -> TeraResult<Value> {
        (self.0)(kwargs, state)
    }
}

pub(crate) fn range(kwargs: Kwargs, _: &State) -> TeraResult<Vec<isize>> {
    let start = kwargs.get::<isize>("start")?.unwrap_or_default();
    let end = kwargs.must_get::<isize>("end")?;
    let step_by = kwargs.get::<usize>("step_by")?.unwrap_or(1);
    if start > end {
        return Err(Error::message(
            "Function `range` was called with a `start` argument greater than the `end` one",
        ));
    }

    Ok((start..end).step_by(step_by).collect())
}

pub(crate) fn throw(kwargs: Kwargs, _: &State) -> TeraResult<bool> {
    let message = kwargs.must_get::<&str>("message")?;
    Err(Error::message(message))
}
