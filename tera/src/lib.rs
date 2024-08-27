mod args;
mod context;
mod errors;
mod filters;
mod functions;
mod parsing;
mod reporting;
mod template;
mod tera;
mod tests;
mod utils;
pub mod value;
pub(crate) mod vm;

pub use crate::tera::{EscapeFn, Tera};
pub use context::Context;
pub use parsing::parser::Parser;
pub use utils::escape_html;
pub use value::number::Number;
pub use value::Value;
pub use vm::state::State;

#[cfg(ahash)]
pub(crate) use hashbrown::{HashMap, HashSet};
#[cfg(not(ahash))]
pub(crate) use std::collections::{HashMap, HashSet};

#[cfg(test)]
mod snapshot_tests;
