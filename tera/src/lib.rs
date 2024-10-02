mod args;
mod context;
mod errors;
mod filters;
mod functions;
#[cfg(feature = "glob_fs")]
mod globbing;
mod parsing;
mod reporting;
mod template;
mod tera;
mod tests;
mod utils;
pub mod value;
pub(crate) mod vm;

pub use crate::tera::{EscapeFn, Tera};
pub use args::Kwargs;
pub use context::Context;
pub use errors::{Error, ErrorKind, TeraResult};
pub use parsing::parser::Parser;
pub use utils::escape_html;
pub use value::number::Number;
pub use value::Value;
pub use vm::state::State;

#[cfg(feature = "ahash")]
pub(crate) use hashbrown::{HashMap, HashSet};
#[cfg(not(feature = "ahash"))]
pub(crate) use std::collections::{HashMap, HashSet};

#[cfg(test)]
mod snapshot_tests;
