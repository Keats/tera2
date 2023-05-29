mod context;
mod errors;
mod parsing;
mod template;
mod tera;
mod utils;
pub mod value;
mod vm;

pub use context::Context;
pub use parsing::parser::Parser;
pub use crate::tera::{EscapeFn, Tera};
pub use utils::escape_html;

#[cfg(test)]
mod tests;
