#![allow(dead_code)]

// mod context;
mod context;
mod errors;
mod parsing;
mod template;
mod tera;
mod utils;
pub mod value;
mod vm;

pub use parsing::parser::Parser;
pub use utils::escape_html;

#[cfg(test)]
mod tests;
