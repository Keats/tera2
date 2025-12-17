use crate::errors::{Error, TeraResult};

/// This allows customizing the delimiters used for blocks, variables, and comments in case
/// you want to template files that contains text like `{{`, like LaTeX.
/// Delimiters need to be 2 characters.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Delimiters {
    /// Start delimiter for blocks, default: `{%`
    pub block_start: &'static str,
    /// End delimiter for blocks, default: `%}`
    pub block_end: &'static str,
    /// Start delimiter for variables, default: `{{`
    pub variable_start: &'static str,
    /// End delimiter for variables, default: `}}`
    pub variable_end: &'static str,
    /// Start delimiter for comments, default: `{#`
    pub comment_start: &'static str,
    /// End delimiter for comments, default: `#}`
    pub comment_end: &'static str,
}

impl Default for Delimiters {
    fn default() -> Self {
        Self {
            block_start: "{%",
            block_end: "%}",
            variable_start: "{{",
            variable_end: "}}",
            comment_start: "{#",
            comment_end: "#}",
        }
    }
}

impl Delimiters {
    /// Returns an error if any delimiter is empty or if there are conflicts
    pub(crate) fn validate(&self) -> TeraResult<()> {
        if self.block_start.len() != 2 {
            return Err(Error::message(
                "`block_start` delimiter must be 2 characters",
            ));
        }
        if self.block_end.len() != 2 {
            return Err(Error::message("`block_end` delimiter must be 2 characters"));
        }
        if self.variable_start.len() != 2 {
            return Err(Error::message(
                "`variable_start` delimiter must be 2 characters",
            ));
        }
        if self.variable_end.len() != 2 {
            return Err(Error::message(
                "`variable_end` delimiter must be 2 characters",
            ));
        }
        if self.comment_start.len() != 2 {
            return Err(Error::message(
                "`comment_start` delimiter must be 2 characters",
            ));
        }
        if self.comment_end.len() != 2 {
            return Err(Error::message(
                "`comment_end` delimiter must be 2 characters",
            ));
        }

        // Check for conflicting start delimiters
        if self.block_start == self.variable_start {
            return Err(Error::message(
                "`block_start` and `variable_start` cannot have the same value",
            ));
        }
        if self.block_start == self.comment_start {
            return Err(Error::message(
                "`block_start` and `comment_start` cannot have the same value",
            ));
        }
        if self.variable_start == self.comment_start {
            return Err(Error::message(
                "`variable_start` and `comment_star`t cannot have the same value",
            ));
        }

        Ok(())
    }
}
