/// Whether to remove the whitespace of a `{% %}` tag
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Ws {
    /// `true` if the tag is `{%-`, `{{-`, `{#-`
    pub left: bool,
    /// `true` if the tag is `-%}`, `-}}`, `-#}`
    pub right: bool,
}

impl Default for Ws {
    fn default() -> Self {
        Ws { left: false, right: false }
    }
}