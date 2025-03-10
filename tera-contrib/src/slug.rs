use slug::slugify;
use tera::{Kwargs, State};

pub fn slug(val: &str, _: Kwargs, _: &State) -> String {
    slugify(val)
}
