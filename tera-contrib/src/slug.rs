use slug::slugify;
use tera::{Kwargs, State};

pub fn slug(val: &str, _: Kwargs, _: &State) -> String {
    slugify(val)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tera::{Context, Kwargs, State};

    #[test]
    fn test_slug() {
        let ctx = Context::new();
        let state = State::new(&ctx);
        assert_eq!(
            slug("Hello World", Kwargs::default(), &state),
            "hello-world"
        );
        assert_eq!(slug("Foo & Bar!", Kwargs::default(), &state), "foo-bar");
    }
}
