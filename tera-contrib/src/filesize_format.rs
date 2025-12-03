use tera::{Kwargs, State};

/// Formats a number of bytes into a human-readable file size string.
/// Uses binary units (KiB, MiB, GiB, etc.) by default.
pub fn filesizeformat(val: u64, kwargs: Kwargs, _: &State) -> String {
    let binary = kwargs.get::<bool>("binary").ok().flatten().unwrap_or(true);

    if binary {
        humansize::format_size(val, humansize::BINARY)
    } else {
        humansize::format_size(val, humansize::DECIMAL)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use tera::value::Map;
    use tera::{Context, Kwargs, State};

    #[test]
    fn test_filesizeformat_binary() {
        let ctx = Context::new();
        let state = State::new(&ctx);
        assert_eq!(filesizeformat(1024, Kwargs::default(), &state), "1 KiB");
        assert_eq!(filesizeformat(1048576, Kwargs::default(), &state), "1 MiB");
    }

    #[test]
    fn test_filesizeformat_decimal() {
        let ctx = Context::new();
        let state = State::new(&ctx);
        let mut map = Map::new();
        map.insert("binary".into(), false.into());
        let kwargs = Kwargs::new(Arc::new(map));
        assert_eq!(filesizeformat(1000, kwargs, &state), "1 kB");
    }
}
