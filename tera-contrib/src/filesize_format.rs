use tera::{Kwargs, State, Value};

/// Formats a number of bytes into a human-readable file size string.
/// Uses binary units (KiB, MiB, GiB, etc.) by default.
pub fn filesizeformat(val: Value, kwargs: Kwargs, _: &State) -> String {
    let bytes = match val.as_number() {
        Some(n) => n.as_float() as u64,
        None => return val.to_string(),
    };

    let binary = kwargs.get::<bool>("binary").ok().flatten().unwrap_or(true);

    if binary {
        humansize::format_size(bytes, humansize::BINARY)
    } else {
        humansize::format_size(bytes, humansize::DECIMAL)
    }
}
