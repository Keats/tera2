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
