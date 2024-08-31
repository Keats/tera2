use std::path::PathBuf;

use globset::{GlobBuilder};
use walkdir::WalkDir;

use crate::errors::{Error, TeraResult};

/// Loads the glob and find all files matching that glob,
/// returning a list of (path, filename)
pub(crate) fn load_from_glob(glob: &str) -> TeraResult<Vec<(PathBuf, String)>> {
    let Some(idx) = glob.find('*') else {
        return Err(Error::message(format!(
            "Not a valid glob: no `*` were found in `{glob}`"
        )));
    };

    let (parent_dir, glob_end) = glob.split_at(idx);

    // If canonicalize fails, just abort it and resume with the given path.
    // Consumers expect invalid globs to just return the empty set instead of failing.
    // See https://github.com/Keats/tera/issues/819#issuecomment-1480392230
    let parent_dir =
        std::fs::canonicalize(parent_dir).unwrap_or_else(|_| std::path::PathBuf::from(parent_dir));

    let canonical_glob = {
        let mut p = parent_dir.clone();
        p.push(glob_end);
        p.to_string_lossy().to_string()
    };

    let glob_matcher = GlobBuilder::new(&canonical_glob)
        .literal_separator(true)
        .build()
        .map_err(|e| Error::message(format!("Glob is invalid: {e}")))?
        .compile_matcher();

    let mut paths = Vec::new();
    for entry in WalkDir::new(&parent_dir)
        .follow_links(true)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let mut path = entry.path().to_path_buf();
        if path.is_dir() || !glob_matcher.is_match(&path) {
            continue;
        }

        if path.starts_with("./") {
            path = path.strip_prefix("./").unwrap().to_path_buf();
        }

        let filepath = path
            .strip_prefix(&parent_dir)
            .unwrap()
            .to_string_lossy()
            // unify on forward slash
            .replace('\\', "/");

        paths.push((path, filepath));
    }

    Ok(paths)
}

#[cfg(test)]
mod tests {
    use super::load_from_glob;
    use std::fs::File;
    use tempfile::tempdir;

    #[test]
    fn can_load_from_glob() {
        let data = load_from_glob("examples/basic/templates/**/*").unwrap();
        assert_eq!(data.len(), 3);
        assert!(data.iter().find(|(_, y)| y == "base.html").is_some());
        assert!(data.iter().find(|(_, y)| y == "users/profile.html").is_some());
    }

    #[test]
    fn can_load_from_glob_with_patterns() {
        let data = load_from_glob("examples/basic/templates/**/*.{html,xml}").unwrap();
        assert_eq!(data.len(), 3);
        assert!(data.iter().find(|(_, y)| y == "base.html").is_some());
        assert!(data.iter().find(|(_, y)| y == "users/profile.html").is_some());
    }

    // https://github.com/Keats/tera/issues/380
    #[test]
    fn glob_work_with_absolute_paths() {
        let tmp_dir = tempdir().expect("create temp dir");
        let cwd = tmp_dir.path().canonicalize().unwrap();
        File::create(cwd.join("hey.html")).expect("Failed to create a test file");
        File::create(cwd.join("ho.html")).expect("Failed to create a test file");
        let glob = cwd.join("*.html").into_os_string().into_string().unwrap();
        let data = load_from_glob(&glob).unwrap();
        assert_eq!(data.len(), 2);
    }

    #[test]
    fn glob_work_with_absolute_paths_and_double_star() {
        let tmp_dir = tempdir().expect("create temp dir");
        let cwd = tmp_dir.path().canonicalize().unwrap();
        File::create(cwd.join("hey.html")).expect("Failed to create a test file");
        File::create(cwd.join("ho.html")).expect("Failed to create a test file");
        let glob = cwd
            .join("**")
            .join("*.html")
            .into_os_string()
            .into_string()
            .unwrap();
        let data = load_from_glob(&glob).unwrap();
        assert_eq!(data.len(), 2);
    }

    // Test for https://github.com/Keats/tera/issues/574
    #[test]
    fn glob_work_with_paths_starting_with_dots() {
        use std::path::PathBuf;

        let this_dir = std::env::current_dir()
            .expect("Could not retrieve the executable's current directory.");

        let scratch_dir = tempfile::Builder::new()
            .prefix("tera_test_scratchspace")
            .tempdir_in(&this_dir)
            .expect(&format!(
                "Could not create temporary directory for test in current directory ({}).",
                this_dir.display()
            ));
        dbg!(&scratch_dir.path().display());

        File::create(scratch_dir.path().join("hey.html")).expect("Failed to create a test file");
        File::create(scratch_dir.path().join("ho.html")).expect("Failed to create a test file");
        let glob = PathBuf::from("./")
            .join(scratch_dir.path().file_name().unwrap())
            .join("**")
            .join("*.html")
            .into_os_string()
            .into_string()
            .unwrap();
        let data = load_from_glob(&glob).unwrap();
        assert_eq!(data.len(), 2);
    }

    // https://github.com/Keats/tera/issues/819
    #[test]
    fn empty_list_on_invalid_glob() {
        let data = load_from_glob("\\dev/null/*").unwrap();
        assert!(data.is_empty());
    }
}
