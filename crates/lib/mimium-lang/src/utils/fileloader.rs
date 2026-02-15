use std::{env, path::PathBuf};
use thiserror::Error;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
fn github_lib_base_url_from_pkg_version() -> String {
    let version = env!("CARGO_PKG_VERSION");
    format!(
        "https://raw.githubusercontent.com/mimium-org/mimium-rs/v{version}/lib/"
    )
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("IoError: {0}")]
    IoError(#[from] std::io::Error),
    #[error("File {path} not found: {message}", path = path.display())]
    FileNotFound { message: String, path: PathBuf },
    #[error("Failed to convert into UTF: {0}")]
    UtfConversionError(#[from] std::string::FromUtf8Error),
    #[error("Failed to join path: {0}")]
    PathJoinError(#[from] env::JoinPathsError),
    #[error("File tried to include itself recusively: {}", path.to_string_lossy())]
    SelfReference { path: PathBuf },
}
fn get_default_library_path() -> Option<PathBuf> {
    #[cfg(not(target_arch = "wasm32"))]
    let home = homedir::my_home().ok().flatten();
    #[cfg(target_arch = "wasm32")]
    let home: Option<PathBuf> = None;
    if home.is_none() {
        log::warn!("default library search path is not available on this platform.");
        return None;
    }
    let p = home.unwrap().join(PathBuf::from(".mimium/lib"));
    Some(p)
}

pub fn get_canonical_path(current_file_or_dir: &str, relpath: &str) -> Result<PathBuf, Error> {
    let parent_dir = get_parent_dir(current_file_or_dir)?;
    let relpath2 = std::path::PathBuf::from(relpath);
    let abspath = [parent_dir, relpath2]
        .into_iter()
        .collect::<std::path::PathBuf>();
    if cfg!(target_arch = "wasm32") {
        //canonicalize is platform-dependent and always returns Err on wasm32
        Ok(abspath)
    } else {
        abspath.canonicalize().map_err(|e| Error::FileNotFound {
            message: e.to_string(),
            path: abspath,
        })
    }
}

fn get_parent_dir(current_file: &str) -> Result<PathBuf, Error> {
    let current_filepath = std::path::Path::new(current_file);
    if current_filepath.is_dir() {
        Ok(current_filepath.into())
    } else {
        #[cfg(not(target_arch = "wasm32"))]
        let cwd = env::current_dir()?;
        #[cfg(target_arch = "wasm32")]
        let cwd = std::path::PathBuf::new();
        Ok(current_filepath.parent().map_or_else(|| cwd, PathBuf::from))
    }
}

/// Used for resolving include.
/// If the filename is given it searches ~/.mimium/lib first. If not found, tries to find in relative path.
/// If the relative path is given explicitly, do not find in standard library path.
pub fn load_mmmlibfile(current_file_or_dir: &str, path: &str) -> Result<(String, PathBuf), Error> {
    let path = std::path::Path::new(path);
    let search_default_lib = !(path.is_absolute() || path.starts_with("."));
    if let (true, Some(stdlibpath)) = (search_default_lib, get_default_library_path()) {
        let cpath = stdlibpath.join(path).canonicalize();
        if let Ok(cpath) = cpath
            && let Ok(content) = load(&cpath.to_string_lossy())
        {
            return Ok((content, cpath));
            // if not found in the stdlib, continue to find in a relative path.
        }
    };
    let cpath = get_canonical_path(current_file_or_dir, &path.to_string_lossy())?;
    if current_file_or_dir == cpath.to_string_lossy() {
        return Err(Error::SelfReference {
            path: cpath.clone(),
        });
    }
    let content = load(&cpath.to_string_lossy())?;
    Ok((content, cpath))
}

#[cfg(not(target_arch = "wasm32"))]
pub fn load(canonical_path: &str) -> Result<String, Error> {
    // debug_assert!(std::path::Path::new(canonical_path).is_absolute());
    let content = std::fs::read(canonical_path).map_err(|e| Error::FileNotFound {
        message: e.to_string(),
        path: PathBuf::from(canonical_path),
    })?;

    let content_r = String::from_utf8(content).map_err(Error::from)?;
    Ok(content_r)
}

#[cfg(target_arch = "wasm32")]
pub fn load(canonical_path: &str) -> Result<String, Error> {
    let content_r = read_file(canonical_path).map_err(|e| Error::FileNotFound {
        message: format!("{:?}", e),
        path: canonical_path.into(),
    })?;
    Ok(content_r)
}

#[cfg(target_arch = "wasm32")]
pub async fn preload_github_stdlib_cache() -> Result<(), String> {
    let base_url = github_lib_base_url_from_pkg_version();
    preload_mimium_lib_cache(&base_url)
        .await
        .map_err(|e| format!("{:?}", e))
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(module = "/src/utils/fileloader.cjs")]
extern "C" {
    #[wasm_bindgen(catch)]
    fn read_file(path: &str) -> Result<String, JsValue>;
    #[wasm_bindgen(catch)]
    pub fn get_env(key: &str) -> Result<String, JsValue>;
    #[wasm_bindgen(catch)]
    async fn preload_mimium_lib_cache(base_url: &str) -> Result<(), JsValue>;
}

#[cfg(all(test, target_arch = "wasm32"))]
mod test {
    use super::*;
    use wasm_bindgen_test::*;
    fn setup_file() -> (String, String) {
        let ans = r#"include("error_include_itself.mmm")
fn dsp(){
    0.0
}"#;
        let file = format!(
            "{}/../mimium-test/tests/mmm/{}",
            get_env("TEST_ROOT").expect("TEST_ROOT is not set"),
            "error_include_itself.mmm"
        );
        (ans.to_string(), file)
    }
    #[wasm_bindgen_test] //wasm only test
    fn fileloader_test() {
        let (ans, file) = setup_file();
        let res = load(&file).expect("failed to load file");
        assert_eq!(res, ans);
    }
    #[wasm_bindgen_test] //wasm only test
    fn loadlib_test() {
        use super::*;
        let (ans, file) = setup_file();
        let (res, _path) = load_mmmlibfile("/", &file).expect("failed to load file");
        assert_eq!(res, ans);
    }
    #[wasm_bindgen_test] //wasm only test
    fn loadlib_test_selfinclude() {
        use super::*;
        let (_, file) = setup_file();
        let err = load_mmmlibfile(&file, &file).expect_err("should be an error");

        assert!(matches!(err, Error::SelfReference { .. }));
    }
}
