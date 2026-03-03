use std::{env, path::PathBuf};
use thiserror::Error;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
fn github_lib_base_url_from_pkg_version() -> String {
    let version = env!("CARGO_PKG_VERSION");
    format!("https://raw.githubusercontent.com/mimium-org/mimium-rs/v{version}/lib/")
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

#[cfg(not(target_arch = "wasm32"))]
fn find_workspace_lib_path(
    current_file_or_dir: &str,
    relpath: &std::path::Path,
) -> Option<PathBuf> {
    let parent = get_parent_dir(current_file_or_dir).ok()?;
    parent.ancestors().find_map(|ancestor| {
        let candidate = ancestor.join("lib").join(relpath);
        candidate
            .exists()
            .then(|| candidate.canonicalize().ok())
            .flatten()
    })
}

#[cfg(target_arch = "wasm32")]
fn find_workspace_lib_path(
    _current_file_or_dir: &str,
    _relpath: &std::path::Path,
) -> Option<PathBuf> {
    None
}

/// Get additional library search paths from the `MIMIUM_LIB_PATH` environment variable.
/// Multiple paths can be separated by `:` (Unix) or `;` (Windows).
fn get_env_lib_paths() -> Vec<PathBuf> {
    env::var("MIMIUM_LIB_PATH")
        .ok()
        .map(|val| env::split_paths(&val).collect())
        .unwrap_or_default()
}

/// Used for resolving include.
///
/// Search order:
/// 1. `~/.mimium/lib` (default library path)
/// 2. Paths specified by the `MIMIUM_LIB_PATH` environment variable
/// 3. Workspace ancestor `lib/` directories
/// 4. Relative to the current file
///
/// Absolute or explicitly relative paths (starting with `.`) skip steps 1-3.
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
    // Search paths from MIMIUM_LIB_PATH environment variable
    if search_default_lib {
        for lib_dir in get_env_lib_paths() {
            let cpath = lib_dir.join(path);
            if let Ok(cpath) = cpath.canonicalize()
                && let Ok(content) = load(&cpath.to_string_lossy())
            {
                return Ok((content, cpath));
            }
        }
    }
    if search_default_lib
        && let Some(cpath) = find_workspace_lib_path(current_file_or_dir, path)
        && let Ok(content) = load(&cpath.to_string_lossy())
    {
        return Ok((content, cpath));
    }
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
pub fn has_network_api() -> bool {
    has_network_api_js().unwrap_or(false)
}

#[cfg(target_arch = "wasm32")]
pub async fn preload_stdlib_cache_with_base_url(base_url: &str) -> Result<(), String> {
    preload_mimium_lib_cache(base_url)
        .await
        .map_err(|e| format!("{:?}", e))
}

#[cfg(target_arch = "wasm32")]
pub async fn preload_user_module_cache(
    source: &str,
    module_base_url: Option<&str>,
) -> Result<(), String> {
    preload_user_module_cache_js(source, module_base_url.unwrap_or(""))
        .await
        .map_err(|e| format!("{:?}", e))
}

#[cfg(target_arch = "wasm32")]
pub fn set_module_base_url(base_url: Option<&str>) -> Result<(), String> {
    set_module_base_url_js(base_url.unwrap_or("")).map_err(|e| format!("{:?}", e))
}

#[cfg(target_arch = "wasm32")]
pub fn put_virtual_file_cache(path: &str, content: &str) -> Result<(), String> {
    __mimium_test_put_cache(path, content).map_err(|e| format!("{e:?}"))
}

#[cfg(target_arch = "wasm32")]
pub fn clear_virtual_file_cache() -> Result<(), String> {
    __mimium_test_clear_cache().map_err(|e| format!("{e:?}"))
}

#[cfg(target_arch = "wasm32")]
pub fn export_virtual_file_cache_json() -> Result<String, String> {
    export_virtual_file_cache_json_js().map_err(|e| format!("{e:?}"))
}

#[cfg(target_arch = "wasm32")]
pub fn import_virtual_file_cache_json(payload: &str) -> Result<(), String> {
    import_virtual_file_cache_json_js(payload).map_err(|e| format!("{e:?}"))
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen(module = "/src/utils/fileloader.mjs")]
extern "C" {
    #[wasm_bindgen(catch)]
    fn read_file(path: &str) -> Result<String, JsValue>;
    #[wasm_bindgen(catch)]
    pub fn get_env(key: &str) -> Result<String, JsValue>;
    #[wasm_bindgen(catch, js_name = has_network_api)]
    fn has_network_api_js() -> Result<bool, JsValue>;
    #[wasm_bindgen(catch)]
    async fn preload_mimium_lib_cache(base_url: &str) -> Result<(), JsValue>;
    #[wasm_bindgen(catch, js_name = preload_user_module_cache)]
    async fn preload_user_module_cache_js(source: &str, base_url: &str) -> Result<(), JsValue>;
    #[wasm_bindgen(catch, js_name = set_module_base_url)]
    fn set_module_base_url_js(base_url: &str) -> Result<(), JsValue>;
    #[wasm_bindgen(catch)]
    fn __mimium_test_put_cache(path: &str, content: &str) -> Result<(), JsValue>;
    #[wasm_bindgen(catch)]
    fn __mimium_test_clear_cache() -> Result<(), JsValue>;
    #[wasm_bindgen(catch, js_name = export_virtual_file_cache_json)]
    fn export_virtual_file_cache_json_js() -> Result<String, JsValue>;
    #[wasm_bindgen(catch, js_name = import_virtual_file_cache_json)]
    fn import_virtual_file_cache_json_js(payload: &str) -> Result<(), JsValue>;
    #[wasm_bindgen(catch)]
    fn __mimium_test_get_last_preload_base_url() -> Result<String, JsValue>;
}

#[cfg(all(test, target_arch = "wasm32"))]
mod test {
    use super::*;
    use wasm_bindgen_test::*;

    // wasm_bindgen_test_configure!(run_in_browser);

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

    // #[wasm_bindgen_test]
    // fn browser_cache_alias_resolution_test() {
    //     __mimium_test_clear_cache().expect("clear cache failed");
    //     __mimium_test_put_cache("core.mmm", "fn dsp(){0.0}").expect("put cache failed");
    //
    //     let a = load("core.mmm").expect("core.mmm not found");
    //     let b = load("./core.mmm").expect("./core.mmm not found");
    //     let c = load("lib/core.mmm").expect("lib/core.mmm not found");
    //     let d = load("/lib/core.mmm").expect("/lib/core.mmm not found");
    //
    //     assert_eq!(a, "fn dsp(){0.0}");
    //     assert_eq!(a, b);
    //     assert_eq!(a, c);
    //     assert_eq!(a, d);
    // }
    //
    // #[wasm_bindgen_test(async)]
    // async fn preload_uses_cargo_version_tag_test() {
    //     __mimium_test_clear_cache().expect("clear cache failed");
    //     preload_github_stdlib_cache()
    //         .await
    //         .expect("preload should complete");
    //
    //     let base_url =
    //         __mimium_test_get_last_preload_base_url().expect("base url should be available");
    //     let expected = format!(
    //         "https://raw.githubusercontent.com/mimium-org/mimium-rs/v{}/lib/",
    //         env!("CARGO_PKG_VERSION")
    //     );
    //     assert_eq!(base_url, expected);
    // }
}
