mod print;

use std::sync::LazyLock;
use std::sync::Mutex;
pub struct GlobalConfig {
    pub indent_size: usize,
}
impl Default for GlobalConfig {
    fn default() -> Self {
        Self { indent_size: 4 }
    }
}
pub static GLOBAL_DATA: LazyLock<Mutex<GlobalConfig>> =
    LazyLock::new(|| Mutex::new(GlobalConfig::default()));

pub use print::pretty_print;
