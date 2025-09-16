use tree_sitter::Language;

extern "C" {
    fn tree_sitter_mimium() -> Language;
}

/// Get the tree-sitter Language for mimium
pub fn language() -> Language {
    unsafe { tree_sitter_mimium() }
}