use std::io::{Read, Write};

use tower_lsp::lsp_types::Url;

use crate::analysis::{AnalysisRequest, analyze_source};
use crate::server::get_default_compiler_context;

pub fn worker_main() {
    let mut stdin = std::io::stdin().lock();
    let mut stdout = std::io::stdout().lock();

    let mut input = Vec::new();
    if stdin.read_to_end(&mut input).is_err() {
        return;
    }

    let request: AnalysisRequest = match serde_json::from_slice(&input) {
        Ok(req) => req,
        Err(_) => return,
    };

    let url = match Url::parse(&request.uri) {
        Ok(url) => url,
        Err(_) => return,
    };

    let compiler_ctx = get_default_compiler_context();
    let response = analyze_source(&request.text, url, &compiler_ctx.builtin_types);

    if let Ok(payload) = serde_json::to_vec(&response) {
        _ = stdout.write_all(&payload);
        _ = stdout.flush();
    }
}
