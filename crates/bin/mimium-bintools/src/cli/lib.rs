use mimium_lang::compiler;
use mpsc::{Receiver, Sender};

struct CompileRequest {
    source: String,
    option: compiler::Config,
}

struct AsyncCompilerService {
    input: Receiver<CompileRequest>,
}
