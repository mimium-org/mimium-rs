use std::sync::mpsc;

use super::RunMode;
use mimium_lang::compiler::{self, emit_ast};
use mimium_lang::interner::ExprNodeId;
use mimium_lang::mir::Mir;
use mimium_lang::plugin::MacroFunction;
use mimium_lang::runtime::vm;
use mimium_lang::utils::error::RichError;
use mpsc::{Receiver, Sender};
pub(crate) type Errors = Vec<RichError>;
pub struct CompileRequest {
    pub source: String,
    pub path: std::path::PathBuf,
    pub option: super::RunOptions,
}

pub struct AsyncCompilerService {
    input: Receiver<CompileRequest>,
    output: Sender<Result<Response, Errors>>,
    compiler: compiler::Context,
}
pub struct AsyncCompilerClient {
    pub tx: Sender<CompileRequest>,
    pub rx: Receiver<Result<Response, Errors>>,
}

pub enum Response {
    Ast(ExprNodeId),
    Mir(Mir),
    ByteCode(vm::Program),
}
struct MacroInfo(Box<dyn MacroFunction>);
//macro will modify system plugin instance from different threads but it will never confilict with VM execution
unsafe impl Send for MacroInfo {}
unsafe impl Sync for MacroInfo {}

impl AsyncCompilerService {
    pub fn new(
        input: Receiver<CompileRequest>,
        output: Sender<Result<Response, Errors>>,
        compiler: compiler::Context,
    ) -> Self {
        Self {
            input,
            output,
            compiler,
        }
    }
    pub fn run(&self) {
        while let Ok(request) = self.input.recv() {
            let response = match request.option.mode {
                RunMode::EmitAst => {
                    let ast = emit_ast(&request.source, Some(request.path.clone()));
                    ast.map(Response::Ast)
                        .map_err(|errs| errs.into_iter().map(RichError::from).collect())
                }
                RunMode::EmitMir => {
                    todo!()
                }
                RunMode::EmitByteCode | RunMode::NativeAudio => self
                    .compiler
                    .emit_bytecode(&request.source)
                    .map(Response::ByteCode)
                    .map_err(|errs| errs.into_iter().map(RichError::from).collect()),
                _ => {
                    todo!()
                }
            };

            let _ = self.output.send(response);
        }
    }
}
pub fn start_async_compiler_service(compiler: compiler::Context) -> AsyncCompilerClient {
    let (tx_request, rx_request) = mpsc::channel();
    let (tx_response, rx_response) = mpsc::channel();

    std::thread::spawn(move || {
        let service = AsyncCompilerService::new(rx_request, tx_response, compiler);
        service.run();
    });
    AsyncCompilerClient {
        tx: tx_request,
        rx: rx_response,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn is_async() {
        use mimium_lang::Config;

        let request = CompileRequest {
            source: "test".to_string(),
            path: std::path::PathBuf::from(""),
            option: crate::RunOptions {
                mode: RunMode::EmitAst,
                with_gui: false,
                config: Config::default(),
            },
        };
        let handle = std::thread::spawn(move || {
            let src = request.source;
            src
        });
        let res = handle.join().unwrap();
        assert_eq!(res, "test".to_string());
    }
}
