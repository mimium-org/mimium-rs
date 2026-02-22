use std::path::PathBuf;
use std::process::Stdio;
use std::time::Duration;

use mimium_lang::interner::Symbol;

use dashmap::DashMap;
use log::{debug, warn};
use mimium_lang::interner::TypeNodeId;
use mimium_lang::plugin::Plugin;
use mimium_lang::{Config, ExecContext};
use ropey::Rope;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::process::Command;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::analysis::{
    AnalysisRequest, AnalysisResponse, CompletionKind, CompletionSymbol, FnSignature,
    analyze_source,
};
use crate::semantic_token::{ImCompleteSemanticToken, LEGEND_TYPE};
type SrcUri = String;
const CHANGE_DEBOUNCE_MS: u64 = 200;

/// Construct an [`ExecContext`] with the default set of plugins.
fn get_default_context(path: Option<PathBuf>, with_gui: bool, config: Config) -> ExecContext {
    let plugins: Vec<Box<dyn Plugin>> = vec![];
    let mut ctx = ExecContext::new(plugins.into_iter(), path, config);
    ctx.add_system_plugin(mimium_symphonia::SamplerPlugin::default());
    ctx.add_system_plugin(mimium_scheduler::get_default_scheduler_plugin());
    if let Some(midi_plug) = mimium_midi::MidiPlugin::try_new() {
        ctx.add_system_plugin(midi_plug);
    } else {
        log::warn!("Midi is not supported on this platform.")
    }

    if with_gui {
        #[cfg(not(target_arch = "wasm32"))]
        ctx.add_system_plugin(mimium_guitools::GuiToolPlugin::default());
    }

    ctx
}
pub(crate) struct MimiumCtx {
    pub(crate) builtin_types: Vec<(Symbol, TypeNodeId)>,
}
impl MimiumCtx {
    fn new() -> Self {
        let mut execctx = get_default_context(None, true, Default::default());
        execctx.prepare_compiler();
        let builtin_types = execctx.get_compiler().unwrap().get_ext_typeinfos();
        Self { builtin_types }
    }
}
impl std::fmt::Display for MimiumCtx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "mimium context")
    }
}
impl std::fmt::Debug for MimiumCtx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "mimium context")
    }
}

pub(crate) fn get_default_compiler_context() -> MimiumCtx {
    MimiumCtx::new()
}

#[derive(Debug)]
struct Backend {
    client: Client,
    compiler_ctx: Option<MimiumCtx>,
    analysis_mode: AnalysisMode,
    document_map: DashMap<SrcUri, Rope>,
    semantic_token_map: DashMap<SrcUri, Vec<ImCompleteSemanticToken>>,
    fn_signature_map: DashMap<SrcUri, Vec<FnSignature>>,
    completion_map: DashMap<SrcUri, Vec<CompletionSymbol>>,
    latest_change_version: DashMap<SrcUri, i32>,
}

#[derive(Debug, Clone, Copy)]
enum AnalysisMode {
    Worker,
    InProcess,
}

impl AnalysisMode {
    fn from_env() -> Self {
        match std::env::var("MIMIUM_LS_ANALYSIS_MODE") {
            Ok(value) if value.eq_ignore_ascii_case("inprocess") => Self::InProcess,
            _ => Self::Worker,
        }
    }
}

fn server_error(message: &str) -> Error {
    let mut error = Error::new(ErrorCode::ServerError(-1));
    error.message = message.to_owned().into();
    error
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        debug!("initialize: {params:#?}");
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: TextDocumentRegistrationOptions {
                                document_selector: Some(vec![DocumentFilter {
                                    language: Some("mimium".to_string()),
                                    scheme: Some("file".to_string()),
                                    pattern: None,
                                }]),
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.to_vec(),
                                    token_modifiers: vec![],
                                },
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                                range: Some(false),
                                work_done_progress_options: WorkDoneProgressOptions {
                                    work_done_progress: None,
                                },
                            },

                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                document_formatting_provider: Some(OneOf::Left(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                }),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![]),
                    resolve_provider: Some(false),
                    all_commit_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    completion_item: None,
                }),
                ..ServerCapabilities::default()
            },
            server_info: None,
            offset_encoding: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "mimium-language-server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        let mut pre_line = 0;
        let mut pre_start = 0;
        let mut semantic_token = || -> Option<_> {
            let rope = self.document_map.get(&uri)?;
            let mut imcomplete_semantic_tokens = self.semantic_token_map.get_mut(&uri)?;
            imcomplete_semantic_tokens.sort_by(|a, b| a.start.cmp(&b.start));

            let semantic_tokens = imcomplete_semantic_tokens.iter().filter_map(|token| {
                let line = rope.try_byte_to_line(token.start).ok()? as u32;
                let first = rope.try_line_to_char(line as usize).ok()? as u32;
                let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
                let delta_line = line - pre_line;
                let delta_start = if delta_line == 0 {
                    start - pre_start
                } else {
                    start
                };
                let res = Some(SemanticToken {
                    delta_line,
                    delta_start,
                    length: token.length as u32,
                    token_type: token.token_type as u32,
                    token_modifiers_bitset: 0,
                });
                pre_line = line;
                pre_start = start;
                res
            });
            let res = semantic_tokens.collect::<Vec<_>>();
            // debug!("semantic_tokens: {:#?}", res);
            Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: res,
            }))
        };
        Ok(semantic_token())
    }
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        debug!("file opened");
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        let text = params.text_document.text;
        self.latest_change_version.insert(uri.to_string(), version);
        self.document_map
            .insert(uri.to_string(), ropey::Rope::from_str(&text));
        self.on_change(uri, version).await
    }
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let uri_str = uri.to_string();
        let version = params.text_document.version;
        self.latest_change_version.insert(uri_str.clone(), version);

        if let Some(change) = params.content_changes.first() {
            self.document_map
                .insert(uri_str.clone(), ropey::Rope::from_str(&change.text));
        }

        tokio::time::sleep(Duration::from_millis(CHANGE_DEBOUNCE_MS)).await;

        let is_latest = self
            .latest_change_version
            .get(&uri_str)
            .map(|latest| *latest == version)
            .unwrap_or(false);

        if is_latest {
            self.on_change(uri, version).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.document_map.insert(
                params.text_document.uri.to_string(),
                ropey::Rope::from_str(&text),
            );
            self.latest_change_version
                .insert(params.text_document.uri.to_string(), -1);
            self.on_change(params.text_document.uri, -1).await;
            _ = self.client.semantic_tokens_refresh().await;
        }
        debug!("file saved!");
    }
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.latest_change_version.remove(&uri);
        self.document_map.remove(&uri);
        self.semantic_token_map.remove(&uri);
        self.fn_signature_map.remove(&uri);
        self.completion_map.remove(&uri);
        debug!("file closed!");
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        debug!("formatting");
        let uri = params.text_document.uri;
        let rope = match self.document_map.get(uri.as_str()) {
            Some(rope) => rope,
            None => {
                return Err(Error::new(ErrorCode::ServerError(-1)));
            }
        };

        let text = rope.to_string();
        let indent_size = params.options.tab_size as usize;
        let width = 80usize;

        // Set global config for formatter
        if let Ok(mut gdata) = mimium_fmt::GLOBAL_DATA.try_lock() {
            gdata.indent_size = indent_size;
        }

        // Call formatter directly
        let formatted = match mimium_fmt::pretty_print(&text, &None, width) {
            Ok(result) => result,
            Err(errs) => {
                let error_messages: Vec<String> = errs.iter().map(|e| e.get_message()).collect();
                return Err(server_error(&format!(
                    "formatter error: {}",
                    error_messages.join(", ")
                )));
            }
        };

        debug!("formatted: {formatted:#?}");

        let last_line = rope.len_lines().saturating_sub(1);
        let last_char = rope.line(last_line).len_chars();
        let range = Range::new(
            Position::new(0, 0),
            Position::new(last_line as u32, last_char as u32),
        );

        Ok(Some(vec![TextEdit {
            range,
            new_text: formatted,
        }]))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();

        let symbols = match self.completion_map.get(&uri) {
            Some(syms) => syms.clone(),
            None => return Ok(None),
        };

        // Optionally filter by prefix typed so far.
        let prefix = {
            let position = params.text_document_position.position;
            let rope = self.document_map.get(&uri);
            rope.and_then(|r| {
                let offset = position_to_offset(position, &r)?;
                let text = r.to_string();
                let bytes = text.as_bytes();
                let end = offset.min(bytes.len());
                let start = find_ident_start(bytes, end);
                std::str::from_utf8(&bytes[start..end])
                    .ok()
                    .map(|s| s.to_string())
            })
            .unwrap_or_default()
        };

        let items: Vec<CompletionItem> = symbols
            .iter()
            .filter_map(|s| {
                // The last segment of a qualified name (e.g. "pingpong_delay" from
                // "delay::pingpong_delay") used for prefix matching and insert text.
                let last_segment = s.name.rsplit("::").next().unwrap_or(s.name.as_str());

                let matches_full = s.name.starts_with(&prefix);
                let matches_segment = last_segment.starts_with(&prefix);
                if !matches_full && !matches_segment {
                    return None;
                }

                let kind = Some(match s.kind {
                    CompletionKind::Function => CompletionItemKind::FUNCTION,
                    CompletionKind::Variable => CompletionItemKind::VARIABLE,
                });
                let detail = if s.type_str.is_empty() {
                    None
                } else {
                    Some(s.type_str.clone())
                };
                // When match is only via the last segment, insert that segment only
                // so the editor doesn't double up the module prefix the user already typed.
                let insert_text = if matches_full {
                    None
                } else {
                    Some(last_segment.to_string())
                };
                Some(CompletionItem {
                    label: s.name.clone(),
                    kind,
                    detail,
                    insert_text,
                    ..Default::default()
                })
            })
            .collect();

        if items.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(items)))
        }
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let rope = match self.document_map.get(&uri) {
            Some(rope) => rope.clone(),
            None => return Ok(None),
        };

        let offset = match position_to_offset(position, &rope) {
            Some(o) => o,
            None => return Ok(None),
        };

        let text = rope.to_string();

        let (fn_name, active_param) = match find_function_call_context(&text, offset) {
            Some(ctx) => ctx,
            None => return Ok(None),
        };

        let signatures = match self.fn_signature_map.get(&uri) {
            Some(sigs) => sigs.clone(),
            None => return Ok(None),
        };

        let sig = match signatures
            .iter()
            .find(|s| s.name == fn_name || s.name.ends_with(&format!("::{fn_name}")))
        {
            Some(s) => s,
            None => return Ok(None),
        };

        let label = format_signature_label(sig);
        let parameters: Vec<ParameterInformation> = sig
            .params
            .iter()
            .map(|p| {
                let param_label = if p.type_str.is_empty() {
                    p.name.clone()
                } else {
                    format!("{}: {}", p.name, p.type_str)
                };
                ParameterInformation {
                    label: ParameterLabel::Simple(param_label),
                    documentation: None,
                }
            })
            .collect();

        Ok(Some(SignatureHelp {
            signatures: vec![SignatureInformation {
                label,
                documentation: None,
                parameters: Some(parameters),
                active_parameter: Some(active_param as u32),
            }],
            active_signature: Some(0),
            active_parameter: Some(active_param as u32),
        }))
    }
}

impl Backend {
    fn resolve_executable_path(candidate: PathBuf) -> Option<PathBuf> {
        std::iter::once(candidate.clone())
            .chain({
                #[cfg(windows)]
                {
                    let exe_candidate = if candidate.extension().is_none() {
                        candidate.with_extension("exe")
                    } else {
                        candidate.clone()
                    };
                    std::iter::once(exe_candidate)
                }
                #[cfg(not(windows))]
                {
                    std::iter::empty()
                }
            })
            .find(|path| path.exists())
    }

    fn worker_path() -> Option<PathBuf> {
        if let Some(path) = std::env::var_os("MIMIUM_LS_WORKER") {
            let candidate = PathBuf::from(path);
            return Self::resolve_executable_path(candidate);
        }
        std::env::current_exe()
            .ok()
            .and_then(|exe| {
                exe.parent()
                    .map(|parent| parent.join("mimium-language-server-worker"))
            })
            .and_then(Self::resolve_executable_path)
    }

    async fn compile_via_worker(
        &self,
        src: &str,
        url: Url,
    ) -> std::result::Result<AnalysisResponse, String> {
        let worker_path = Self::worker_path().ok_or_else(|| {
            "worker binary is not found. set MIMIUM_LS_WORKER or place mimium-language-server-worker next to the language server binary".to_string()
        })?;
        let mut child = Command::new(worker_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| format!("failed to spawn worker: {e}"))?;

        let request = AnalysisRequest {
            uri: url.to_string(),
            text: src.to_string(),
        };
        let payload =
            serde_json::to_vec(&request).map_err(|e| format!("failed to encode request: {e}"))?;

        if let Some(mut stdin) = child.stdin.take() {
            if stdin.write_all(&payload).await.is_err() {
                return Err("failed to write request to worker stdin".to_string());
            }
            _ = stdin.shutdown().await;
        }

        let mut stdout = child
            .stdout
            .take()
            .ok_or_else(|| "worker stdout is not available".to_string())?;
        let mut stderr = child
            .stderr
            .take()
            .ok_or_else(|| "worker stderr is not available".to_string())?;

        let stdout_task = tokio::spawn(async move {
            let mut buf = Vec::new();
            let _ = stdout.read_to_end(&mut buf).await;
            buf
        });
        let stderr_task = tokio::spawn(async move {
            let mut buf = Vec::new();
            let _ = stderr.read_to_end(&mut buf).await;
            buf
        });

        let status = match tokio::time::timeout(Duration::from_secs(3), child.wait()).await {
            Ok(wait_result) => wait_result.map_err(|e| format!("failed waiting worker: {e}"))?,
            Err(_) => {
                warn!("worker timed out (pid={:?}), killing process", child.id());
                _ = child.start_kill();
                _ = child.wait().await;
                _ = stdout_task.await;
                _ = stderr_task.await;
                return Err("worker timed out".to_string());
            }
        };

        let stdout = stdout_task
            .await
            .map_err(|e| format!("failed to join worker stdout task: {e}"))?;
        let stderr = stderr_task
            .await
            .map_err(|e| format!("failed to join worker stderr task: {e}"))?;

        if !status.success() {
            return Err(format!(
                "worker exited with status {}: {}",
                status,
                String::from_utf8_lossy(&stderr)
            ));
        }

        serde_json::from_slice::<AnalysisResponse>(&stdout)
            .map_err(|e| format!("failed to decode worker response: {e}"))
    }

    fn compile_in_process(&self, src: &str, url: Url) -> Option<AnalysisResponse> {
        self.compiler_ctx
            .as_ref()
            .map(|compiler_ctx| analyze_source(src, url, &compiler_ctx.builtin_types))
    }

    async fn on_change(&self, uri: Url, version: i32) {
        debug!("{}", version);
        let text = match self.document_map.get(uri.as_str()) {
            Some(rope) => rope.to_string(),
            None => return,
        };

        let analysis = match self.analysis_mode {
            AnalysisMode::Worker => match self.compile_via_worker(&text, uri.clone()).await {
                Ok(analysis) => analysis,
                Err(message) => {
                    self
                        .client
                        .log_message(
                            MessageType::ERROR,
                            format!(
                                "mimium-language-server analysis failed in worker mode: {message}. set MIMIUM_LS_ANALYSIS_MODE=inprocess to bypass worker mode"
                            ),
                        )
                        .await;
                    return;
                }
            },
            AnalysisMode::InProcess => match self.compile_in_process(&text, uri.clone()) {
                Some(analysis) => analysis,
                None => {
                    self
                        .client
                        .log_message(
                            MessageType::ERROR,
                            "mimium-language-server analysis failed: inprocess mode is selected but compiler context is unavailable",
                        )
                        .await;
                    return;
                }
            },
        };

        self.semantic_token_map
            .insert(uri.to_string(), analysis.semantic_tokens);
        self.fn_signature_map
            .insert(uri.to_string(), analysis.fn_signatures);
        self.completion_map
            .insert(uri.to_string(), analysis.completion_symbols);
        self.client
            .publish_diagnostics(uri, analysis.diagnostics, Some(version))
            .await;
    }
}

pub async fn lib_main() {
    env_logger::init();
    let analysis_mode = AnalysisMode::from_env();
    let compiler_ctx = match analysis_mode {
        AnalysisMode::Worker => None,
        AnalysisMode::InProcess => Some(get_default_compiler_context()),
    };

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        compiler_ctx,
        analysis_mode,
        document_map: DashMap::new(),
        semantic_token_map: DashMap::new(),
        fn_signature_map: DashMap::new(),
        completion_map: DashMap::new(),
        latest_change_version: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    let line_char_offset = rope.try_line_to_char(position.line as usize).ok()?;
    let slice = rope.slice(0..line_char_offset + position.character as usize);
    Some(slice.len_bytes())
}

/// Find which function is being called at the given byte offset and which
/// parameter the cursor is on.
///
/// Returns `(function_name, active_parameter_index)` or `None` if the cursor
/// is not inside a function call.
fn find_function_call_context(text: &str, offset: usize) -> Option<(String, usize)> {
    let bytes = text.as_bytes();
    let mut depth: i32 = 0;
    let mut comma_count: usize = 0;
    let mut i = offset;

    // Scan backwards from the cursor to find the unmatched opening paren.
    // Track nesting depth and count commas at depth 0 (the call we care about).
    while i > 0 {
        i -= 1;
        match bytes[i] {
            b')' => depth += 1,
            b'(' => {
                if depth == 0 {
                    // Found the unmatched opening paren — extract the identifier
                    // that precedes it (skipping whitespace).
                    let paren_pos = i;
                    let end = skip_whitespace_backwards(bytes, paren_pos);
                    if end == 0 {
                        return None;
                    }
                    let start = find_ident_start(bytes, end);
                    let name = std::str::from_utf8(&bytes[start..end]).ok()?;
                    if name.is_empty() {
                        return None;
                    }
                    return Some((name.to_string(), comma_count));
                }
                depth -= 1;
            }
            b',' if depth == 0 => comma_count += 1,
            _ => {}
        }
    }

    None
}

/// Skip whitespace backwards and return the position just past the last
/// non-whitespace character.
fn skip_whitespace_backwards(bytes: &[u8], mut pos: usize) -> usize {
    while pos > 0 && bytes[pos - 1].is_ascii_whitespace() {
        pos -= 1;
    }
    pos
}

/// Walk backwards from `end` (exclusive) to find where an identifier starts.
fn find_ident_start(bytes: &[u8], end: usize) -> usize {
    let mut pos = end;
    while pos > 0 && is_ident_char(bytes[pos - 1]) {
        pos -= 1;
    }
    pos
}

fn is_ident_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Build a human-readable label string for a function signature.
fn format_signature_label(sig: &FnSignature) -> String {
    let params_str = sig
        .params
        .iter()
        .map(|p| {
            if p.type_str.is_empty() {
                p.name.clone()
            } else {
                format!("{}: {}", p.name, p.type_str)
            }
        })
        .collect::<Vec<_>>()
        .join(", ");

    if sig.return_type.is_empty() {
        format!("{}({})", sig.name, params_str)
    } else {
        format!("{}({}) -> {}", sig.name, params_str, sig.return_type)
    }
}
