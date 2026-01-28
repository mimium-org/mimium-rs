use std::path::PathBuf;
use std::process::Stdio;

use mimium_lang::interner::Symbol;

pub mod semantic_token;

use dashmap::DashMap;
use log::debug;
use mimium_lang::compiler::mirgen;
use mimium_lang::interner::{ExprNodeId, TypeNodeId};
use mimium_lang::parser;
use mimium_lang::plugin::Plugin;
use mimium_lang::utils::error::ReportableError;
use mimium_lang::{Config, ExecContext};
use ropey::Rope;
use semantic_token::{ImCompleteSemanticToken, LEGEND_TYPE, ParseResult, parse};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use tokio::io::AsyncWriteExt;
use tokio::process::Command;
type SrcUri = String;

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
struct MimiumCtx {
    builtin_types: Vec<(Symbol, TypeNodeId)>,
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

#[derive(Debug)]
struct Backend {
    client: Client,
    compiler_ctx: MimiumCtx,
    ast_map: DashMap<SrcUri, ExprNodeId>,
    // semantic_map: DashMap<SrcUri, Semantic>,
    document_map: DashMap<SrcUri, Rope>,
    semantic_token_map: DashMap<SrcUri, Vec<ImCompleteSemanticToken>>,
    // Parser state
    parser_arena_map: DashMap<SrcUri, parser::GreenNodeArena>,
    parser_root_map: DashMap<SrcUri, parser::GreenNodeId>,
    parser_tokens_map: DashMap<SrcUri, Vec<parser::Token>>,
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
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
            language_id: "mimium".to_string(),
        })
        .await
    }
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            text: params.content_changes[0].text.clone(),
            uri: params.text_document.uri,
            version: params.text_document.version,
            language_id: "mimium".to_string(),
        })
        .await
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        dbg!(&params.text);
        if let Some(text) = params.text {
            let item = TextDocumentItem {
                uri: params.text_document.uri,
                text,
                version: -1,
                language_id: "mimium".to_string(),
            };
            self.on_change(item).await;
            _ = self.client.semantic_tokens_refresh().await;
        }
        debug!("file saved!");
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        debug!("file closed!");
    }
    
    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        let rope = match self.document_map.get(uri.as_str()) {
            Some(rope) => rope,
            None => return Ok(None),
        };

        let text = rope.to_string();
        let indent_size = params.options.tab_size as usize;
        let width = 80usize;

        let formatter_path = std::env::var("HOME")
            .map(|home| format!("{home}/.mimium/bin/mimium-fmt"))
            .unwrap_or_else(|_| {
                std::env::current_exe()
                    .ok()
                    .and_then(|path| path.parent().map(|parent| parent.join("mimium-fmt")))
                    .map(|path| path.to_string_lossy().to_string())
                    .unwrap_or_else(|| "mimium-fmt".to_string())
            });

        let mut child = match Command::new(formatter_path)
            .arg("--width")
            .arg(width.to_string())
            .arg("--indent-size")
            .arg(indent_size.to_string())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
        {
            Ok(child) => child,
            Err(_) => return Ok(None),
        };

        if let Some(mut stdin) = child.stdin.take()
            && stdin.write_all(text.as_bytes()).await.is_err() {
                return Ok(None);
            }

        let output = match child.wait_with_output().await {
            Ok(output) => output,
            Err(_) => return Ok(None),
        };

        if !output.status.success() {
            return Ok(None);
        }

        let formatted = String::from_utf8_lossy(&output.stdout).to_string();

        let last_line = rope.len_lines().saturating_sub(1);
        let last_char = rope.line(last_line).len_chars();
        let range = Range::new(Position::new(0, 0), Position::new(last_line as u32, last_char as u32));

        Ok(Some(vec![TextEdit { range, new_text: formatted }]))
    }
}
fn diagnostic_from_error(
    error: Box<dyn ReportableError>,
    url: Url,
    rope: &Rope,
) -> Option<Diagnostic> {
    let severity = DiagnosticSeverity::ERROR;
    let main_message = error.get_message();
    let labels = error.get_labels();
    let (mainloc, _mainmsg) = labels.first()?;

    let span = &mainloc.span;
    let start_position = offset_to_position(span.start, rope)?;
    let end_position = offset_to_position(span.end, rope)?;
    let related_informations = labels
        .iter()
        .filter_map(|(loc, msg)| {
            let span = &loc.span;
            let start_position = offset_to_position(span.start, rope)?;
            let end_position = offset_to_position(span.end, rope)?;
            let uri = if loc.path.to_string_lossy() != "" {
                Url::from_file_path(loc.path.clone()).unwrap_or(url.clone())
            } else {
                url.clone()
            };
            Some(DiagnosticRelatedInformation {
                location: Location {
                    uri,
                    range: Range::new(start_position, end_position),
                },
                message: msg.clone(),
            })
        })
        .collect();
    Some(Diagnostic::new(
        Range::new(start_position, end_position),
        Some(severity),
        None,
        None,
        main_message.clone(),
        Some(related_informations),
        None,
    ))
}
impl Backend {
    fn compile(&self, src: &str, url: Url) -> Vec<Diagnostic> {
        let rope = ropey::Rope::from_str(src);

        // Run parser for IDE features
        let parser_tokens = parser::tokenize(src);
        let parser_preparsed = parser::preparse(&parser_tokens);
        let (parser_root, parser_arena, parser_tokens, _parser_errors) =
            parser::parse_cst(parser_tokens, &parser_preparsed);

        // Generate semantic tokens from parser by traversing Green Tree
        let semantic_tokens =
            semantic_token::tokens_from_green(parser_root, &parser_arena, &parser_tokens);
        self.semantic_token_map
            .insert(url.to_string(), semantic_tokens);

        // Store parser results
        self.parser_tokens_map
            .insert(url.to_string(), parser_tokens);
        self.parser_root_map
            .insert(url.to_string(), parser_root);
        self.parser_arena_map
            .insert(url.to_string(), parser_arena);

        // Run existing parser for type checking
        let ParseResult {
            ast,
            errors,
            semantic_tokens: _, // Ignore semantic tokens from old parser
        } = parse(src, url.as_str());
        // Note: semantic_token_map is already populated above with parser tokens
        let errs = {
            let ast = ast.wrap_to_staged_expr();
            let (_, _, typeerrs) = mirgen::typecheck(ast, &self.compiler_ctx.builtin_types, None);
            errors.into_iter().chain(typeerrs).collect::<Vec<_>>()
        };

        errs.into_iter()
            .flat_map(|item| diagnostic_from_error(item, url.clone(), &rope))
            .collect::<Vec<Diagnostic>>()
    }
    async fn on_change(&self, params: TextDocumentItem) {
        debug!("{}", &params.version);
        let rope = ropey::Rope::from_str(&params.text);
        self.document_map
            .insert(params.uri.to_string(), rope.clone());
        let diagnostics = self.compile(&params.text, params.uri.clone());
        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;
    }
}

pub async fn lib_main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        compiler_ctx: MimiumCtx::new(),
        ast_map: DashMap::new(),
        document_map: DashMap::new(),
        semantic_token_map: DashMap::new(),
        parser_arena_map: DashMap::new(),
        parser_root_map: DashMap::new(),
        parser_tokens_map: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new(line as u32, column as u32))
}

fn position_to_offset(position: Position, rope: &Rope) -> Option<usize> {
    let line_char_offset = rope.try_line_to_char(position.line as usize).ok()?;
    let slice = rope.slice(0..line_char_offset + position.character as usize);
    Some(slice.len_bytes())
}
