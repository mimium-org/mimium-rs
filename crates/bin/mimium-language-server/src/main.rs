use std::sync::Arc;

use dashmap::DashMap;
use log::debug;
use mimium_lang::compiler::mirgen;
use mimium_lang::interner::{ExprNodeId, Symbol, TypeNodeId};
use mimium_lang::utils::error::ReportableError;
use mimium_language_server::semantic_token::{
    ImCompleteSemanticToken, LEGEND_TYPE, ParseResult, parse,
};
use ropey::Rope;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
type SrcUri = String;

struct MimiumCtx {
    builtin_types: Vec<(Symbol, TypeNodeId)>,
}
impl MimiumCtx {
    fn new() -> Self {
        let mut execctx = mimium_cli::get_default_context(None, true, Default::default());
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
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        debug!("initialize: {:#?}", params);
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
                text: text,
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
}
fn diagnostic_from_error(
    error: Box<dyn ReportableError>,
    url: Url,
    rope: &Rope,
) -> Option<Diagnostic> {
    let severity = DiagnosticSeverity::ERROR;

    if let Some(((mainloc, mainmsg), rest)) = error.get_labels().split_first() {
        let span = &mainloc.span;
        let start_position = offset_to_position(span.start, &rope)?;
        let end_position = offset_to_position(span.end, &rope)?;
        let related_informations = rest
            .iter()
            .filter_map(|(loc, msg)| {
                let span = &loc.span;
                let start_position = offset_to_position(span.start, &rope)?;
                let end_position = offset_to_position(span.end, &rope)?;
                let uri = if loc.path.to_string() != "" {
                    Url::from_file_path(std::path::PathBuf::from(loc.path.to_string()))
                        .unwrap_or(url.clone())
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
            mainmsg.clone(),
            Some(related_informations),
            None,
        ))
    } else {
        None
    }
}
impl Backend {
    fn compile(&self, src: &str, url: Url) -> Vec<Diagnostic> {
        let rope = ropey::Rope::from_str(&src);

        let ParseResult {
            ast,
            errors,
            semantic_tokens,
        } = parse(src, url.as_str());
        self.semantic_token_map
            .insert(url.to_string(), semantic_tokens);
        let errs = {
            let res = mirgen::compile(ast, &self.compiler_ctx.builtin_types, &[], None);
            if res.is_err() {
                errors
                    .into_iter()
                    .chain(res.err().unwrap().into_iter())
                    .collect::<Vec<_>>()
            } else {
                errors
            }
        };
        let diagnostics = errs
            .into_iter()
            .flat_map(|item| diagnostic_from_error(item, url.clone(), &rope))
            .collect::<Vec<Diagnostic>>();
        diagnostics
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

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        compiler_ctx: MimiumCtx::new(),
        ast_map: DashMap::new(),
        document_map: DashMap::new(),
        semantic_token_map: DashMap::new(),
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
