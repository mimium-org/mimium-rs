use dashmap::DashMap;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

/// The mimium language server implementation.
pub struct MimiumLanguageServer {
    /// The LSP client.
    client: Client,
    /// Document storage.
    documents: Arc<DashMap<Url, String>>,
}

impl MimiumLanguageServer {
    /// Create a new language server instance.
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(DashMap::new()),
        }
    }

    /// Parse a document and return diagnostics.
    async fn parse_document(&self, uri: &Url, text: &str) -> Vec<Diagnostic> {
        use crate::capabilities::diagnostics;
        use std::path::PathBuf;

        // Convert URI to path
        let path = uri.to_file_path().ok();
        
        // Parse the document and get diagnostics
        diagnostics::parse_document(text, path)
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for MimiumLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                // Add more capabilities as they are implemented
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: "mimium-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "mimium language server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        
        // Store the document
        self.documents.insert(uri.clone(), text.clone());
        
        // Parse the document and publish diagnostics
        let diagnostics = self.parse_document(&uri, &text).await;
        self.client.publish_diagnostics(uri, diagnostics, None).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        
        // Get the full content of the document
        if let Some(change) = params.content_changes.get(0) {
            let text = change.text.clone();
            
            // Update the document
            self.documents.insert(uri.clone(), text.clone());
            
            // Parse the document and publish diagnostics
            let diagnostics = self.parse_document(&uri, &text).await;
            self.client.publish_diagnostics(uri, diagnostics, None).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        
        // Get the document content
        if let Some(doc) = self.documents.get(&uri) {
            let text = doc.value().clone();
            
            // Parse the document and publish diagnostics
            let diagnostics = self.parse_document(&uri, &text).await;
            self.client.publish_diagnostics(uri, diagnostics, None).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        
        // Remove the document
        self.documents.remove(&uri);
        
        // Clear diagnostics
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        use crate::capabilities::hover;
        
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        
        // Get the document content
        if let Some(doc) = self.documents.get(&uri) {
            let text = doc.value().clone();
            
            // Get hover information
            let hover_info = hover::hover_at_position(&text, position, &uri).await;
            Ok(hover_info)
        } else {
            Ok(None)
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        use crate::capabilities::completion;
        
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        
        // Get the document content
        if let Some(doc) = self.documents.get(&uri) {
            let text = doc.value().clone();
            
            // Get completion items
            let completion_response = completion::completion_at_position(&text, position, &uri).await;
            Ok(completion_response)
        } else {
            Ok(None)
        }
    }
}
