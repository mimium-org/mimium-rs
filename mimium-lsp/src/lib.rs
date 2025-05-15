//! Language Server Protocol implementation for the mimium programming language.

pub mod server;
pub mod capabilities;

use server::MimiumLanguageServer;
use tower_lsp::{LspService, Server};

/// Start the LSP server with the given I/O streams.
pub async fn start_server() -> anyhow::Result<()> {
    // Initialize the logger
    env_logger::init();

    // Create the language server
    let (service, socket) = LspService::new(|client| MimiumLanguageServer::new(client));

    // Start the server
    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket)
        .serve(service)
        .await;

    Ok(())
}
