//! Language server of mimium for IDE support.
//!
//! This crate is only functional on non-wasm32 targets since it depends on
//! tokio and tower-lsp which don't support wasm32.

#[cfg(not(target_arch = "wasm32"))]
pub mod semantic_token;

#[cfg(not(target_arch = "wasm32"))]
mod server;

#[cfg(not(target_arch = "wasm32"))]
pub use server::lib_main;
