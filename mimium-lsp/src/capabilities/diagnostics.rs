use mimium_lang::compiler::parser;
use mimium_lang::utils::error::ReportableError;
use mimium_lang::utils::metadata::Location;
use std::path::PathBuf;
use tower_lsp::lsp_types::*;

/// Convert a mimium span to an LSP range.
pub fn span_to_range(span: &std::ops::Range<usize>, text: &str) -> Range {
    // Calculate line and character positions from byte offsets
    let start_pos = position_from_offset(span.start, text);
    let end_pos = position_from_offset(span.end, text);

    Range {
        start: start_pos,
        end: end_pos,
    }
}

/// Convert a byte offset to an LSP position.
pub fn position_from_offset(offset: usize, text: &str) -> Position {
    let mut line = 0;
    let mut character = 0;
    let mut current_offset = 0;

    for c in text.chars() {
        if current_offset >= offset {
            break;
        }

        if c == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }

        current_offset += c.len_utf8();
    }

    Position { line, character }
}

/// Convert mimium errors to LSP diagnostics.
pub fn errors_to_diagnostics(errors: &[Box<dyn ReportableError>], text: &str) -> Vec<Diagnostic> {
    errors
        .iter()
        .flat_map(|err| {
            let labels = err.get_labels();
            let message = err.get_message();

            labels.into_iter().map(move |(location, label_message)| {
                let range = span_to_range(&location.span, text);

                Diagnostic {
                    range,
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("mimium-lsp".to_string()),
                    message: format!("{}: {}", message, label_message),
                    related_information: None,
                    tags: None,
                    data: None,
                }
            })
        })
        .collect()
}

/// Parse a mimium document and return diagnostics.
pub fn parse_document(text: &str, path: Option<PathBuf>) -> Vec<Diagnostic> {
    let (_, errors) = parser::parse(text, path);
    
    if errors.is_empty() {
        vec![]
    } else {
        errors_to_diagnostics(&errors, text)
    }
}
