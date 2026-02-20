use mimium_lang::compiler::mirgen;
use mimium_lang::interner::{Symbol, TypeNodeId};
use mimium_lang::utils::error::ReportableError;
use ropey::Rope;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Position, Range, Url,
};

use crate::semantic_token::{ImCompleteSemanticToken, ParseResult, parse};

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct AnalysisRequest {
    pub uri: String,
    pub text: String,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct AnalysisResponse {
    pub diagnostics: Vec<Diagnostic>,
    pub semantic_tokens: Vec<ImCompleteSemanticToken>,
}

pub fn analyze_source(
    src: &str,
    url: Url,
    builtin_types: &[(Symbol, TypeNodeId)],
) -> AnalysisResponse {
    let rope = ropey::Rope::from_str(src);

    let parser_tokens = mimium_lang::compiler::parser::tokenize(src);
    let parser_preparsed = mimium_lang::compiler::parser::preparse(&parser_tokens);
    let (parser_root, parser_arena, parser_tokens, _parser_errors) =
        mimium_lang::compiler::parser::parse_cst(parser_tokens, &parser_preparsed);

    let semantic_tokens =
        crate::semantic_token::tokens_from_green(parser_root, &parser_arena, &parser_tokens, src);

    let ParseResult {
        ast,
        errors,
        semantic_tokens: _,
        module_info,
    } = parse(src, url.as_str());

    let errs = {
        let ast = ast.wrap_to_staged_expr();
        let (_, _, typeerrs) =
            mirgen::typecheck_with_module_info(ast, builtin_types, None, module_info);
        errors.into_iter().chain(typeerrs).collect::<Vec<_>>()
    };

    let diagnostics = errs
        .into_iter()
        .flat_map(|item| diagnostic_from_error(item, url.clone(), &rope))
        .collect::<Vec<Diagnostic>>();

    AnalysisResponse {
        diagnostics,
        semantic_tokens,
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

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new(line as u32, column as u32))
}
