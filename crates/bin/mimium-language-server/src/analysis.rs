use mimium_lang::ast::Expr;
use mimium_lang::ast::program::VisibilityMap;
use mimium_lang::compiler::mirgen;
use mimium_lang::interner::{ExprNodeId, Symbol, TypeNodeId};
use mimium_lang::pattern::Pattern;
use mimium_lang::types::Type;
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

/// Parameter information for signature help.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ParamInfo {
    pub name: String,
    pub type_str: String,
    pub has_default: bool,
}

/// Signature information for a function definition.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct FnSignature {
    pub name: String,
    pub params: Vec<ParamInfo>,
    pub return_type: String,
}

/// Kind of a completion symbol.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum CompletionKind {
    Function,
    Variable,
}

/// A single symbol available for completion.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CompletionSymbol {
    pub name: String,
    pub kind: CompletionKind,
    /// Human-readable type annotation, empty when unknown.
    pub type_str: String,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct AnalysisResponse {
    pub diagnostics: Vec<Diagnostic>,
    pub semantic_tokens: Vec<ImCompleteSemanticToken>,
    pub fn_signatures: Vec<FnSignature>,
    pub completion_symbols: Vec<CompletionSymbol>,
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

    // Save visibility info before module_info is consumed by typecheck.
    let visibility_map = module_info.visibility_map.clone();

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

    let fn_signatures = collect_fn_signatures(ast, &visibility_map);
    let completion_symbols = collect_completion_symbols(ast, &visibility_map);

    AnalysisResponse {
        diagnostics,
        semantic_tokens,
        fn_signatures,
        completion_symbols,
    }
}

/// Format a type for display in signature help.
/// Returns an empty string for unknown types.
fn format_type_for_display(ty: &Type) -> String {
    match ty {
        Type::Unknown => String::new(),
        other => format!("{other}"),
    }
}

/// Returns `true` if the symbol with the given mangled name should be included
/// in completions/signatures.
///
/// Top-level symbols (no `$` in mangled name) are always visible because they
/// live in the same file scope. Module members (contain `$`) are only visible
/// when declared `pub`.
fn is_visible(mangled: &str, visibility_map: &VisibilityMap) -> bool {
    if !mangled.contains('$') {
        // Top-level: always available within the same file.
        return true;
    }
    // Module member: only public ones are accessible outside the module.
    let sym = mimium_lang::interner::ToSymbol::to_symbol(&mangled);
    visibility_map.get(&sym).copied().unwrap_or(false)
}

/// Convert an internally mangled name (`mod$fn`) to mimium source syntax (`mod::fn`).
fn demangle_name(name: &str) -> String {
    name.replace('$', "::")
}

/// Walk the AST and collect all function signatures.
fn collect_fn_signatures(root: ExprNodeId, visibility_map: &VisibilityMap) -> Vec<FnSignature> {
    let mut sigs = Vec::new();
    collect_fn_signatures_rec(root, visibility_map, &mut sigs);
    // Deduplicate by name, keeping the first occurrence.
    let mut seen = std::collections::HashSet::new();
    sigs.retain(|s| seen.insert(s.name.clone()));
    sigs
}

/// Recursively walk the AST to find function definitions (LetRec with Lambda).
fn collect_fn_signatures_rec(
    expr_id: ExprNodeId,
    visibility_map: &VisibilityMap,
    sigs: &mut Vec<FnSignature>,
) {
    match expr_id.to_expr() {
        Expr::LetRec(typed_id, value, next) => {
            if let Expr::Lambda(params, ret_ty, _body) = value.to_expr() {
                if is_visible(typed_id.id.as_str(), visibility_map) {
                    let name = demangle_name(typed_id.id.as_str());
                    let params_info = params
                        .iter()
                        .map(|p| {
                            let ty = p.ty.to_type();
                            ParamInfo {
                                name: p.id.as_str().to_string(),
                                type_str: format_type_for_display(&ty),
                                has_default: p.default_value.is_some(),
                            }
                        })
                        .collect();
                    let return_type = ret_ty
                        .map(|r| format_type_for_display(&r.to_type()))
                        .unwrap_or_default();
                    sigs.push(FnSignature {
                        name,
                        params: params_info,
                        return_type,
                    });
                }
            }
            if let Some(next) = next {
                collect_fn_signatures_rec(next, visibility_map, sigs);
            }
        }
        Expr::Let(_pat, _value, Some(next)) => {
            collect_fn_signatures_rec(next, visibility_map, sigs);
        }
        Expr::Then(e1, e2) => {
            collect_fn_signatures_rec(e1, visibility_map, sigs);
            if let Some(e2) = e2 {
                collect_fn_signatures_rec(e2, visibility_map, sigs);
            }
        }
        Expr::Bracket(inner) => {
            collect_fn_signatures_rec(inner, visibility_map, sigs);
        }
        Expr::Escape(inner) => {
            collect_fn_signatures_rec(inner, visibility_map, sigs);
        }
        _ => {}
    }
}

/// Walk the AST and collect all symbols visible at the top level or as locals.
fn collect_completion_symbols(
    root: ExprNodeId,
    visibility_map: &VisibilityMap,
) -> Vec<CompletionSymbol> {
    let mut syms = Vec::new();
    collect_completion_symbols_rec(root, visibility_map, &mut syms);
    // Deduplicate by name, keeping the first occurrence.
    let mut seen = std::collections::HashSet::new();
    syms.retain(|s| seen.insert(s.name.clone()));
    syms
}

/// Recursively walk the AST collecting bindings.
///
/// Collects:
/// - `LetRec` bindings: functions (Lambda) and plain values
/// - `Let` bindings: variable patterns (including tuple destructuring)
/// - Lambda parameter names from function bodies
fn collect_completion_symbols_rec(
    expr_id: ExprNodeId,
    visibility_map: &VisibilityMap,
    syms: &mut Vec<CompletionSymbol>,
) {
    match expr_id.to_expr() {
        Expr::LetRec(typed_id, value, next) => {
            let type_str = format_type_for_display(&typed_id.ty.to_type());
            let (kind, type_str) = if let Expr::Lambda(_params, ret_ty, body) = value.to_expr() {
                // Do not collect lambda parameters here: they are local to the
                // function body and not visible in the surrounding scope.
                // Recurse into the body to pick up any nested let-bindings.
                collect_completion_symbols_rec(body, visibility_map, syms);
                let ret = ret_ty
                    .map(|r| format_type_for_display(&r.to_type()))
                    .unwrap_or_default();
                (CompletionKind::Function, ret)
            } else {
                (CompletionKind::Variable, type_str)
            };
            if is_visible(typed_id.id.as_str(), visibility_map) {
                syms.push(CompletionSymbol {
                    name: demangle_name(typed_id.id.as_str()),
                    kind,
                    type_str,
                });
            }
            if let Some(next) = next {
                collect_completion_symbols_rec(next, visibility_map, syms);
            }
        }
        Expr::Let(typed_pattern, _value, next) => {
            collect_pattern_symbols(
                &typed_pattern.pat,
                &format_type_for_display(&typed_pattern.ty.to_type()),
                syms,
            );
            if let Some(next) = next {
                collect_completion_symbols_rec(next, visibility_map, syms);
            }
        }
        Expr::Then(e1, e2) => {
            collect_completion_symbols_rec(e1, visibility_map, syms);
            if let Some(e2) = e2 {
                collect_completion_symbols_rec(e2, visibility_map, syms);
            }
        }
        Expr::Bracket(inner) | Expr::Escape(inner) => {
            collect_completion_symbols_rec(inner, visibility_map, syms);
        }
        _ => {}
    }
}

/// Recursively extract bound names from a pattern.
fn collect_pattern_symbols(pattern: &Pattern, type_str: &str, syms: &mut Vec<CompletionSymbol>) {
    match pattern {
        Pattern::Single(sym) => {
            syms.push(CompletionSymbol {
                name: sym.as_str().to_string(),
                kind: CompletionKind::Variable,
                type_str: type_str.to_string(),
            });
        }
        Pattern::Tuple(pats) => {
            for p in pats {
                collect_pattern_symbols(p, "", syms);
            }
        }
        Pattern::Record(fields) => {
            for (name, pat) in fields {
                // Prefer the field name as label; recurse if it binds further.
                collect_pattern_symbols(pat, "", syms);
                // If the sub-pattern is just Placeholder, still expose the field name.
                if matches!(pat, Pattern::Placeholder) {
                    syms.push(CompletionSymbol {
                        name: name.as_str().to_string(),
                        kind: CompletionKind::Variable,
                        type_str: String::new(),
                    });
                }
            }
        }
        Pattern::Placeholder | Pattern::Error => {}
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
