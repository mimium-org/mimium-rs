use std::path::PathBuf;

use mimium_lang::compiler::parser::{
    parse_program, parser_errors_to_reportable, preparse, tokenize, PreParsedTokens, Token,
    TokenKind,
};
use mimium_lang::utils::error::ReportableError;
use pretty::Arena;

use crate::print;

pub fn pretty_print(
    src: &str,
    file_path: &Option<PathBuf>,
    width: usize,
) -> Result<String, Vec<Box<dyn ReportableError>>> {
    let tokens = tokenize(src);
    let preparsed = preparse(&tokens);
    let leading_comments = extract_file_leading_comments(src, &tokens);
    let (prog, parse_errs) = parse_program(src, file_path.clone().unwrap_or_default());
    let errs = parser_errors_to_reportable(
        src,
        file_path.clone().unwrap_or_default(),
        parse_errs,
    );
    if !errs.is_empty() {
        return Err(errs);
    }

    let allocator = Arena::new();
    let doc = print::program::pretty::<_, ()>(prog, &allocator);
    let mut w = Vec::new();
    doc.render(width, &mut w).unwrap();
    let mut formatted = String::from_utf8(w).unwrap();
    if formatted.ends_with('\n') {
        formatted.pop();
    }

    let gap_comments = collect_gap_comments(src, &tokens, &preparsed);
    let formatted_with_comments = insert_gap_comments(&formatted, &gap_comments);

    if leading_comments.is_empty() {
        Ok(formatted_with_comments)
    } else {
        Ok(format!("{leading_comments}{formatted_with_comments}"))
    }
}

fn extract_file_leading_comments(source: &str, tokens: &[Token]) -> String {
    let mut output = String::new();
    for token in tokens {
        if token.is_trivia() {
            if matches!(
                token.kind,
                TokenKind::SingleLineComment | TokenKind::MultiLineComment
            ) {
                output.push_str(token.text(source));
                output.push('\n');
            }
            continue;
        }
        break;
    }
    output
}

/// Represents a comment with its kind and whether a linebreak precedes it
#[derive(Debug, Clone)]
struct CommentInfo {
    kind: TokenKind,
    text: String,
    /// True if there was an explicit linebreak before this comment in source.
    /// For comments after single-line comments, this is false since the newline
    /// is implicit in the single-line comment itself.
    needs_leading_newline: bool,
}

fn collect_gap_comments(
    source: &str,
    tokens: &[Token],
    preparsed: &PreParsedTokens,
) -> Vec<Vec<CommentInfo>> {
    let indices = &preparsed.token_indices;
    if indices.len() < 2 {
        return Vec::new();
    }

    let mut result = vec![Vec::new(); indices.len() - 1];
    for gap_idx in 0..(indices.len() - 1) {
        let start = indices[gap_idx] + 1;
        let end = indices[gap_idx + 1];
        if start >= end || end > tokens.len() {
            continue;
        }

        let gap_tokens = &tokens[start..end];
        let mut comments = Vec::new();
        let mut had_explicit_linebreak = false;
        let mut after_single_line_comment = false;

        for t in gap_tokens {
            match t.kind {
                TokenKind::LineBreak => {
                    // Only count as explicit if not right after a single-line comment
                    if !after_single_line_comment {
                        had_explicit_linebreak = true;
                    }
                    after_single_line_comment = false;
                }
                TokenKind::SingleLineComment | TokenKind::MultiLineComment => {
                    comments.push(CommentInfo {
                        kind: t.kind,
                        text: t.text(source).to_string(),
                        needs_leading_newline: had_explicit_linebreak,
                    });
                    had_explicit_linebreak = false;
                    after_single_line_comment = t.kind == TokenKind::SingleLineComment;
                }
                TokenKind::Whitespace => {
                    // Don't reset linebreak state
                }
                _ => {}
            }
        }

        if !comments.is_empty() {
            result[gap_idx] = comments;
        }
    }

    result
}

fn insert_gap_comments(formatted: &str, gap_comments: &[Vec<CommentInfo>]) -> String {
    let fmt_tokens = tokenize(formatted);
    let fmt_preparsed = preparse(&fmt_tokens);
    let nontrivia_positions = &fmt_preparsed.token_indices;

    let mut output = String::new();
    let mut token_idx = 0usize;
    let mut nontrivia_idx = 0usize;

    while token_idx < fmt_tokens.len() {
        if nontrivia_idx < nontrivia_positions.len()
            && token_idx == nontrivia_positions[nontrivia_idx]
        {
            let token = fmt_tokens[token_idx];
            output.push_str(token.text(formatted));

            if nontrivia_idx < gap_comments.len() && !gap_comments[nontrivia_idx].is_empty() {
                // We need to insert comments after this token
                let comments = &gap_comments[nontrivia_idx];
                
                // Look ahead to skip trailing trivia
                let mut j = token_idx + 1;
                
                while j < fmt_tokens.len() {
                    let next = fmt_tokens[j];
                    match next.kind {
                        TokenKind::LineBreak | TokenKind::Whitespace => {
                            j += 1;
                        }
                        _ => break,
                    }
                }

                // Insert comments
                for (ci, comment) in comments.iter().enumerate() {
                    if comment.needs_leading_newline {
                        // Explicit linebreak from source - add it
                        output.push('\n');
                    } else if ci == 0 {
                        // First comment, no explicit linebreak
                        // Add space if inline (trailing comment on same line)
                        if !output.ends_with(' ') && !output.ends_with('\n') {
                            output.push(' ');
                        }
                    }
                    // For subsequent comments (ci > 0) without needs_leading_newline,
                    // we don't add anything because the previous single-line comment
                    // already includes implicit newline
                    
                    output.push_str(&comment.text);
                    
                    // Single-line comments implicitly end with newline
                    if comment.kind == TokenKind::SingleLineComment {
                        output.push('\n');
                    }
                }

                // Skip past the trivia we've handled
                token_idx = j;
                nontrivia_idx += 1;
                continue;
            }

            token_idx += 1;
            nontrivia_idx += 1;
            continue;
        }

        let token = fmt_tokens[token_idx];
        output.push_str(token.text(formatted));
        token_idx += 1;
    }

    output
}
