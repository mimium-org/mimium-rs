use std::path::PathBuf;

use mimium_lang::lossless_parser::{
    parse_program_lossless, parser_errors_to_reportable, preparse, tokenize, LosslessToken,
    PreParsedTokens, TokenKind,
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
    let (prog, parse_errs) = parse_program_lossless(src, file_path.clone().unwrap_or_default());
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

fn extract_file_leading_comments(source: &str, tokens: &[LosslessToken]) -> String {
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

fn collect_gap_comments(
    source: &str,
    tokens: &[LosslessToken],
    preparsed: &PreParsedTokens,
) -> Vec<Vec<(TokenKind, String)>> {
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

        let comments = tokens[start..end]
            .iter()
            .filter(|t| matches!(t.kind, TokenKind::SingleLineComment | TokenKind::MultiLineComment))
            .map(|t| (t.kind, t.text(source).to_string()))
            .collect::<Vec<_>>();

        if !comments.is_empty() {
            result[gap_idx] = comments;
        }
    }

    result
}

fn insert_gap_comments(formatted: &str, gap_comments: &[Vec<(TokenKind, String)>]) -> String {
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
                let mut j = token_idx + 1;

                for (kind, text) in &gap_comments[nontrivia_idx] {
                    match kind {
                        TokenKind::SingleLineComment => {
                            output.push_str(text);
                            // Skip whitespace until linebreak
                            while j < fmt_tokens.len()
                                && matches!(fmt_tokens[j].kind, TokenKind::Whitespace)
                            {
                                j += 1;
                            }
                            if j < fmt_tokens.len()
                                && matches!(fmt_tokens[j].kind, TokenKind::LineBreak)
                            {
                                output.push_str(fmt_tokens[j].text(formatted));
                                j += 1;
                                while j < fmt_tokens.len()
                                    && matches!(fmt_tokens[j].kind, TokenKind::Whitespace)
                                {
                                    output.push_str(fmt_tokens[j].text(formatted));
                                    j += 1;
                                }
                            }
                        }
                        TokenKind::MultiLineComment => {
                            if !ends_with_whitespace(&output) {
                                output.push(' ');
                            }
                            output.push_str(text);

                            while j < fmt_tokens.len()
                                && matches!(fmt_tokens[j].kind, TokenKind::Whitespace)
                            {
                                j += 1;
                            }
                            if j < fmt_tokens.len()
                                && matches!(fmt_tokens[j].kind, TokenKind::LineBreak)
                            {
                                output.push_str(fmt_tokens[j].text(formatted));
                                j += 1;
                                while j < fmt_tokens.len()
                                    && matches!(fmt_tokens[j].kind, TokenKind::Whitespace)
                                {
                                    output.push_str(fmt_tokens[j].text(formatted));
                                    j += 1;
                                }
                            } else {
                                output.push(' ');
                                while j < fmt_tokens.len()
                                    && matches!(fmt_tokens[j].kind, TokenKind::Whitespace)
                                {
                                    j += 1;
                                }
                            }
                        }
                        _ => {}
                    }
                }

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

fn ends_with_whitespace(value: &str) -> bool {
    value
        .chars()
        .last()
        .is_some_and(|ch| ch == ' ' || ch == '\t' || ch == '\n')
}
