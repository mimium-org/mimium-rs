use std::{collections::VecDeque, path::PathBuf};

use mimium_lang::{
    ast::{program::Program, statement::Statement},
    interner::ExprNodeId,
    pattern::TypedId,
    utils::{error::ReportableError, metadata::Span},
};
use tower_lsp::lsp_types::{SemanticTokenType, SemanticTokens};

/// Same as lstp_types::SemanticToken but lacks some fields like token_modifiers_bitset.
#[derive(Debug)]
pub struct ImCompleteSemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: usize,
}

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::COMMENT,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::MACRO,
    SemanticTokenType::TYPE,
    SemanticTokenType::TYPE_PARAMETER,
];
pub struct ParseResult {
    pub ast: ExprNodeId,
    pub semantic_tokens: Vec<ImCompleteSemanticToken>,
    pub errors: Vec<Box<dyn ReportableError>>,
}
use mimium_lang::compiler::parser::Token;

fn get_token_id(semt: &SemanticTokenType) -> usize {
    LEGEND_TYPE.iter().position(|item| item == semt).unwrap()
}
fn token_to_semantic_token(token: &Token, span: &Span) -> Option<ImCompleteSemanticToken> {
    let token_type = match token {
        Token::Function
        | Token::Let
        | Token::LetRec
        | Token::If
        | Token::Else
        | Token::SelfLit
        | Token::Now
        | Token::SampleRate => get_token_id(&SemanticTokenType::KEYWORD),
        Token::Ident(_) => get_token_id(&SemanticTokenType::VARIABLE),
        Token::Float(_) | Token::Int(_) => get_token_id(&SemanticTokenType::NUMBER),
        Token::Str(_) => get_token_id(&SemanticTokenType::STRING),
        Token::Comment(_) => get_token_id(&SemanticTokenType::COMMENT),
        Token::Assign | Token::Dollar | Token::BackQuote | Token::Op(_) => {
            get_token_id(&SemanticTokenType::OPERATOR)
        }
        Token::StringType | Token::IntegerType | Token::FloatType | Token::StructType => {
            get_token_id(&SemanticTokenType::TYPE)
        }
        Token::MacroExpand(_) => get_token_id(&SemanticTokenType::MACRO),

        _ => return None,
    };
    Some(ImCompleteSemanticToken {
        start: span.start,
        length: span.end - span.start,
        token_type,
    })
}

pub fn parse(src: &str, uri: &str) -> ParseResult {
    let (tokens, mut errs) = mimium_lang::compiler::parser::lex(src, Some(PathBuf::from(uri)));
    let semantic_token = if let Some(tks) = tokens {
        tks.iter()
            .filter_map(|(token, span)| token_to_semantic_token(token, &(span.start..span.end)))
            .collect()
    } else {
        Vec::new()
    };
    let (ast, parse_errs) =
        mimium_lang::compiler::parser::parse_to_expr(src, Some(PathBuf::from(uri)));
    errs.extend(parse_errs);
    ParseResult {
        ast,
        semantic_tokens: semantic_token,
        errors: errs,
    }
}
