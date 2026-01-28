//! Parser for mimium language.
//!
//! This module provides the public parser API built on the Red-Green Syntax Tree.

pub use crate::parser_internal::cst_parser;
pub use crate::parser_internal::green;
pub use crate::parser_internal::green::{GreenNode, GreenNodeArena, GreenNodeId, SyntaxKind};
pub use crate::parser_internal::preparser::{preparse, PreParsedTokens};
pub use crate::parser_internal::red::{red_to_ast, AstNode, RedNode};
pub use crate::parser_internal::token::{Token, TokenKind};
pub use crate::parser_internal::tokenizer::tokenize;
pub use crate::parser_internal::{
    green_to_red, parse, parse_cst, parse_to_expr, parser_errors_to_reportable,
};
pub use crate::parser_internal::lower::add_global_context;

/// Parse source to a program with error collection.
pub fn parse_program(
    source: &str,
    file_path: std::path::PathBuf,
) -> (crate::ast::program::Program, Vec<cst_parser::ParserError>) {
    crate::parser_internal::lower::parse_program(source, file_path)
}
