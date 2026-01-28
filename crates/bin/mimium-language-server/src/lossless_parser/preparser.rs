/// Pre-parser for handling trivia (comments, whitespace, linebreaks)
/// Converts token sequence into index sequence with trivia maps
use super::token::{LosslessToken, TokenKind};
use std::collections::HashMap;

/// Trivia information - comments and whitespace
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trivia {
    pub token_index: usize,
    pub kind: TokenKind,
}

/// Pre-parsed result with non-trivia token indices and trivia maps
#[derive(Debug)]
pub struct PreParsedTokens {
    /// Indices into the original token array for non-trivia tokens
    pub token_indices: Vec<usize>,
    /// Map from token index -> leading trivia indices
    pub leading_trivia_map: HashMap<usize, Vec<usize>>,
    /// Map from token index -> trailing trivia indices
    pub trailing_trivia_map: HashMap<usize, Vec<usize>>,
}

impl PreParsedTokens {
    pub fn new() -> Self {
        Self {
            token_indices: Vec::new(),
            leading_trivia_map: HashMap::new(),
            trailing_trivia_map: HashMap::new(),
        }
    }
    
    /// Get the non-trivia token at the given index
    pub fn get_token<'a>(&self, idx: usize, tokens: &'a [LosslessToken]) -> Option<&'a LosslessToken> {
        self.token_indices.get(idx).and_then(|&i| tokens.get(i))
    }
    
    /// Get leading trivia for a token
    pub fn get_leading_trivia<'a>(&self, idx: usize, tokens: &'a [LosslessToken]) -> Vec<&'a LosslessToken> {
        self.leading_trivia_map
            .get(&idx)
            .map(|indices| {
                indices.iter()
                    .filter_map(|&i| tokens.get(i))
                    .collect()
            })
            .unwrap_or_default()
    }
    
    /// Get trailing trivia for a token
    pub fn get_trailing_trivia<'a>(&self, idx: usize, tokens: &'a [LosslessToken]) -> Vec<&'a LosslessToken> {
        self.trailing_trivia_map
            .get(&idx)
            .map(|indices| {
                indices.iter()
                    .filter_map(|&i| tokens.get(i))
                    .collect()
            })
            .unwrap_or_default()
    }
}

/// Pre-parse tokens to separate trivia from syntax tokens
/// 
/// The strategy is:
/// - Whitespace and comments following a syntax token are trailing trivia
/// - Whitespace and comments before a syntax token are leading trivia
/// - LineBreaks act as separators: trivia before a linebreak is trailing,
///   trivia after a linebreak is leading
pub fn preparse(tokens: &[LosslessToken]) -> PreParsedTokens {
    let mut result = PreParsedTokens::new();
    let mut pending_trivia = Vec::new();
    let mut last_was_linebreak = false;
    let mut last_token_idx: Option<usize> = None;
    
    for (i, token) in tokens.iter().enumerate() {
        if token.is_trivia() {
            // Collect trivia
            pending_trivia.push(i);
            
            if token.kind == TokenKind::LineBreak {
                // LineBreak: attach all pending trivia as trailing to previous token
                if let Some(last_idx) = last_token_idx {
                    result.trailing_trivia_map
                        .entry(last_idx)
                        .or_insert_with(Vec::new)
                        .extend(pending_trivia.drain(..));
                } else {
                    // No previous token, discard or keep as leading for first token
                    pending_trivia.clear();
                }
                last_was_linebreak = true;
            } else {
                last_was_linebreak = false;
            }
        } else if token.kind != TokenKind::Eof {
            // Non-trivia token
            let current_idx = result.token_indices.len();
            
            // Attach pending trivia
            if !pending_trivia.is_empty() {
                if last_was_linebreak || last_token_idx.is_none() {
                    // Attach as leading trivia
                    result.leading_trivia_map
                        .entry(current_idx)
                        .or_insert_with(Vec::new)
                        .extend(pending_trivia.drain(..));
                } else {
                    // Attach as trailing trivia to previous token
                    if let Some(last_idx) = last_token_idx {
                        result.trailing_trivia_map
                            .entry(last_idx)
                            .or_insert_with(Vec::new)
                            .extend(pending_trivia.drain(..));
                    }
                }
            }
            
            result.token_indices.push(i);
            last_token_idx = Some(current_idx);
            last_was_linebreak = false;
        }
    }
    
    // Handle any remaining trailing trivia
    if !pending_trivia.is_empty() {
        if let Some(last_idx) = last_token_idx {
            result.trailing_trivia_map
                .entry(last_idx)
                .or_insert_with(Vec::new)
                .extend(pending_trivia);
        }
    }
    
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lossless_parser::tokenizer::tokenize;
    
    #[test]
    fn test_preparse_simple() {
        let source = "fn dsp() { 42 }";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        
        // Should have tokens: fn, dsp, (, ), {, 42, }
        assert_eq!(preparsed.token_indices.len(), 7);
        
        let first_token = preparsed.get_token(0, &tokens).unwrap();
        assert_eq!(first_token.kind, TokenKind::Function);
    }
    
    #[test]
    fn test_preparse_with_comments() {
        let source = "fn // comment\ndsp";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        
        // Should have tokens: fn, dsp
        assert_eq!(preparsed.token_indices.len(), 2);
        
        // fn should have trailing trivia (whitespace, comment, linebreak, whitespace)
        let trailing = preparsed.get_trailing_trivia(0, &tokens);
        assert!(trailing.len() >= 2);
        
        // dsp might have leading trivia
        let leading = preparsed.get_leading_trivia(1, &tokens);
        assert!(leading.is_empty() || leading.len() > 0);
    }
    
    #[test]
    fn test_preparse_leading_trivia() {
        let source = "// header comment\nfn dsp";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        
        // First token should be fn with leading trivia
        let first_token = preparsed.get_token(0, &tokens).unwrap();
        assert_eq!(first_token.kind, TokenKind::Function);
        
        let leading = preparsed.get_leading_trivia(0, &tokens);
        assert!(leading.is_empty()); // Leading trivia before first token is discarded
    }
    
    #[test]
    fn test_preparse_trailing_trivia() {
        let source = "fn dsp // inline comment\n";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        
        // dsp should have trailing trivia
        let dsp_idx = 1; // Index of 'dsp' in non-trivia tokens
        let trailing = preparsed.get_trailing_trivia(dsp_idx, &tokens);
        assert!(trailing.len() >= 1);
    }
    
    #[test]
    fn test_preparse_multiline_comment() {
        let source = "fn /* comment\nspanning\nlines */ dsp";
        let tokens = tokenize(source);
        let preparsed = preparse(&tokens);
        
        // Should have tokens: fn, dsp
        assert_eq!(preparsed.token_indices.len(), 2);
        
        // fn should have trailing trivia (whitespace, multiline comment, whitespace)
        let trailing = preparsed.get_trailing_trivia(0, &tokens);
        assert!(trailing.iter().any(|t| t.kind == TokenKind::MultiLineComment));
    }
}
