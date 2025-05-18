use tower_lsp::lsp_types::*;

pub(crate) fn semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::KEYWORD,      // 0: fn, macro, let, if, etc
            SemanticTokenType::TYPE,         // 1: float, int, string, struct
            SemanticTokenType::OPERATOR,     // 2: +, -, *, /, etc
            SemanticTokenType::COMMENT,      // 3: // and /* */
            SemanticTokenType::NUMBER,       // 4: integers and floats
            SemanticTokenType::STRING,       // 5: "string literals"
            SemanticTokenType::VARIABLE,     // 6: user-defined identifiers
            SemanticTokenType::FUNCTION,     // 7: function names
            SemanticTokenType::PARAMETER,    // 8: function parameters
            SemanticTokenType::MACRO,        // 9: macro names
        ],
        token_modifiers: vec![],
    }
}

pub(crate) struct SemanticTokensBuilder {
    /// The list of tokens encoded as 5-tuples of integers
    data: Vec<u32>,
    /// Previous token's line number for delta encoding
    prev_line: u32,
    /// Previous token's character offset for delta encoding
    prev_char: u32,
}

impl SemanticTokensBuilder {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            prev_line: 0,
            prev_char: 0,
        }
    }

    pub fn push(&mut self, line: u32, start_char: u32, length: u32, token_type: u32, token_modifiers: u32) {
        // Calculate delta values
        let delta_line = line - self.prev_line;
        let delta_char = if delta_line == 0 {
            start_char - self.prev_char
        } else {
            start_char
        };

        // Store tokens in the LSP format (delta encoded)
        self.data.extend_from_slice(&[
            delta_line,
            delta_char,
            length,
            token_type,
            token_modifiers,
        ]);

        // Update previous position
        self.prev_line = line;
        self.prev_char = start_char;
    }

    pub fn build(self) -> SemanticTokens {
        // Convert each group of 5 numbers into a SemanticToken
        let tokens = self.data.chunks(5).map(|chunk| {
            SemanticToken {
                delta_line: chunk[0],
                delta_start: chunk[1],
                length: chunk[2],
                token_type: chunk[3],
                token_modifiers_bitset: chunk[4],
            }
        }).collect();

        SemanticTokens {
            result_id: None,
            data: tokens,
        }
    }
}

// Token type indices
pub(crate) const KEYWORD: u32 = 0;
pub(crate) const TYPE: u32 = 1;
pub(crate) const OPERATOR: u32 = 2;
pub(crate) const COMMENT: u32 = 3;
pub(crate) const NUMBER: u32 = 4;
pub(crate) const STRING: u32 = 5;
pub(crate) const VARIABLE: u32 = 6;
pub(crate) const FUNCTION: u32 = 7;
pub(crate) const PARAMETER: u32 = 8;
pub(crate) const MACRO: u32 = 9;
