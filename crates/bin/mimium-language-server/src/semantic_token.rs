use std::path::PathBuf;

use mimium_lang::{
    compiler::parser::{
        GreenNodeArena, GreenNodeId, SyntaxKind, Token, TokenKind, green::GreenNode,
    },
    interner::ExprNodeId,
    utils::error::ReportableError,
};
use tower_lsp::lsp_types::SemanticTokenType;

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
    SemanticTokenType::NAMESPACE,
];
pub struct ParseResult {
    pub ast: ExprNodeId,
    pub semantic_tokens: Vec<ImCompleteSemanticToken>,
    pub errors: Vec<Box<dyn ReportableError>>,
    pub module_info: mimium_lang::ast::program::ModuleInfo,
}

fn get_token_id(semt: &SemanticTokenType) -> usize {
    LEGEND_TYPE.iter().position(|item| item == semt).unwrap()
}

fn token_kind_to_semantic_index(kind: TokenKind) -> Option<usize> {
    let token_type = match kind {
        TokenKind::Function
        | TokenKind::Let
        | TokenKind::LetRec
        | TokenKind::If
        | TokenKind::Else
        | TokenKind::SelfLit
        | TokenKind::Now
        | TokenKind::SampleRate
        | TokenKind::Macro
        | TokenKind::Include
        | TokenKind::StageKwd
        | TokenKind::Mod
        | TokenKind::Use
        | TokenKind::Pub
        | TokenKind::Main => get_token_id(&SemanticTokenType::KEYWORD),
        TokenKind::IdentFunction => get_token_id(&SemanticTokenType::FUNCTION),
        TokenKind::IdentParameter => get_token_id(&SemanticTokenType::PARAMETER),
        TokenKind::Ident | TokenKind::IdentVariable => get_token_id(&SemanticTokenType::VARIABLE),
        TokenKind::Float | TokenKind::Int => get_token_id(&SemanticTokenType::NUMBER),
        TokenKind::Str => get_token_id(&SemanticTokenType::STRING),
        TokenKind::SingleLineComment | TokenKind::MultiLineComment => {
            get_token_id(&SemanticTokenType::COMMENT)
        }
        TokenKind::Assign
        | TokenKind::OpPipe
        | TokenKind::OpAt
        | TokenKind::OpSum
        | TokenKind::OpMinus
        | TokenKind::OpProduct
        | TokenKind::OpDivide
        | TokenKind::OpModulo
        | TokenKind::OpExponent
        | TokenKind::OpEqual
        | TokenKind::OpNotEqual
        | TokenKind::OpLessThan
        | TokenKind::OpLessEqual
        | TokenKind::OpGreaterThan
        | TokenKind::OpGreaterEqual
        | TokenKind::OpAnd
        | TokenKind::OpOr
        | TokenKind::OpUnknown
        | TokenKind::Arrow
        | TokenKind::LeftArrow
        | TokenKind::LambdaArgBeginEnd
        | TokenKind::BackQuote
        | TokenKind::Dollar => get_token_id(&SemanticTokenType::OPERATOR),
        TokenKind::FloatType
        | TokenKind::IntegerType
        | TokenKind::StringType
        | TokenKind::StructType => get_token_id(&SemanticTokenType::TYPE),
        TokenKind::MacroExpand => get_token_id(&SemanticTokenType::MACRO),
        TokenKind::Whitespace
        | TokenKind::LineBreak
        | TokenKind::ParenBegin
        | TokenKind::ParenEnd
        | TokenKind::BlockBegin
        | TokenKind::BlockEnd
        | TokenKind::ArrayBegin
        | TokenKind::ArrayEnd
        | TokenKind::Comma
        | TokenKind::Colon
        | TokenKind::DoubleColon
        | TokenKind::SemiColon
        | TokenKind::Dot
        | TokenKind::DoubleDot
        | TokenKind::PlaceHolder
        | TokenKind::Sharp
        | TokenKind::FatArrow
        | TokenKind::Error
        | TokenKind::Eof => return None,
        TokenKind::Match => get_token_id(&SemanticTokenType::KEYWORD),
        TokenKind::Type => get_token_id(&SemanticTokenType::KEYWORD),
    };
    Some(token_type)
}

// Legacy function - no longer used with parser
// fn token_to_semantic_token(token: &Token, span: &Span) -> Option<ImCompleteSemanticToken> {
//     ...
// }

// No longer needed - token_kind_to_semantic_index handles this directly
// fn parser_token_to_semantic_token(token: &Token) -> Option<ImCompleteSemanticToken> {
//     token_kind_to_semantic_index(token.kind).map(|token_type| ImCompleteSemanticToken {
//         start: token.start,
//         length: token.length,
//         token_type,
//     })
// }

pub fn tokens_from_parser(tokens: &[Token]) -> Vec<ImCompleteSemanticToken> {
    tokens
        .iter()
        .filter_map(|token| {
            token_kind_to_semantic_index(token.kind).map(|token_type| ImCompleteSemanticToken {
                start: token.start,
                length: token.length,
                token_type,
            })
        })
        .collect()
}

/// Context for semantic token analysis.
/// Groups together all the data needed for contextual semantic highlighting.
struct SemanticContext<'a> {
    arena: &'a GreenNodeArena,
    tokens: &'a [Token],
    token_types: &'a mut [Option<usize>],
    function_id: usize,
    namespace_id: usize,
}

impl<'a> SemanticContext<'a> {
    fn new(
        arena: &'a GreenNodeArena,
        tokens: &'a [Token],
        token_types: &'a mut [Option<usize>],
    ) -> Self {
        Self {
            arena,
            tokens,
            token_types,
            function_id: get_token_id(&SemanticTokenType::FUNCTION),
            namespace_id: get_token_id(&SemanticTokenType::NAMESPACE),
        }
    }

    fn is_trivia(&self, node_id: GreenNodeId) -> bool {
        matches!(
            self.arena.get(node_id),
            GreenNode::Token { token_index, .. } if self.tokens[*token_index].is_trivia()
        )
    }

    fn is_identifier(&self, token_index: usize) -> bool {
        matches!(
            self.tokens[token_index].kind,
            TokenKind::Ident
                | TokenKind::IdentFunction
                | TokenKind::IdentVariable
                | TokenKind::IdentParameter
        )
    }

    fn mark_as_function(&mut self, node_id: GreenNodeId) {
        match self.arena.get(node_id) {
            GreenNode::Token { token_index, .. } => {
                if self.is_identifier(*token_index) {
                    self.token_types[*token_index] = Some(self.function_id);
                }
            }
            GreenNode::Internal { children, .. } => {
                let children = children.clone();
                for child in children {
                    self.mark_as_function(child);
                }
            }
        }
    }

    fn contains_pipe(&self, node_id: GreenNodeId) -> bool {
        match self.arena.get(node_id) {
            GreenNode::Token { token_index, .. } => {
                self.tokens[*token_index].kind == TokenKind::OpPipe
            }
            GreenNode::Internal { children, .. } => {
                children.iter().any(|child| self.contains_pipe(*child))
            }
        }
    }

    fn find_prev_non_trivia(&self, children: &[GreenNodeId], idx: usize) -> Option<GreenNodeId> {
        children
            .iter()
            .take(idx)
            .rev()
            .find(|child| !self.is_trivia(**child))
            .copied()
    }

    /// Collect all identifier token indices from a node tree.
    fn collect_ident_tokens(&self, node_id: GreenNodeId, result: &mut Vec<usize>) {
        match self.arena.get(node_id) {
            GreenNode::Token { token_index, .. } => {
                if self.is_identifier(*token_index) {
                    result.push(*token_index);
                }
            }
            GreenNode::Internal { children, .. } => {
                for child in children {
                    self.collect_ident_tokens(*child, result);
                }
            }
        }
    }

    /// Mark identifiers in a QualifiedPath as namespace tokens, except the last one.
    /// For `foo::bar::baz`, `foo` and `bar` become NAMESPACE, `baz` stays as VARIABLE/FUNCTION.
    fn mark_qualified_path_namespaces(&mut self, node_id: GreenNodeId) {
        let mut ident_tokens: Vec<usize> = Vec::new();
        self.collect_ident_tokens(node_id, &mut ident_tokens);

        // Mark all but the last identifier as namespace
        if ident_tokens.len() > 1 {
            for &token_idx in ident_tokens.iter().take(ident_tokens.len() - 1) {
                self.token_types[token_idx] = Some(self.namespace_id);
            }
        }
    }

    /// Mark all identifiers in a path as namespace tokens.
    /// Used for `use foo::bar` where even `bar` should be namespace-colored.
    fn mark_all_as_namespace(&mut self, node_id: GreenNodeId) {
        let mut ident_tokens: Vec<usize> = Vec::new();
        self.collect_ident_tokens(node_id, &mut ident_tokens);

        for &token_idx in &ident_tokens {
            self.token_types[token_idx] = Some(self.namespace_id);
        }
    }

    /// Mark the module name identifier as namespace in module declarations.
    fn mark_module_name(&mut self, children: &[GreenNodeId]) {
        if let Some(token_index) = children
            .iter()
            .find_map(|child| match self.arena.get(*child) {
                GreenNode::Token { token_index, .. }
                    if matches!(
                        self.tokens[*token_index].kind,
                        TokenKind::Ident | TokenKind::IdentFunction | TokenKind::IdentVariable
                    ) =>
                {
                    Some(*token_index)
                }
                _ => None,
            })
        {
            self.token_types[token_index] = Some(self.namespace_id);
        }
    }

    /// Mark the function name in function declarations.
    fn mark_function_decl_name(&mut self, children: &[GreenNodeId]) {
        if let Some(token_index) = children
            .iter()
            .find_map(|child| match self.arena.get(*child) {
                GreenNode::Token { token_index, .. }
                    if matches!(
                        self.tokens[*token_index].kind,
                        TokenKind::Ident | TokenKind::IdentFunction
                    ) =>
                {
                    Some(*token_index)
                }
                _ => None,
            })
        {
            self.token_types[token_index] = Some(self.function_id);
        }
    }
}

impl SemanticContext<'_> {
    /// Apply contextual semantics to the syntax tree.
    /// This method walks the tree and applies semantic token types based on context.
    fn apply(&mut self, node_id: GreenNodeId) {
        let Some(children) = self.arena.children(node_id) else {
            return;
        };
        let children = children.to_vec();

        match self.arena.kind(node_id) {
            Some(SyntaxKind::FunctionDecl) => {
                self.mark_function_decl_name(&children);
            }
            Some(SyntaxKind::ModuleDecl) => {
                self.mark_module_name(&children);
            }
            Some(SyntaxKind::UseStmt) => {
                for child in &children {
                    if self.arena.kind(*child) == Some(SyntaxKind::QualifiedPath) {
                        self.mark_all_as_namespace(*child);
                    }
                }
            }
            _ => {}
        }

        for (idx, child) in children.iter().enumerate() {
            if let Some(kind) = self.arena.kind(*child) {
                match kind {
                    SyntaxKind::CallExpr => {
                        if let Some(prev) = self.find_prev_non_trivia(&children, idx) {
                            if self.arena.kind(prev) == Some(SyntaxKind::QualifiedPath) {
                                self.mark_qualified_path_namespaces(prev);
                            }
                            self.mark_as_function(prev);
                        }
                    }
                    SyntaxKind::BinaryExpr if self.contains_pipe(*child) => {
                        if let Some(prev) = self.find_prev_non_trivia(&children, idx) {
                            if self.arena.kind(prev) == Some(SyntaxKind::QualifiedPath) {
                                self.mark_qualified_path_namespaces(prev);
                            }
                            self.mark_as_function(prev);
                        }
                    }
                    SyntaxKind::QualifiedPath => {
                        self.mark_qualified_path_namespaces(*child);
                    }
                    _ => {}
                }
            }
            self.apply(*child);
        }
    }
}

pub fn tokens_from_green(
    root: GreenNodeId,
    arena: &GreenNodeArena,
    tokens: &[Token],
) -> Vec<ImCompleteSemanticToken> {
    let mut token_types: Vec<Option<usize>> = tokens
        .iter()
        .map(|token| token_kind_to_semantic_index(token.kind))
        .collect();

    let mut ctx = SemanticContext::new(arena, tokens, &mut token_types);
    ctx.apply(root);

    tokens
        .iter()
        .enumerate()
        .filter_map(|(idx, token)| {
            token_types[idx].map(|token_type| ImCompleteSemanticToken {
                start: token.start,
                length: token.length,
                token_type,
            })
        })
        .collect()
}

pub fn parse(src: &str, uri: &str) -> ParseResult {
    use mimium_lang::compiler::parser::{parse_to_expr, tokenize};
    let tokens = tokenize(src);
    let semantic_tokens = tokens
        .iter()
        .filter_map(|token| {
            let span = token.start..token.end();
            token_kind_to_semantic_index(token.kind).map(|token_type| ImCompleteSemanticToken {
                start: span.start,
                length: span.end - span.start,
                token_type,
            })
        })
        .collect();

    let (ast, module_info, parse_errs) = parse_to_expr(src, Some(PathBuf::from(uri)));

    ParseResult {
        ast,
        semantic_tokens,
        errors: parse_errs,
        module_info,
    }
}
