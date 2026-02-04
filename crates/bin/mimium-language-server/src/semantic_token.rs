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
        | TokenKind::Error
        | TokenKind::Eof => return None,
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

fn is_trivia_node(node_id: GreenNodeId, arena: &GreenNodeArena, tokens: &[Token]) -> bool {
    matches!(arena.get(node_id), GreenNode::Token { token_index, .. } if tokens[*token_index].is_trivia())
}

fn mark_identifiers_as_function(
    node_id: GreenNodeId,
    arena: &GreenNodeArena,
    tokens: &[Token],
    token_types: &mut [Option<usize>],
    function_id: usize,
) {
    match arena.get(node_id) {
        GreenNode::Token { token_index, .. } => {
            if matches!(
                tokens[*token_index].kind,
                TokenKind::Ident
                    | TokenKind::IdentFunction
                    | TokenKind::IdentVariable
                    | TokenKind::IdentParameter
            ) {
                token_types[*token_index] = Some(function_id);
            }
        }
        GreenNode::Internal { children, .. } => {
            children.iter().for_each(|child| {
                mark_identifiers_as_function(*child, arena, tokens, token_types, function_id)
            });
        }
    }
}

fn node_contains_pipe(node_id: GreenNodeId, arena: &GreenNodeArena, tokens: &[Token]) -> bool {
    match arena.get(node_id) {
        GreenNode::Token { token_index, .. } => tokens[*token_index].kind == TokenKind::OpPipe,
        GreenNode::Internal { children, .. } => children
            .iter()
            .any(|child| node_contains_pipe(*child, arena, tokens)),
    }
}

fn find_prev_non_trivia(
    children: &[GreenNodeId],
    idx: usize,
    arena: &GreenNodeArena,
    tokens: &[Token],
) -> Option<GreenNodeId> {
    children
        .iter()
        .take(idx)
        .rev()
        .find(|child| !is_trivia_node(**child, arena, tokens))
        .copied()
}

/// Mark identifiers in a QualifiedPath as namespace tokens, except the last one.
/// For `foo::bar::baz`, `foo` and `bar` become NAMESPACE, `baz` stays as VARIABLE/FUNCTION.
fn mark_qualified_path_namespaces(
    node_id: GreenNodeId,
    arena: &GreenNodeArena,
    tokens: &[Token],
    token_types: &mut [Option<usize>],
    namespace_id: usize,
) {
    // Collect all identifier tokens in this qualified path
    let mut ident_tokens: Vec<usize> = Vec::new();
    collect_ident_tokens(node_id, arena, tokens, &mut ident_tokens);
    
    // Mark all but the last identifier as namespace
    if ident_tokens.len() > 1 {
        for &token_idx in ident_tokens.iter().take(ident_tokens.len() - 1) {
            token_types[token_idx] = Some(namespace_id);
        }
    }
}

/// Mark all identifiers in a path as namespace tokens.
/// Used for `use foo::bar` where even `bar` should be namespace-colored in the use context.
fn mark_all_as_namespace(
    node_id: GreenNodeId,
    arena: &GreenNodeArena,
    tokens: &[Token],
    token_types: &mut [Option<usize>],
    namespace_id: usize,
) {
    let mut ident_tokens: Vec<usize> = Vec::new();
    collect_ident_tokens(node_id, arena, tokens, &mut ident_tokens);
    
    for &token_idx in &ident_tokens {
        token_types[token_idx] = Some(namespace_id);
    }
}

/// Collect all identifier token indices from a node tree.
fn collect_ident_tokens(
    node_id: GreenNodeId,
    arena: &GreenNodeArena,
    tokens: &[Token],
    result: &mut Vec<usize>,
) {
    match arena.get(node_id) {
        GreenNode::Token { token_index, .. } => {
            if matches!(
                tokens[*token_index].kind,
                TokenKind::Ident
                    | TokenKind::IdentFunction
                    | TokenKind::IdentVariable
                    | TokenKind::IdentParameter
            ) {
                result.push(*token_index);
            }
        }
        GreenNode::Internal { children, .. } => {
            for child in children {
                collect_ident_tokens(*child, arena, tokens, result);
            }
        }
    }
}

/// Mark the module name identifier as namespace in module declarations.
fn mark_module_name(
    children: &[GreenNodeId],
    arena: &GreenNodeArena,
    tokens: &[Token],
    token_types: &mut [Option<usize>],
    namespace_id: usize,
) {
    // Find the first identifier token which is the module name
    if let Some(token_index) = children.iter().find_map(|child| match arena.get(*child) {
        GreenNode::Token { token_index, .. }
            if matches!(
                tokens[*token_index].kind,
                TokenKind::Ident | TokenKind::IdentFunction | TokenKind::IdentVariable
            ) =>
        {
            Some(*token_index)
        }
        _ => None,
    }) {
        token_types[token_index] = Some(namespace_id);
    }
}

fn mark_function_decl_name(
    children: &[GreenNodeId],
    arena: &GreenNodeArena,
    tokens: &[Token],
    token_types: &mut [Option<usize>],
    function_id: usize,
) {
    if let Some(token_index) = children.iter().find_map(|child| match arena.get(*child) {
        GreenNode::Token { token_index, .. }
            if matches!(
                tokens[*token_index].kind,
                TokenKind::Ident | TokenKind::IdentFunction
            ) =>
        {
            Some(*token_index)
        }
        _ => None,
    }) {
        token_types[token_index] = Some(function_id);
    }
}

fn apply_contextual_semantics(
    node_id: GreenNodeId,
    arena: &GreenNodeArena,
    tokens: &[Token],
    token_types: &mut [Option<usize>],
    function_id: usize,
    namespace_id: usize,
) {
    if let Some(children) = arena.children(node_id) {
        match arena.kind(node_id) {
            Some(SyntaxKind::FunctionDecl) => {
                mark_function_decl_name(children, arena, tokens, token_types, function_id);
            }
            Some(SyntaxKind::ModuleDecl) => {
                // Mark the module name as namespace
                mark_module_name(children, arena, tokens, token_types, namespace_id);
            }
            Some(SyntaxKind::UseStmt) => {
                // Mark all identifiers in use statement as namespace
                for child in children {
                    if arena.kind(*child) == Some(SyntaxKind::QualifiedPath) {
                        mark_all_as_namespace(*child, arena, tokens, token_types, namespace_id);
                    }
                }
            }
            _ => {}
        }

        for (idx, child) in children.iter().enumerate() {
            if let Some(kind) = arena.kind(*child) {
                match kind {
                    SyntaxKind::CallExpr => {
                        if let Some(prev) = find_prev_non_trivia(children, idx, arena, tokens) {
                            // Check if prev is a QualifiedPath - mark namespaces first
                            if arena.kind(prev) == Some(SyntaxKind::QualifiedPath) {
                                mark_qualified_path_namespaces(
                                    prev,
                                    arena,
                                    tokens,
                                    token_types,
                                    namespace_id,
                                );
                            }
                            mark_identifiers_as_function(
                                prev,
                                arena,
                                tokens,
                                token_types,
                                function_id,
                            );
                        }
                    }
                    SyntaxKind::BinaryExpr if node_contains_pipe(*child, arena, tokens) => {
                        if let Some(prev) = find_prev_non_trivia(children, idx, arena, tokens) {
                            // Check if prev is a QualifiedPath - mark namespaces first
                            if arena.kind(prev) == Some(SyntaxKind::QualifiedPath) {
                                mark_qualified_path_namespaces(
                                    prev,
                                    arena,
                                    tokens,
                                    token_types,
                                    namespace_id,
                                );
                            }
                            mark_identifiers_as_function(
                                prev,
                                arena,
                                tokens,
                                token_types,
                                function_id,
                            );
                        }
                    }
                    SyntaxKind::QualifiedPath => {
                        // For standalone qualified paths (not in call/use context), 
                        // mark the namespace parts
                        mark_qualified_path_namespaces(
                            *child,
                            arena,
                            tokens,
                            token_types,
                            namespace_id,
                        );
                    }
                    _ => {}
                }
            }
            apply_contextual_semantics(*child, arena, tokens, token_types, function_id, namespace_id);
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

    let function_id = get_token_id(&SemanticTokenType::FUNCTION);
    let namespace_id = get_token_id(&SemanticTokenType::NAMESPACE);
    apply_contextual_semantics(root, arena, tokens, &mut token_types, function_id, namespace_id);

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

    let (ast, _module_env, _module_info, parse_errs) = parse_to_expr(src, Some(PathBuf::from(uri)));

    ParseResult {
        ast,
        semantic_tokens,
        errors: parse_errs,
    }
}
