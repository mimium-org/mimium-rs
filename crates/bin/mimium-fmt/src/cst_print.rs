//! CST-based pretty printer with comment-aware width calculation
//!
//! This module provides pretty printing directly from the CST (Concrete Syntax Tree),
//! ensuring that comments are included in width calculations for proper line breaking.

use std::path::PathBuf;

use mimium_lang::compiler::parser::{
    parse_cst, preparse, tokenize, GreenNodeArena, GreenNodeId, PreParsedTokens, SyntaxKind,
    Token, TokenKind,
};
use mimium_lang::utils::error::ReportableError;
use pretty::{Arena, DocAllocator, DocBuilder, Pretty};

use crate::GLOBAL_DATA;

fn get_indent_size() -> usize {
    if let Ok(gdata) = GLOBAL_DATA.try_lock() {
        gdata.indent_size
    } else {
        4
    }
}

/// Pretty print source code using CST-based approach with comment-aware width calculation
pub fn pretty_print(
    src: &str,
    _file_path: &Option<PathBuf>,
    width: usize,
) -> Result<String, Vec<Box<dyn ReportableError>>> {
    let tokens = tokenize(src);
    let preparsed = preparse(&tokens);

    let (cst_root, arena, _tokens, errors) = parse_cst(tokens.clone(), &preparsed);

    if !errors.is_empty() {
        // For now, return a simple error message
        // TODO: Convert CST parser errors to reportable errors properly
        return Err(vec![]);
    }

    let doc_allocator: Arena<'_, ()> = Arena::new();
    let ctx = PrintContext {
        arena: &arena,
        tokens: &tokens,
        preparsed: &preparsed,
        source: src,
    };

    let doc = cst_to_doc::<_, ()>(cst_root, &ctx, &doc_allocator);

    let mut output = Vec::new();
    doc.render(width, &mut output).unwrap();
    let mut formatted = String::from_utf8(output).unwrap();
    
    // Ensure output ends with a trailing newline
    if !formatted.ends_with('\n') {
        formatted.push('\n');
    }
    
    Ok(formatted)
}

/// Context for CST printing
struct PrintContext<'a> {
    arena: &'a GreenNodeArena,
    tokens: &'a [Token],
    preparsed: &'a PreParsedTokens,
    source: &'a str,
}

/// Helper to create a breakable comma separator
fn breakable_comma<'a, D, A>(allocator: &'a D) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone,
{
    allocator.text(",").append(allocator.line())
}

/// Convert a CST node to a pretty document
fn cst_to_doc<'a, D, A>(node_id: GreenNodeId, ctx: &PrintContext, allocator: &'a D) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    let node = ctx.arena.get(node_id);

    match node {
        mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } => {
            // Token node - emit the token text with its trivia
            emit_token_with_trivia(*token_index, ctx, allocator)
        }
        mimium_lang::compiler::parser::green::GreenNode::Internal { kind, children, .. } => {
            // Internal node - dispatch based on syntax kind
            match kind {
                SyntaxKind::Program => print_program(children, ctx, allocator),
                SyntaxKind::FunctionDecl => print_function_decl(children, ctx, allocator),
                SyntaxKind::LetDecl => print_let_decl(children, ctx, allocator),
                SyntaxKind::LetRecDecl => print_letrec_decl(children, ctx, allocator),
                SyntaxKind::BinaryExpr => print_binary_expr(children, ctx, allocator),
                SyntaxKind::UnaryExpr => print_unary_expr(children, ctx, allocator),
                SyntaxKind::CallExpr => print_call_expr(children, ctx, allocator),
                SyntaxKind::LambdaExpr => print_lambda_expr(children, ctx, allocator),
                SyntaxKind::IfExpr => print_if_expr(children, ctx, allocator),
                SyntaxKind::BlockExpr => print_block_expr(children, ctx, allocator),
                SyntaxKind::TupleExpr => print_tuple_expr(children, ctx, allocator),
                SyntaxKind::RecordExpr => print_record_expr(children, ctx, allocator),
                SyntaxKind::ArrayExpr => print_array_expr(children, ctx, allocator),
                SyntaxKind::ParenExpr => print_paren_expr(children, ctx, allocator),
                SyntaxKind::FieldAccess => print_field_access(children, ctx, allocator),
                SyntaxKind::IndexExpr => print_index_expr(children, ctx, allocator),
                SyntaxKind::MacroExpansion => print_macro_expansion(children, ctx, allocator),
                SyntaxKind::EscapeExpr => print_escape_expr(children, ctx, allocator),
                SyntaxKind::BracketExpr => print_bracket_expr(children, ctx, allocator),
                SyntaxKind::IncludeStmt => print_include_stmt(children, ctx, allocator),
                SyntaxKind::StageDecl => print_stage_decl(children, ctx, allocator),
                SyntaxKind::TypeAnnotation => print_type_annotation(children, ctx, allocator),
                SyntaxKind::ParamList => print_param_list(children, ctx, allocator),
                SyntaxKind::ArgList => print_arg_list(children, ctx, allocator),
                SyntaxKind::Pattern => print_pattern(children, ctx, allocator),
                SyntaxKind::SinglePattern => print_single_pattern(children, ctx, allocator),
                SyntaxKind::TuplePattern => print_tuple_pattern(children, ctx, allocator),
                SyntaxKind::RecordPattern => print_record_pattern(children, ctx, allocator),
                // Literals
                SyntaxKind::IntLiteral
                | SyntaxKind::FloatLiteral
                | SyntaxKind::StringLiteral
                | SyntaxKind::SelfLiteral
                | SyntaxKind::NowLiteral
                | SyntaxKind::SampleRateLiteral
                | SyntaxKind::PlaceHolderLiteral
                | SyntaxKind::Identifier => print_leaf_children(children, ctx, allocator),
                // Types
                SyntaxKind::PrimitiveType
                | SyntaxKind::UnitType
                | SyntaxKind::TypeIdent => print_leaf_children(children, ctx, allocator),
                SyntaxKind::FunctionType => print_function_type(children, ctx, allocator),
                SyntaxKind::TupleType => print_tuple_type(children, ctx, allocator),
                SyntaxKind::RecordType => print_record_type(children, ctx, allocator),
                SyntaxKind::ArrayType => print_array_type(children, ctx, allocator),
                SyntaxKind::CodeType => print_code_type(children, ctx, allocator),
                // Statement wrapper
                SyntaxKind::Statement => print_statement(children, ctx, allocator),
                SyntaxKind::AssignExpr => print_assign_expr(children, ctx, allocator),
                SyntaxKind::ExprList => print_expr_list(children, ctx, allocator),
                SyntaxKind::ParamDefault => print_param_default(children, ctx, allocator),
                SyntaxKind::Error => allocator.text("/* error */"),
            }
        }
    }
}

/// Find the preparsed index for an original token index
fn find_preparsed_index(token_index: usize, preparsed: &PreParsedTokens) -> Option<usize> {
    preparsed
        .token_indices
        .iter()
        .position(|&idx| idx == token_index)
}

/// Emit a token with its leading and trailing trivia
fn emit_token_with_trivia<'a, D, A>(
    token_index: usize,
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone,
{
    let token = &ctx.tokens[token_index];
    let token_text = token.text(ctx.source);

    // Find preparsed index for this token
    let preparsed_idx = find_preparsed_index(token_index, ctx.preparsed);

    let mut doc = allocator.nil();

    // Emit leading trivia
    if let Some(idx) = preparsed_idx {
        let leading = ctx.preparsed.get_leading_trivia(idx, ctx.tokens);
        for trivia in leading {
            doc = doc.append(emit_trivia(trivia, ctx.source, allocator));
        }
    }

    // Emit the token itself
    doc = doc.append(allocator.text(token_text.to_string()));

    // Emit trailing trivia
    if let Some(idx) = preparsed_idx {
        let trailing = ctx.preparsed.get_trailing_trivia(idx, ctx.tokens);
        for trivia in trailing {
            doc = doc.append(emit_trivia(trivia, ctx.source, allocator));
        }
    }

    doc
}

/// Emit a trivia token (comment or whitespace)
fn emit_trivia<'a, D, A>(
    trivia: &Token,
    source: &str,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone,
{
    match trivia.kind {
        TokenKind::SingleLineComment => {
            // Single-line comment - include the text and add hardline
            let text = trivia.text(source);
            allocator.text(" ").append(allocator.text(text.to_string())).append(allocator.hardline())
        }
        TokenKind::MultiLineComment => {
            // Block comment - include as-is with surrounding space
            let text = trivia.text(source);
            allocator.text(" ").append(allocator.text(text.to_string())).append(allocator.text(" "))
        }
        TokenKind::LineBreak => {
            // Line break - convert to hardline
            allocator.hardline()
        }
        TokenKind::Whitespace => {
            // Skip whitespace trivia - let the formatting logic handle spacing
            allocator.nil()
        }
        _ => allocator.nil(),
    }
}

/// Print all children as concatenated tokens (for leaf-like nodes)
fn print_leaf_children<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    let docs: Vec<_> = children
        .iter()
        .map(|&child| cst_to_doc(child, ctx, allocator))
        .collect();
    allocator.concat(docs)
}

// ============================================================================
// Program and top-level statements
// ============================================================================

fn print_program<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    let stmt_docs: Vec<_> = children
        .iter()
        .map(|&child| cst_to_doc(child, ctx, allocator))
        .collect();
    allocator.intersperse(stmt_docs, allocator.hardline())
}

fn print_statement<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // Statement is a wrapper - just print children
    print_leaf_children(children, ctx, allocator)
}

fn print_include_stmt<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // include("path")
    print_leaf_children(children, ctx, allocator)
}

fn print_stage_decl<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // #stage(main)
    print_leaf_children(children, ctx, allocator)
}

// ============================================================================
// Declarations
// ============================================================================

fn print_function_decl<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // fn name(params) { body } or fn name(params)->type { body }
    // Structure: fn, name_token, ParamList, optional -> and type, BlockExpr or expr
    let mut result = allocator.nil();
    let mut seen_fn = false;
    let mut seen_name = false;
    
    for &child in children.iter() {
        let node = ctx.arena.get(child);
        
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];
            
            match token.kind {
                TokenKind::Function => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    result = result.append(allocator.space());
                    seen_fn = true;
                    continue;
                }
                TokenKind::Ident | TokenKind::IdentFunction => {
                    if seen_fn && !seen_name {
                        result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                        seen_name = true;
                        continue;
                    }
                }
                TokenKind::Arrow => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    continue;
                }
                _ => {}
            }
        }
        
        // For internal nodes (ParamList, BlockExpr, type nodes)
        if let mimium_lang::compiler::parser::green::GreenNode::Internal { kind, .. } = node {
            let child_doc = cst_to_doc(child, ctx, allocator);
            
            match kind {
                SyntaxKind::ParamList => {
                    // Param list comes right after function name (no space)
                    result = result.append(child_doc);
                }
                SyntaxKind::BlockExpr => {
                    // Add space before block
                    result = result.append(allocator.space()).append(child_doc);
                }
                _ => {
                    // Type nodes, etc.
                    result = result.append(child_doc);
                }
            }
            continue;
        }
        
        // Default: just append
        result = result.append(cst_to_doc(child, ctx, allocator));
    }
    
    result
}

fn print_let_decl<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // let pattern = expr
    // Structure: let, pattern, =, expr(s)
    let mut result = allocator.nil();
    let mut seen_let = false;
    let mut seen_pattern = false;
    let mut seen_eq = false;
    let mut rhs_started = false;
    
    for &child in children.iter() {
        let node = ctx.arena.get(child);
        
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];
            
            match token.kind {
                TokenKind::Let => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    result = result.append(allocator.space());
                    seen_let = true;
                    continue;
                }
                TokenKind::Assign => {
                    result = result.append(allocator.space());
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    result = result.append(allocator.space());
                    seen_eq = true;
                    continue;
                }
                _ => {}
            }
        }
        
        let child_doc = cst_to_doc(child, ctx, allocator);
        
        if seen_let && !seen_eq {
            // Pattern - just concatenate (pattern might have multiple children due to CST structure)
            if !seen_pattern {
                result = result.append(child_doc);
                seen_pattern = true;
            } else {
                result = result.append(child_doc);
            }
        } else if seen_eq {
            // RHS expression(s) - concatenate without extra spacing
            if !rhs_started {
                result = result.append(child_doc);
                rhs_started = true;
            } else {
                result = result.append(child_doc);
            }
        }
    }
    
    result.group()
}

fn print_letrec_decl<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // letrec name = expr
    // Structure: letrec, identifier, =, expr(s)
    let mut result = allocator.nil();
    let mut seen_letrec = false;
    let mut seen_name = false;
    let mut seen_eq = false;
    let mut rhs_started = false;
    
    for &child in children.iter() {
        let node = ctx.arena.get(child);
        
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];
            
            match token.kind {
                TokenKind::LetRec => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    result = result.append(allocator.space());
                    seen_letrec = true;
                    continue;
                }
                TokenKind::Assign => {
                    result = result.append(allocator.space());
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    result = result.append(allocator.space());
                    seen_eq = true;
                    continue;
                }
                _ => {}
            }
        }
        
        let child_doc = cst_to_doc(child, ctx, allocator);
        
        if seen_letrec && !seen_eq {
            // Name/pattern
            if !seen_name {
                result = result.append(child_doc);
                seen_name = true;
            } else {
                result = result.append(child_doc);
            }
        } else if seen_eq {
            // RHS expression(s)
            if !rhs_started {
                result = result.append(child_doc);
                rhs_started = true;
            } else {
                result = result.append(child_doc);
            }
        }
    }
    
    result.group()
}

fn print_assign_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // lhs = rhs
    print_leaf_children(children, ctx, allocator)
}

// ============================================================================
// Expressions
// ============================================================================

fn print_binary_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // lhs op rhs - add spaces around operator
    let mut result = allocator.nil();
    
    for &child in children.iter() {
        let node = ctx.arena.get(child);
        
        // Check if this is an operator token
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];
            let is_operator = matches!(
                token.kind,
                TokenKind::OpSum
                    | TokenKind::OpMinus
                    | TokenKind::OpProduct
                    | TokenKind::OpDivide
                    | TokenKind::OpModulo
                    | TokenKind::OpExponent
                    | TokenKind::OpAnd
                    | TokenKind::OpOr
                    | TokenKind::OpEqual
                    | TokenKind::OpNotEqual
                    | TokenKind::OpLessThan
                    | TokenKind::OpGreaterThan
                    | TokenKind::OpLessEqual
                    | TokenKind::OpGreaterEqual
                    | TokenKind::OpAt
                    | TokenKind::OpPipe
            );
            
            if is_operator {
                // Add space before and after operator
                result = result
                    .append(allocator.space())
                    .append(emit_token_with_trivia(*token_index, ctx, allocator))
                    .append(allocator.space());
                continue;
            }
        }
        
        // For expressions, just append
        let child_doc = cst_to_doc(child, ctx, allocator);
        result = result.append(child_doc);
    }
    
    result.group()
}

fn print_unary_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // op expr
    print_leaf_children(children, ctx, allocator)
}

fn print_call_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // callee(args) or callee(args) @ time
    // Structure: callee, ArgList, (optional: @, time_expr)
    let mut result = allocator.nil();
    
    for &child in children.iter() {
        let node = ctx.arena.get(child);
        
        // Check if this is an ArgList node - format specially
        if let mimium_lang::compiler::parser::green::GreenNode::Internal { kind, .. } = node {
            if *kind == SyntaxKind::ArgList {
                result = result.append(cst_to_doc(child, ctx, allocator));
                continue;
            }
        }
        
        // For other children (callee, @, time), concatenate directly
        result = result.append(cst_to_doc(child, ctx, allocator));
    }
    
    result.group()
}

fn print_lambda_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // |params| body or |params|->return_type body
    // Structure: |, param tokens (with optional :type), commas, |, optional -> and return_type, body_expr(s)
    //
    // Note: Due to CST parser structure, body expressions may be split into multiple children.
    // For example, `x+1` becomes Identifier(x) followed by BinaryExpr(+, 1) as siblings.
    // We need to concatenate all body children without adding extra spaces between them.
    
    let mut result = allocator.nil();
    let mut in_params = false;
    let mut params_docs = Vec::new();
    let mut current_param = allocator.nil();
    let mut has_param_content = false;
    let mut after_params = false;
    let mut body_started = false;
    
    for &child in children.iter() {
        let node = ctx.arena.get(child);
        
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];
            
            match token.kind {
                TokenKind::LambdaArgBeginEnd => {
                    if !in_params && !after_params {
                        // Opening |
                        result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                        in_params = true;
                    } else if in_params {
                        // Closing | - flush the current param if any
                        if has_param_content {
                            params_docs.push(current_param.clone());
                        }
                        
                        // Join params with comma and space
                        let params_combined = if params_docs.is_empty() {
                            allocator.nil()
                        } else {
                            allocator.intersperse(params_docs.clone(), allocator.text(", "))
                        };
                        
                        result = result.append(params_combined);
                        result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                        in_params = false;
                        after_params = true;
                        params_docs.clear();
                        current_param = allocator.nil();
                        has_param_content = false;
                    }
                    continue;
                }
                TokenKind::Comma if in_params => {
                    // Comma separates params
                    if has_param_content {
                        params_docs.push(current_param.clone());
                        current_param = allocator.nil();
                        has_param_content = false;
                    }
                    continue;
                }
                TokenKind::Arrow if after_params => {
                    // Return type annotation
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    continue;
                }
                _ => {}
            }
        }
        
        let child_doc = cst_to_doc(child, ctx, allocator);
        
        if in_params {
            // Accumulate parameter tokens
            current_param = current_param.append(child_doc);
            has_param_content = true;
        } else if after_params {
            // Body expression(s) - add space only before the FIRST body child
            if !body_started {
                result = result.append(allocator.space()).append(child_doc);
                body_started = true;
            } else {
                // Subsequent body children are concatenated without extra space
                // (they're part of the same logical expression)
                result = result.append(child_doc);
            }
        }
    }
    
    result.group()
}

fn print_if_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // if cond then_expr [else alt_expr]
    // Structure: if, condition_expr, then_expr, (optional: else, alt_expr or nested IfExpr)
    let mut result = allocator.nil();
    let mut seen_if = false;
    let mut seen_cond = false;
    let mut seen_then = false;
    let mut seen_else = false;
    
    for &child in children.iter() {
        let node = ctx.arena.get(child);
        
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];
            
            match token.kind {
                TokenKind::If => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    seen_if = true;
                    continue;
                }
                TokenKind::Else => {
                    result = result
                        .append(allocator.space())
                        .append(emit_token_with_trivia(*token_index, ctx, allocator));
                    seen_else = true;
                    continue;
                }
                _ => {}
            }
        }
        
        // Process expression children
        let child_doc = cst_to_doc(child, ctx, allocator);
        
        if !seen_cond && seen_if {
            // This is the condition expression (already includes parens if present in source)
            result = result.append(child_doc.group());
            seen_cond = true;
        } else if !seen_then && seen_cond {
            // This is the then branch
            result = result.append(allocator.space()).append(child_doc.group());
            seen_then = true;
        } else if seen_else {
            // This is the else branch (could be nested IfExpr for else if)
            result = result.append(allocator.space()).append(child_doc.group());
        }
    }
    
    result.group()
}

fn print_block_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // { stmts }
    // Structure: {, statements..., }
    
    let mut result = allocator.nil();
    let mut in_body = false;
    let mut body_docs = Vec::new();

    for &child in children.iter() {
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } =
            ctx.arena.get(child)
        {
            let token = &ctx.tokens[*token_index];
            match token.kind {
                TokenKind::BlockBegin => {
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    in_body = true;
                    continue;
                }
                TokenKind::BlockEnd => {
                    // Build body with indentation
                    if !body_docs.is_empty() {
                        let body = allocator.concat(body_docs.clone());
                        // Put hardline + body inside nest so indentation applies
                        result = result.append(
                            allocator.hardline()
                                .append(body)
                                .nest(get_indent_size() as isize)
                        );
                        result = result.append(allocator.hardline());
                    }
                    result = result.append(emit_token_with_trivia(*token_index, ctx, allocator));
                    in_body = false;
                    continue;
                }
                _ => {}
            }
        }

        if in_body {
            let child_doc = cst_to_doc(child, ctx, allocator);
            body_docs.push(child_doc);
        }
    }

    result
}

fn print_tuple_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (e1, e2, ...)
    print_grouped_list(children, ctx, allocator, "(", ")")
}

fn print_record_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // {field = val, ...}
    print_grouped_list(children, ctx, allocator, "{", "}")
}

fn print_array_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // [e1, e2, ...]
    print_grouped_list(children, ctx, allocator, "[", "]")
}

fn print_paren_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (expr)
    print_leaf_children(children, ctx, allocator)
}

fn print_field_access<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // expr.field
    print_leaf_children(children, ctx, allocator)
}

fn print_index_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // expr.0
    print_leaf_children(children, ctx, allocator)
}

fn print_macro_expansion<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // macro!(args)
    print_leaf_children(children, ctx, allocator)
}

fn print_escape_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // $expr
    print_leaf_children(children, ctx, allocator)
}

fn print_bracket_expr<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // `expr
    print_leaf_children(children, ctx, allocator)
}

// ============================================================================
// Types
// ============================================================================

fn print_type_annotation<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // : type
    print_leaf_children(children, ctx, allocator)
}

fn print_function_type<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (params) -> return
    print_leaf_children(children, ctx, allocator)
}

fn print_tuple_type<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (t1, t2)
    print_grouped_list(children, ctx, allocator, "(", ")")
}

fn print_record_type<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // {field: type, ...}
    print_grouped_list(children, ctx, allocator, "{", "}")
}

fn print_array_type<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // [type; size]
    print_leaf_children(children, ctx, allocator)
}

fn print_code_type<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // `type
    print_leaf_children(children, ctx, allocator)
}

// ============================================================================
// Patterns
// ============================================================================

fn print_pattern<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    print_leaf_children(children, ctx, allocator)
}

fn print_single_pattern<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // name: type
    print_leaf_children(children, ctx, allocator)
}

fn print_tuple_pattern<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (p1, p2)
    print_grouped_list(children, ctx, allocator, "(", ")")
}

fn print_record_pattern<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // {field, ...}
    print_grouped_list(children, ctx, allocator, "{", "}")
}

// ============================================================================
// Lists
// ============================================================================

fn print_param_list<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (p1, p2, ...)
    print_grouped_list(children, ctx, allocator, "(", ")")
}

fn print_arg_list<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // (a1, a2, ...)
    print_grouped_list(children, ctx, allocator, "(", ")")
}

fn print_expr_list<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // e1, e2, ...
    let docs: Vec<_> = children
        .iter()
        .map(|&child| cst_to_doc(child, ctx, allocator))
        .collect();
    allocator.concat(docs)
}

fn print_param_default<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    print_leaf_children(children, ctx, allocator)
}

// ============================================================================
// Helper functions
// ============================================================================

/// Print a grouped list with delimiters (parens, braces, brackets)
/// Handles indentation for multi-line lists
fn print_grouped_list<'a, D, A>(
    children: &[GreenNodeId],
    ctx: &PrintContext,
    allocator: &'a D,
    _open: &str,
    _close: &str,
) -> DocBuilder<'a, D, A>
where
    D: DocAllocator<'a, A>,
    D::Doc: Clone + Pretty<'a, D, A>,
    A: Clone,
{
    // Collect items between delimiters, excluding commas
    let mut items = Vec::new();
    let mut open_doc = allocator.nil();
    let mut close_doc = allocator.nil();
    let mut found_open = false;

    for &child in children.iter() {
        let node = ctx.arena.get(child);
        if let mimium_lang::compiler::parser::green::GreenNode::Token { token_index, .. } = node {
            let token = &ctx.tokens[*token_index];
            match token.kind {
                TokenKind::ParenBegin | TokenKind::BlockBegin | TokenKind::ArrayBegin => {
                    open_doc = emit_token_with_trivia(*token_index, ctx, allocator);
                    found_open = true;
                    continue;
                }
                TokenKind::ParenEnd | TokenKind::BlockEnd | TokenKind::ArrayEnd => {
                    close_doc = emit_token_with_trivia(*token_index, ctx, allocator);
                    continue;
                }
                TokenKind::Comma => {
                    // Skip commas - we'll add them with proper breaking
                    continue;
                }
                _ => {}
            }
        }

        if found_open {
            items.push(cst_to_doc(child, ctx, allocator));
        }
    }

    if items.is_empty() {
        open_doc.append(close_doc)
    } else {
        let items_doc = allocator.intersperse(items, breakable_comma(allocator));
        open_doc
            .append(items_doc.nest(get_indent_size() as isize).group())
            .append(close_doc)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_function() {
        let src = "fn dsp() { 42 }";
        let result = pretty_print(src, &None, 80);
        assert!(result.is_ok());
    }

    #[test]
    fn test_with_comment() {
        let src = "let x = 1 // comment\nlet y = 2";
        let result = pretty_print(src, &None, 80);
        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.contains("// comment"));
    }
}
