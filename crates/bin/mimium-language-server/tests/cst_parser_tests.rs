//! Integration tests for CST Parser
//!
//! This module contains comprehensive tests for the CST parser implementation.
//! Tests are organized by feature area:
//! - Basic parsing (programs, statements)
//! - Literals (int, float, string, special)
//! - Expressions (binary, unary, function calls)
//! - Type annotations
//! - Complex expressions (tuples, records, arrays)
//! - Control flow (if/else)
//! - Phase 2 features (field access, indexing, assignment)
//! - Error recovery

use mimium_language_server::lossless_parser::cst_parser::*;
use mimium_language_server::lossless_parser::green::SyntaxKind;
use mimium_language_server::lossless_parser::preparser::preparse;
use mimium_language_server::lossless_parser::tokenizer::tokenize;

#[test]
fn test_parse_simple_program() {
    let source = "fn dsp() { 42 }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert_eq!(arena.kind(root_id), Some(SyntaxKind::Program));
    assert!(arena.children(root_id).is_some());
    assert!(
        errors.is_empty(),
        "Expected no errors, got: {}",
        errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("; ")
    );
}

#[test]
fn test_parse_let_statement() {
    let source = "let x = 42";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    let children = arena.children(root_id).unwrap();
    assert!(!children.is_empty());
    assert!(errors.is_empty(), "Expected no errors, got {:?}", errors);
}

#[test]
fn test_parse_function() {
    let source = "fn add(x, y) { x }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(arena.width(root_id) > 0);
    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_tuple() {
    let source = "(1, 2, 3)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    let children = arena.children(root_id).unwrap();
    assert!(!children.is_empty());
    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_record() {
    let source = "{x = 1, y = 2}";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    let children = arena.children(root_id).unwrap();
    assert!(!children.is_empty());
    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_if_expr() {
    let source = "if x { 1 } else { 2 }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(arena.width(root_id) > 0);
    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_block() {
    let source = "{ let x = 1 }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(arena.width(root_id) > 0);
    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
}

#[test]
fn test_parse_error_recovery_unexpected_token() {
    // Invalid syntax: missing closing brace
    let source = "fn test() { 42";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (_root_id, _arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    // Should have collected an error
    assert!(!errors.is_empty(), "Expected parse errors, got none");
    // The error might be UnexpectedEof instead of UnexpectedToken since we reach EOF
    assert!(
        matches!(errors[0].detail, ErrorDetail::UnexpectedToken { .. })
            || matches!(errors[0].detail, ErrorDetail::UnexpectedEof { .. }),
        "Expected UnexpectedToken or UnexpectedEof, got: {:?}",
        errors[0].detail
    );
}

#[test]
fn test_parse_error_recovery_missing_block() {
    // Missing expression in let binding
    let source = "let x =";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (_root_id, _arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    // Should have collected an error when EOF is reached after '='
    assert!(
        !errors.is_empty(),
        "Expected parse errors for incomplete let binding"
    );
    // Error should be about unexpected EOF expecting an expression
    assert!(
        matches!(&errors[0].detail, ErrorDetail::UnexpectedEof { expected } if expected == "expression"),
        "Expected UnexpectedEof for 'expression', got: {:?}",
        errors[0].detail
    );
    // Parser should continue without crashing
    assert!(_arena.kind(_root_id).is_some());
}

#[test]
fn test_parse_type_annotation_simple() {
    let source = "fn add(x: float, y: float) { x }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_lambda_with_type() {
    let source = "|x: float| -> float x";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::LambdaExpr);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_int_literal() {
    let source = "42";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_identifier() {
    let source = "variable_name";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::Identifier);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_float_literal() {
    let source = "3.14";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::FloatLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_type_annotation_function_param() {
    // Converted from: test_typed_param() - simple parameter with type
    let source = "fn test(x: float) { x }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::FunctionDecl);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::TypeAnnotation);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_lambda_simple() {
    // Converted from: test_lambda() - simple lambda expression
    let source = "|x| x";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::LambdaExpr);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_tuple_simple() {
    // Converted from: test_tuple() - simple tuple expression
    let source = "(1, 2)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::TupleExpr);
    // Verify two IntLiterals in the tuple
    assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 2);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_tuple_nested() {
    // Converted from: test_nested_tuple() - nested tuple expression
    let source = "((1, 2), 3)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::TupleExpr);
    // Verify at least two TupleExpr nodes (outer and inner)
    assert!(count_nodes_by_kind(&arena, root_id, SyntaxKind::TupleExpr) >= 2);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_record_simple() {
    // Converted from: test_record_simple() - simple record with fields
    let source = "{x = 1, y = 2}";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::RecordExpr);
    // Should have 2 IntLiterals (values)
    assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 2);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_type_annotation_primitive() {
    // Converted from: test_type_annotation() - primitive types
    let source = "fn test(x: float, y: int) { x }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::TypeAnnotation);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::PrimitiveType);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_type_tuple() {
    // Converted from: test_tuple_type() - tuple type annotation
    let source = "fn test(x: (float, int)) { x }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::TupleType);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_type_function() {
    // Converted from: test_function_type() - function type annotation
    let source = "fn apply(f: (float) -> float) { f }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::FunctionType);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_type_record() {
    // Converted from: test_record_type() - record type annotation
    let source = "fn test(p: {x: float, y: float}) { p }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::RecordType);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_if_else_expr() {
    // Converted from: test_if_else() - if-else expression
    let source = "if x { 1 } else { 2 }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IfExpr);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_binary_add() {
    // Converted from binary operator tests
    let source = "1 + 2";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BinaryExpr);
    assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 2);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_binary_multiply() {
    let source = "3 * 4";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BinaryExpr);
    assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 2);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_binary_precedence() {
    // Test operator precedence: 1 + 2 * 3 should parse as 1 + (2 * 3)
    let source = "1 + 2 * 3";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    // Should have two BinaryExpr nodes (one for +, one for *)
    assert!(count_nodes_by_kind(&arena, root_id, SyntaxKind::BinaryExpr) >= 2);
    assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 3);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_binary_complex_precedence() {
    // More complex precedence test
    let source = "1 + 2 * 3 + 4";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BinaryExpr);
    assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 4);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_unary_minus() {
    let source = "-42";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::UnaryExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_unary_nested() {
    let source = "--42";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    // Should have nested UnaryExpr nodes
    assert!(count_nodes_by_kind(&arena, root_id, SyntaxKind::UnaryExpr) >= 2);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_call_simple() {
    let source = "f(x)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::CallExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::ArgList);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_call_multiple_args() {
    let source = "add(1, 2, 3)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::CallExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::ArgList);
    assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 3);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_call_nested() {
    let source = "f(g(x))";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    // Should have two CallExpr nodes (f and g)
    assert!(count_nodes_by_kind(&arena, root_id, SyntaxKind::CallExpr) >= 2);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_call_with_binary_expr() {
    let source = "f(x + 1)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::CallExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BinaryExpr);
    assert!(arena.width(root_id) > 0);
}

// Phase 2 tests: Array literals, field access, indexing, special literals

#[test]
fn test_parse_array_literal() {
    let source = "[1, 2, 3]";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::ArrayExpr);
    assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 3);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_array_empty() {
    let source = "[]";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::ArrayExpr);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_field_access_simple() {
    let source = "obj.field";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::FieldAccess);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_field_access_chained() {
    let source = "obj.field1.field2";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    // Should have two FieldAccess nodes for chained access
    assert!(count_nodes_by_kind(&arena, root_id, SyntaxKind::FieldAccess) >= 2);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_array_indexing_simple() {
    let source = "arr[0]";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IndexExpr);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_array_indexing_with_expression() {
    let source = "arr[i + 1]";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IndexExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BinaryExpr);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_special_literal_self() {
    let source = "self";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::SelfLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_special_literal_now() {
    let source = "now";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::NowLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_special_literal_samplerate() {
    let source = "samplerate";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::SampleRateLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_complex_expression_mixed() {
    // arr[0].field |> f(x + 1)
    let source = "arr[0].field |> f(x + 1)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    // Should have indexing, field access, pipe, call, and binary operations
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IndexExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::FieldAccess);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::CallExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BinaryExpr);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_array_of_tuples() {
    let source = "[(1, 2), (3, 4)]";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::ArrayExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::TupleExpr);
    assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 4);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_record_with_field_access() {
    let source = "{x = arr[0]}";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::RecordExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IndexExpr);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_assignment_simple() {
    let source = "x = 5";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::AssignExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::Identifier);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_assignment_field_access() {
    let source = "obj.field = 10";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::AssignExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::FieldAccess);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_assignment_array_index() {
    let source = "arr[0] = 42";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::AssignExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IndexExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    // Should have at least 2 IntLiteral nodes: arr[0] and 42
    assert_node_count(&arena, root_id, SyntaxKind::IntLiteral, 2);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_assignment_complex_expression() {
    let source = "x = y + 1";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::AssignExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BinaryExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    assert!(arena.width(root_id) > 0);
}

// Phase 3 tests: Pattern matching

#[test]
fn test_parse_pattern_single() {
    let source = "let x = 42";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::LetDecl);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::SinglePattern);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_pattern_tuple() {
    let source = "let (x, y) = (1, 2)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::LetDecl);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::TuplePattern);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::TupleExpr);
    // Should have 2 SinglePattern nodes within the TuplePattern
    assert!(count_nodes_by_kind(&arena, root_id, SyntaxKind::SinglePattern) >= 2);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_pattern_tuple_nested() {
    let source = "let ((a, b), c) = ((1, 2), 3)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::LetDecl);
    // Should have nested TuplePattern nodes
    assert!(count_nodes_by_kind(&arena, root_id, SyntaxKind::TuplePattern) >= 2);
    assert!(count_nodes_by_kind(&arena, root_id, SyntaxKind::SinglePattern) >= 3);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_pattern_record() {
    let source = "let {a = x, b = y} = record";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::LetDecl);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::RecordPattern);
    // Should have 2 SinglePattern nodes (x and y)
    assert_node_count(&arena, root_id, SyntaxKind::SinglePattern, 2);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_pattern_record_nested() {
    let source = "let {a = (x, y), b = z} = record";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::LetDecl);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::RecordPattern);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::TuplePattern);
    // Should have 3 SinglePattern nodes (x, y, z)
    assert_node_count(&arena, root_id, SyntaxKind::SinglePattern, 3);
    assert!(arena.width(root_id) > 0);
}

// Phase 4 tests: Include and Stage declarations

#[test]
fn test_parse_include_simple() {
    let source = r#"include("file.mmm")"#;
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IncludeStmt);
    // String literal should be present as Str token
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_include_with_path() {
    let source = r#"include("libs/osc.mmm")"#;
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IncludeStmt);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_stage_main() {
    let source = "#stage(main)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::StageDecl);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_stage_macro() {
    let source = "#stage(macro)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::StageDecl);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_program_with_include() {
    let source = r#"include("osc.mmm")
fn dsp() { 0.0 }"#;
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IncludeStmt);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::FunctionDecl);
    assert!(arena.width(root_id) > 0);
}

// Phase 4b tests: Macro expansion and Bracket/Escape

#[test]
fn test_parse_macro_expansion_simple() {
    let source = r#"Slider!("freq", 100, 1, 200)"#;
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::MacroExpansion);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_macro_expansion_no_args() {
    let source = "Macro!()";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::MacroExpansion);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_bracket_expr_simple() {
    let source = "`x";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BracketExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::Identifier);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_bracket_expr_block() {
    let source = "`{x + 1}";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BracketExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BlockExpr);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_escape_expr() {
    let source = "$x";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::EscapeExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::Identifier);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_bracket_escape_combined() {
    let source = "`($x + 1)";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BracketExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::EscapeExpr);
    assert!(arena.width(root_id) > 0);
}

// Phase 5 tests: Enhanced features

#[test]
fn test_parse_projection_tuple() {
    let source = "pair.0";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::FieldAccess);
    // Projection is recognized as field access with numeric index
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_projection_second() {
    let source = "triple.1";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::FieldAccess);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_else_if_chain() {
    let source = "if a { 1 } else if b { 2 } else { 3 }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    // Should have multiple IfExpr nodes (nested)
    assert!(count_nodes_by_kind(&arena, root_id, SyntaxKind::IfExpr) >= 2);
    assert!(count_nodes_by_kind(&arena, root_id, SyntaxKind::IntLiteral) >= 3);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_else_if_simple() {
    let source = "if x { 1 } else if y { 2 }";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IfExpr);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_placeholder_in_expr() {
    let source = "_ + 1";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::BinaryExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::Identifier);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_placeholder_tuple() {
    let source = "let (_, x) = pair";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::TuplePattern);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::SinglePattern);
    assert!(arena.width(root_id) > 0);
}

// Phase 6: Optional Advanced Features

#[test]
fn test_parse_record_update_simple() {
    let source = "let r2 = {r <- x = 10}";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::RecordExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::Identifier);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_record_update_with_fields() {
    let source = "let r2 = {r <- x = 10, y = 20}";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::RecordExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::Identifier);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    assert!(arena.width(root_id) > 0);
}

#[test]
fn test_parse_incomplete_record() {
    let source = "let r = {x = 10, y = 20, ..}";
    let tokens = tokenize(source);
    let preparsed = preparse(&tokens);
    let (root_id, arena, _tokens, errors) = parse_cst(tokens, &preparsed);

    assert!(errors.is_empty(), "Expected no errors, got {errors:?}");
    use mimium_language_server::lossless_parser::cst_test_helpers::*;
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::RecordExpr);
    assert_cst_contains_kind(&arena, root_id, SyntaxKind::IntLiteral);
    assert!(arena.width(root_id) > 0);
}
