/// Example demonstrating the lossless parser
///
/// This file shows how to use the lossless parser for IDE features.
use mimium_language_server::lossless_parser;

fn main() {
    let source = r#"
// This is a comment
fn dsp() {
    let x = 42
    /* Multi-line
       comment */
    let y = 3.14
    x + y
}
"#;

    println!("=== Lossless Parser Demo ===\n");
    println!("Source code:\n{}\n", source);

    // Step 1: Tokenize
    println!("Step 1: Tokenization");
    let tokens = lossless_parser::tokenize(source);
    println!("Total tokens: {}", tokens.len());
    println!("First 10 tokens:");
    for (i, token) in tokens.iter().take(10).enumerate() {
        println!(
            "  {}: {} at {}:{} = {:?}",
            i,
            token.kind,
            token.start,
            token.length,
            token.text(source)
        );
    }
    println!();

    // Step 2: Pre-parse
    println!("Step 2: Pre-parsing (separating trivia)");
    let preparsed = lossless_parser::preparse(&tokens);
    println!("Non-trivia tokens: {}", preparsed.token_indices.len());
    println!(
        "Leading trivia map entries: {}",
        preparsed.leading_trivia_map.len()
    );
    println!(
        "Trailing trivia map entries: {}",
        preparsed.trailing_trivia_map.len()
    );
    println!();

    // Step 3: Parse to CST
    println!("Step 3: CST Parsing (Green Tree)");
    let (root_id, arena, annotated_tokens, errors) = lossless_parser::parse_cst(tokens, &preparsed);
    println!("Root node kind: {:?}", arena.kind(root_id));
    println!("Root node width: {}", arena.width(root_id));
    if !errors.is_empty() {
        println!("Parse errors: {errors:?}");
    }
    println!();

    // Step 4: Convert to AST
    println!("Step 4: AST Conversion (Red Tree)");
    let red = lossless_parser::green_to_red(root_id, 0);
    let ast = lossless_parser::red_to_ast(&red, source, &annotated_tokens, &arena);
    println!("AST: {:#?}", ast);

    println!("\n=== Demo Complete ===");
}
