/// Demonstration of error recovery in the lossless parser
///
/// Shows how the parser continues after encountering invalid characters
use mimium_language_server::lossless_parser;

fn main() {
    // Test error recovery with invalid characters (§ and ©)
    let source = "fn dsp() { let x = 42 § let y = 3.14 © }";

    println!("=== Error Recovery Demo ===\n");
    println!("Source with invalid characters:");
    println!("{}", source);
    println!();

    let tokens = lossless_parser::tokenize(source);

    println!("Tokens parsed (including errors):");
    for (i, token) in tokens.iter().enumerate() {
        if token.kind == lossless_parser::TokenKind::Eof {
            break;
        }
        let status = if token.is_error() {
            "ERROR"
        } else if token.is_trivia() {
            "trivia"
        } else {
            "ok"
        };
        println!(
            "  {:2}: {:20} @ {}:{:2} - {:10} [{}]",
            i,
            format!("{:?}", token.kind),
            token.start,
            token.length,
            format!("'{}'", token.text(source)),
            status
        );
    }

    let error_count = tokens.iter().filter(|t| t.is_error()).count();
    let valid_count = tokens
        .iter()
        .filter(|t| !t.is_error() && !t.is_trivia() && t.kind != lossless_parser::TokenKind::Eof)
        .count();

    println!();
    println!("Summary:");
    println!("  - Total errors recovered: {}", error_count);
    println!("  - Valid tokens parsed: {}", valid_count);
    println!("  - Parser continued successfully!");
    println!();
    println!("The parser can continue parsing after errors, collecting all valid");
    println!("tokens while marking invalid characters as Error tokens.");
}
