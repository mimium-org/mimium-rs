/// Lossless tokenizer for mimium language
/// Converts source text into a sequence of position-aware tokens
use super::token::{LosslessToken, TokenKind};

/// Tokenize the source text into a sequence of lossless tokens
pub fn tokenize(source: &str) -> Vec<LosslessToken> {
    let mut tokens = Vec::new();
    let mut chars = source.char_indices().peekable();
    
    while let Some((start, ch)) = chars.next() {
        let token = match ch {
            // Whitespace (not including newlines)
            ' ' | '\t' | '\r' => {
                let mut end = start + ch.len_utf8();
                while let Some(&(pos, c)) = chars.peek() {
                    if c == ' ' || c == '\t' || c == '\r' {
                        chars.next();
                        end = pos + c.len_utf8();
                    } else {
                        break;
                    }
                }
                LosslessToken::new(TokenKind::Whitespace, start, end - start)
            }
            
            // Newlines
            '\n' => {
                let mut end = start + 1;
                while let Some(&(pos, c)) = chars.peek() {
                    if c == '\n' {
                        chars.next();
                        end = pos + 1;
                    } else {
                        break;
                    }
                }
                LosslessToken::new(TokenKind::LineBreak, start, end - start)
            }
            
            // Comments
            '/' if chars.peek().map(|(_, c)| *c) == Some('/') => {
                chars.next(); // consume second '/'
                let mut end = start + 2;
                while let Some(&(pos, c)) = chars.peek() {
                    if c == '\n' {
                        break;
                    }
                    chars.next();
                    end = pos + c.len_utf8();
                }
                LosslessToken::new(TokenKind::SingleLineComment, start, end - start)
            }
            
            '/' if chars.peek().map(|(_, c)| *c) == Some('*') => {
                chars.next(); // consume '*'
                let mut end = start + 2;
                let mut prev = ' ';
                while let Some((pos, c)) = chars.next() {
                    end = pos + c.len_utf8();
                    if prev == '*' && c == '/' {
                        break;
                    }
                    prev = c;
                }
                LosslessToken::new(TokenKind::MultiLineComment, start, end - start)
            }
            
            // String literals
            '"' => {
                let mut end = start + 1;
                let mut escaped = false;
                while let Some((pos, c)) = chars.next() {
                    end = pos + c.len_utf8();
                    if escaped {
                        escaped = false;
                    } else if c == '\\' {
                        escaped = true;
                    } else if c == '"' {
                        break;
                    }
                }
                LosslessToken::new(TokenKind::Str, start, end - start)
            }
            
            // Numbers
            '0'..='9' => {
                let mut end = start + ch.len_utf8();
                let mut has_dot = false;
                
                while let Some(&(pos, c)) = chars.peek() {
                    match c {
                        '0'..='9' => {
                            chars.next();
                            end = pos + c.len_utf8();
                        }
                        '.' => {
                            // Look ahead one more character to see if it's a digit
                            // We need to clone to peek further without consuming
                            let mut chars_clone = chars.clone();
                            chars_clone.next(); // Skip the '.'
                            if let Some(&(_, next_c)) = chars_clone.peek() {
                                if next_c.is_ascii_digit() && !has_dot {
                                    has_dot = true;
                                    chars.next();
                                    end = pos + 1;
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                        _ => break,
                    }
                }
                
                let kind = if has_dot { TokenKind::Float } else { TokenKind::Int };
                LosslessToken::new(kind, start, end - start)
            }
            
            // Identifiers and keywords
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut end = start + ch.len_utf8();
                while let Some(&(pos, c)) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' {
                        chars.next();
                        end = pos + c.len_utf8();
                    } else {
                        break;
                    }
                }
                
                let text = &source[start..end];
                let kind = match text {
                    "float" => TokenKind::FloatType,
                    "int" => TokenKind::IntegerType,
                    "string" => TokenKind::StringType,
                    "struct" => TokenKind::StructType,
                    "self" => TokenKind::SelfLit,
                    "now" => TokenKind::Now,
                    "samplerate" => TokenKind::SampleRate,
                    "let" => TokenKind::Let,
                    "letrec" => TokenKind::LetRec,
                    "fn" => TokenKind::Function,
                    "macro" => TokenKind::Macro,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "include" => TokenKind::Include,
                    "stage" => TokenKind::StageKwd,
                    "main" => TokenKind::Main,
                    "_" => TokenKind::PlaceHolder,
                    _ => {
                        // Check for macro expansion (identifier followed by !)
                        if let Some(&(_, '!')) = chars.peek() {
                            chars.next();
                            end += 1;
                            TokenKind::MacroExpand
                        } else {
                            TokenKind::Ident
                        }
                    }
                };
                
                LosslessToken::new(kind, start, end - start)
            }
            
            // Operators and punctuation
            '+' => LosslessToken::new(TokenKind::OpSum, start, 1),
            '*' => LosslessToken::new(TokenKind::OpProduct, start, 1),
            '%' => LosslessToken::new(TokenKind::OpModulo, start, 1),
            '^' => LosslessToken::new(TokenKind::OpExponent, start, 1),
            
            '-' => {
                if chars.peek().map(|(_, c)| *c) == Some('>') {
                    chars.next();
                    LosslessToken::new(TokenKind::Arrow, start, 2)
                } else {
                    LosslessToken::new(TokenKind::OpMinus, start, 1)
                }
            }
            
            '/' => LosslessToken::new(TokenKind::OpDivide, start, 1),
            
            '=' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    LosslessToken::new(TokenKind::OpEqual, start, 2)
                } else {
                    LosslessToken::new(TokenKind::Assign, start, 1)
                }
            }
            
            '!' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    LosslessToken::new(TokenKind::OpNotEqual, start, 2)
                } else {
                    // Standalone '!' - treat as unknown operator for now
                    // In actual mimium syntax, '!' is used for macro expansion
                    // which is handled in the identifier pattern
                    LosslessToken::new(TokenKind::OpUnknown, start, 1)
                }
            }
            
            '<' => {
                match chars.peek().map(|(_, c)| *c) {
                    Some('-') => {
                        chars.next();
                        LosslessToken::new(TokenKind::LeftArrow, start, 2)
                    }
                    Some('=') => {
                        chars.next();
                        LosslessToken::new(TokenKind::OpLessEqual, start, 2)
                    }
                    _ => LosslessToken::new(TokenKind::OpLessThan, start, 1),
                }
            }
            
            '>' => {
                if chars.peek().map(|(_, c)| *c) == Some('=') {
                    chars.next();
                    LosslessToken::new(TokenKind::OpGreaterEqual, start, 2)
                } else {
                    LosslessToken::new(TokenKind::OpGreaterThan, start, 1)
                }
            }
            
            '&' => {
                if chars.peek().map(|(_, c)| *c) == Some('&') {
                    chars.next();
                    LosslessToken::new(TokenKind::OpAnd, start, 2)
                } else {
                    LosslessToken::new(TokenKind::OpUnknown, start, 1)
                }
            }
            
            '|' => {
                match chars.peek().map(|(_, c)| *c) {
                    Some('|') => {
                        chars.next();
                        LosslessToken::new(TokenKind::OpOr, start, 2)
                    }
                    Some('>') => {
                        chars.next();
                        LosslessToken::new(TokenKind::OpPipe, start, 2)
                    }
                    _ => LosslessToken::new(TokenKind::LambdaArgBeginEnd, start, 1),
                }
            }
            
            '@' => LosslessToken::new(TokenKind::OpAt, start, 1),
            
            ',' => LosslessToken::new(TokenKind::Comma, start, 1),
            
            '.' => {
                if chars.peek().map(|(_, c)| *c) == Some('.') {
                    chars.next();
                    LosslessToken::new(TokenKind::DoubleDot, start, 2)
                } else {
                    LosslessToken::new(TokenKind::Dot, start, 1)
                }
            }
            
            ':' => LosslessToken::new(TokenKind::Colon, start, 1),
            ';' => LosslessToken::new(TokenKind::SemiColon, start, 1),
            
            '(' => LosslessToken::new(TokenKind::ParenBegin, start, 1),
            ')' => LosslessToken::new(TokenKind::ParenEnd, start, 1),
            '[' => LosslessToken::new(TokenKind::ArrayBegin, start, 1),
            ']' => LosslessToken::new(TokenKind::ArrayEnd, start, 1),
            '{' => LosslessToken::new(TokenKind::BlockBegin, start, 1),
            '}' => LosslessToken::new(TokenKind::BlockEnd, start, 1),
            
            '`' => LosslessToken::new(TokenKind::BackQuote, start, 1),
            '$' => LosslessToken::new(TokenKind::Dollar, start, 1),
            '#' => LosslessToken::new(TokenKind::Sharp, start, 1),
            
            _ => continue, // Skip unknown characters
        };
        
        tokens.push(token);
    }
    
    // Add EOF token
    tokens.push(LosslessToken::new(TokenKind::Eof, source.len(), 0));
    
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_tokenize_simple() {
        let source = "fn dsp() { 42 }";
        let tokens = tokenize(source);
        
        assert_eq!(tokens[0].kind, TokenKind::Function);
        assert_eq!(tokens[0].text(source), "fn");
        
        assert_eq!(tokens[1].kind, TokenKind::Whitespace);
        
        assert_eq!(tokens[2].kind, TokenKind::Ident);
        assert_eq!(tokens[2].text(source), "dsp");
    }
    
    #[test]
    fn test_tokenize_numbers() {
        let source = "42 3.14";
        let tokens = tokenize(source);
        
        assert_eq!(tokens[0].kind, TokenKind::Int);
        assert_eq!(tokens[0].text(source), "42");
        
        assert_eq!(tokens[2].kind, TokenKind::Float);
        assert_eq!(tokens[2].text(source), "3.14");
    }
    
    #[test]
    fn test_tokenize_string() {
        let source = r#""hello world""#;
        let tokens = tokenize(source);
        
        assert_eq!(tokens[0].kind, TokenKind::Str);
        assert_eq!(tokens[0].text(source), r#""hello world""#);
    }
    
    #[test]
    fn test_tokenize_comments() {
        let source = "// single line\n/* multi\nline */";
        let tokens = tokenize(source);
        
        assert_eq!(tokens[0].kind, TokenKind::SingleLineComment);
        assert_eq!(tokens[0].text(source), "// single line");
        
        assert_eq!(tokens[1].kind, TokenKind::LineBreak);
        
        assert_eq!(tokens[2].kind, TokenKind::MultiLineComment);
        assert_eq!(tokens[2].text(source), "/* multi\nline */");
    }
    
    #[test]
    fn test_tokenize_operators() {
        let source = "+ - * / == != < <= > >= && || |>";
        let tokens = tokenize(source);
        
        let op_kinds: Vec<_> = tokens.iter()
            .filter(|t| !matches!(t.kind, TokenKind::Whitespace | TokenKind::Eof))
            .map(|t| t.kind)
            .collect();
        
        assert_eq!(op_kinds, vec![
            TokenKind::OpSum,
            TokenKind::OpMinus,
            TokenKind::OpProduct,
            TokenKind::OpDivide,
            TokenKind::OpEqual,
            TokenKind::OpNotEqual,
            TokenKind::OpLessThan,
            TokenKind::OpLessEqual,
            TokenKind::OpGreaterThan,
            TokenKind::OpGreaterEqual,
            TokenKind::OpAnd,
            TokenKind::OpOr,
            TokenKind::OpPipe,
        ]);
    }
    
    #[test]
    fn test_trivia_detection() {
        let source = "fn // comment\n dsp";
        let tokens = tokenize(source);
        
        assert!(!tokens[0].is_trivia()); // fn
        assert!(tokens[1].is_trivia());  // whitespace
        assert!(tokens[2].is_trivia());  // comment
        assert!(tokens[3].is_trivia());  // linebreak
        assert!(tokens[4].is_trivia());  // whitespace
        assert!(!tokens[5].is_trivia()); // dsp
    }
}
