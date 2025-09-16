module.exports = grammar({
  name: 'mimium',

  extras: $ => [
    /\s/,
    $.comment,
  ],

  rules: {
    // Program
    source_file: $ => repeat($.item),

    // Top-level items
    item: $ => choice(
      $.include_statement,
      $.function_definition,
      $.macro_definition,
      $.let_statement,
      $.expression,
    ),

    // Include statement
    include_statement: $ => seq(
      'include',
      '(',
      $.string_literal,
      ')',
    ),

    // Let statements
    let_statement: $ => seq(
      choice('let', 'letrec'),
      $.pattern,
      optional($.type_annotation),
      '=',
      $.expression,
    ),

    // Function definition
    function_definition: $ => seq(
      'fn',
      $.identifier,
      '(',
      optional($.parameter_list),
      ')',
      optional($.return_type),
      $.block_expression,
    ),

    // Macro definition
    macro_definition: $ => seq(
      'macro',
      $.identifier,
      '(',
      optional($.parameter_list),
      ')',
      optional($.return_type),
      $.block_expression,
    ),

    // Parameters
    parameter_list: $ => sep1($.parameter, ','),
    parameter: $ => seq(
      $.pattern,
      optional($.type_annotation),
    ),

    // Return type
    return_type: $ => seq('->', $.type),

    // Assignment
    assignment: $ => seq(
      $.assignee,
      '=',
      $.expression,
    ),

    assignee: $ => choice(
      $.identifier,
      $.field_access,
      $.array_access,
    ),

    // Expressions
    expression: $ => choice(
      $.assignment,
      $.binary_expression,
      $.unary_expression,
      $.primary_expression,
      $.application_expression,
      $.lambda_expression,
      $.if_expression,
      $.block_expression,
      $.pipe_expression,
    ),

    // Binary expressions
    binary_expression: $ => choice(
      prec.left(10, seq($.expression, choice('*', '/', '%'), $.expression)),
      prec.left(9, seq($.expression, choice('+', '-'), $.expression)),
      prec.left(8, seq($.expression, choice('<', '>', '<=', '>='), $.expression)),
      prec.left(7, seq($.expression, choice('==', '!='), $.expression)),
      prec.left(6, seq($.expression, '&&', $.expression)),
      prec.left(5, seq($.expression, '||', $.expression)),
    ),

    // Unary expressions
    unary_expression: $ => choice(
      prec(11, seq('-', $.expression)),
      prec(11, seq('!', $.expression)),
    ),

    // Pipe expression
    pipe_expression: $ => prec.left(4, seq($.expression, '|>', $.expression)),

    // Primary expressions
    primary_expression: $ => choice(
      $.identifier,
      $.numeric_literal,
      $.string_literal,
      $.self_literal,
      $.now_literal,
      $.samplerate_literal,
      $.array_literal,
      $.record_literal,
      $.tuple_expression,
      $.parenthesized_expression,
      $.field_access,
      $.array_access,
      $.macro_call,
    ),

    // Application (function call)
    application_expression: $ => prec.left(12, seq(
      $.expression,
      '(',
      optional($.argument_list),
      ')',
    )),

    argument_list: $ => sep1($.expression, ','),

    // Lambda expression
    lambda_expression: $ => seq(
      '|',
      optional($.parameter_list),
      '|',
      $.expression,
    ),

    // If expression
    if_expression: $ => seq(
      'if',
      $.expression,
      $.block_expression,
      optional(seq('else', $.block_expression)),
    ),

    // Block expression
    block_expression: $ => prec(1, seq(
      '{',
      repeat($.item),
      '}',
    )),

    // Field access
    field_access: $ => prec.left(13, seq($.expression, '.', $.identifier)),

    // Array access
    array_access: $ => prec.left(13, seq($.expression, '[', $.expression, ']')),

    // Literals
    numeric_literal: $ => choice(
      $.integer_literal,
      $.float_literal,
    ),

    integer_literal: $ => /\d+/,
    float_literal: $ => /\d+\.\d*/,
    
    string_literal: $ => seq(
      '"',
      repeat(choice(
        /[^"\\]/,
        /\\./,
      )),
      '"',
    ),

    self_literal: $ => 'self',
    now_literal: $ => 'now',
    samplerate_literal: $ => 'samplerate',

    // Array literal
    array_literal: $ => seq(
      '[',
      optional(sep1($.expression, ',')),
      ']',
    ),

    // Record literal
    record_literal: $ => prec(2, seq(
      '{',
      optional(sep1($.record_field, ',')),
      '}',
    )),

    record_field: $ => choice(
      seq($.identifier, ':', $.expression),
      seq('..', $.expression), // Default spread
      prec(1, $.identifier), // Shorthand - lower precedence
    ),

    // Tuple expression
    tuple_expression: $ => seq(
      '(',
      $.expression,
      ',',
      sep1($.expression, ','),
      ')',
    ),

    // Parenthesized expression
    parenthesized_expression: $ => seq(
      '(',
      $.expression,
      ')',
    ),

    // Macro call
    macro_call: $ => prec(15, seq(
      $.identifier,
      '!',
      '(',
      optional($.argument_list),
      ')',
    )),

    // Patterns
    pattern: $ => choice(
      $.identifier_pattern,
      $.tuple_pattern,
      $.array_pattern,
      $.record_pattern,
      $.wildcard_pattern,
    ),

    identifier_pattern: $ => $.identifier,
    wildcard_pattern: $ => '_',

    tuple_pattern: $ => seq(
      '(',
      sep1($.pattern, ','),
      ')',
    ),

    array_pattern: $ => seq(
      '[',
      optional(sep1($.pattern, ',')),
      ']',
    ),

    record_pattern: $ => seq(
      '{',
      optional(sep1($.record_pattern_field, ',')),
      '}',
    ),

    record_pattern_field: $ => seq(
      $.identifier,
      optional(seq(':', $.pattern)),
    ),

    // Types
    type: $ => choice(
      $.primitive_type,
      $.function_type,
      $.tuple_type,
      $.array_type,
      $.record_type,
      $.type_identifier,
    ),

    primitive_type: $ => choice(
      'float',
      'int',
      'string',
      'struct',
    ),

    function_type: $ => seq(
      '(',
      optional(sep1($.type, ',')),
      ')',
      '->',
      $.type,
    ),

    tuple_type: $ => seq(
      '(',
      sep1($.type, ','),
      ')',
    ),

    array_type: $ => seq(
      '[',
      $.type,
      ']',
    ),

    record_type: $ => seq(
      '{',
      optional(sep1($.record_type_field, ',')),
      '}',
    ),

    record_type_field: $ => seq(
      $.identifier,
      ':',
      $.type,
    ),

    type_identifier: $ => $.identifier,

    type_annotation: $ => seq(':', $.type),

    // Identifiers
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // Comments
    comment: $ => choice(
      seq('//', /[^\r\n]*/),
      seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'),
    ),
  },
});

// Helper function for comma-separated lists
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}