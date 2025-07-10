# Active Context: mimium-rs

## Current Work Focus

The current focus is on implementing Parameter Pack for the mimium programming language, which will allow functions to accept tuples or records as arguments when the types match. This feature will enhance the language's expressiveness and provide more flexible function calling patterns, especially when combined with pipe operators.

Previous work focused on implementing Record type for the mimium programming language based on GitHub issue #99, providing a way to define structured data with named fields, enhancing the language's expressiveness and data modeling capabilities.

## Implementation Strategy for Record Type

The implementation of Record type will be divided into four main phases:

1. **Parser Implementation**: Extend the parser to recognize record syntax
2. **Type System Integration**: Enhance the type system to support record types
3. **MIR Generation**: Update the MIR generator to handle records
4. **VM Implementation**: Extend the VM to execute operations on records

### Phase 1: Parser Implementation

1. **Record Type & Constructor Syntax**: Add syntax for defining record types with named fields: `let person: {name: string, age: int} = {name: "John", age: 30}`
2. **Field Access Syntax**: Add dot notation for accessing record fields: `person.name` or `person.age`
3. **Field Update Syntax**: Add syntax for updating record fields: `person.age = 31`

### Phase 2: Type System Integration

1. **Record Type Representation**: Extend the `Type` enum to include records with named fields
2. **Type Checking**: Implement type checking for record creation and field access
3. **Type Inference**: Update type inference to handle record types
4. **Subtyping Rules**: Mimium uses structural subtyping. That means if the function requires record type `{name: string, age:int}` for its argument, the actual argument can be any record types which have `name` and `age` field at least.

### Phase 3: MIR Generation

1. **Record Operations**: Generate MIR for record creation, field access, and field update. This operation must be the same instruction for the tuple type at the MIR level.
2. **Memory Layout**: Define memory layout for records
3. **Field Lookup**: Implement efficient field lookup mechanisms
4. **Optimization**: Apply optimizations specific to records

### Phase 4: VM Implementation

1. **Runtime Representation**: Define runtime representation for records. This is almost same as the tuple types and maybe no modification is required.

## Recent Changes

1. **LSP Implementation**: Completed the initial implementation of the Language Server Protocol server
2. **Roadmap Update**: Updated the roadmap to include Record type implementation
3. **Issue Analysis**: Analyzed requirements and constraints for Record type implementation

## Next Steps

1. **Parser Implementation**:
   - Define grammar rules for record type declarations
   - Add parsing for record construction expressions
   - Implement field access and field update parsing
   - Add test cases for parser validation

2. **Type System Integration**:
   - Extend the `Type` enum with `RecordField` and update `Struct` type
   - Implement type checking for record operations
   - Update type inference to handle record types
   - Add test cases for type system validation

3. **MIR Generation**:
   - Modify MIR instructions for record operations, based on the functionality for tuple types.
   - Define memory layout for records
   - Implement field lookup and access
   - Add test cases for MIR validation

4. **VM Implementation**:
   - Update runtime representation
   - Add benchmarks for performance evaluation

5. **Documentation and Examples**:
   - Document the record type feature
   - Create example programs using records
   - Update language reference

## Active Decisions and Considerations

1. **Record Type Design**:
   - **Structural vs. Nominal Typing**: Using structural typing for records where two record types with identical fields are considered compatible, rather than nominal typing which would distinguish types by name.
   - **Immutable vs. Mutable**: Records will be mutable, allowing field updates after creation, consistent with existing mimium design.
   - **Memory Layout**: Using a flat memory layout for records to optimize performance for audio processing.
   - **Field Lookup**: Using string interning for field names to optimize field lookup.

2. **Language Integration**:
   - **Syntax**: Keeping syntax similar to Rust and TypeScript for familiarity.
   - **Type Aliases**: Supporting type aliases for records to enable clearer code.
   - **Relationship with Tuples**: Records will be distinct from but complementary to tuples, with records using named fields and tuples using positional fields.

3. **Performance Considerations**:
   - **Runtime Efficiency**: Ensuring record operations are efficient enough for real-time audio processing.
   - **Memory Usage**: Minimizing memory overhead for record storage.
   - **Field Access Optimization**: Optimizing field access to be as fast as constant-time lookups where possible.

4. **Error Handling**:
   - **Detailed Errors**: Providing clear error messages for missing fields or type mismatches.
   - **Runtime Checks**: Implementing runtime checks for field access to prevent invalid operations.

## Important Patterns and Preferences

1. **Code Organization**:
   - Following the existing compiler pipeline architecture
   - Keeping changes modular and focused on specific components
   - Using extensive unit and integration tests to validate changes

2. **Naming Conventions**:
   - Following Rust naming conventions (snake_case for functions, CamelCase for types)
   - Using consistent naming for record-related components

3. **Error Handling**:
   - Using the existing `ReportableError` trait for error reporting
   - Providing clear error messages with contextual information
   - Implementing graceful error recovery where possible

4. **Documentation**:
   - Adding comprehensive documentation for record types
   - Including examples in documentation
   - Documenting design decisions and trade-offs

5. **Testing**:
   - Creating extensive unit tests for each component
   - Adding integration tests for end-to-end validation
   - Testing edge cases and error conditions

## Learnings and Project Insights

1. **Record Type Usage Patterns**: 
   - Records are useful for organizing related data together
   - They enable clearer code by naming fields explicitly
   - They can represent complex data structures like audio effects parameters, instrument settings, or MIDI data

2. **Relationship to Other Language Features**:
   - Records complement tuples by adding named fields
   - They build on the existing type system infrastructure
   - They serve as a foundation for future features like modules and objects

3. **Implementation Challenges**:
   - Field lookup needs to be optimized for performance
   - Type checking and inference for records can be complex
   - Memory layout affects runtime performance

4. **Example Use Cases**:

   ```mimium
   // Define a synth parameters record type
   type alias SynthParams = {
     freq: float,
     amp: float,
     attack: float,
     release: float
   }
   
   // Create an instance
   fn create_synth() {
     let params = {freq: 440.0, amp: 0.8, attack: 0.01, release: 0.5}
     oscillator(params.freq) * envelope(params.attack, params.release) * params.amp
   }
   ```

5. **Future Extensions**:

- Functionalities for defining default value of record types.
- Automatic parameter packing for function using record, when the parameter names matches to the field names of the single record type.
- Based on these 2 functionalities above, default values for function parameters.

## Implementation Strategy for Parameter Pack

The implementation of Parameter Pack will extend the existing type system and MIR generation to support automatic unpacking of tuples and records when used as function arguments. This feature will be implemented in the following phases:

1. **Type System Updates**: Extend the function type to optionally include parameter names
2. **Type Checking Extensions**: Update type checking for function applications
3. **MIR Generation**: Modify MIR generation for function applications to handle parameter packs
4. **Testing and Documentation**: Add test cases and documentation for the feature

### Phase 1: Type System Updates

1. **Function Type Extension**: Extend the function type representation to optionally include parameter names
   ```rust
   // Conceptual representation
   struct FunctionType {
       parameter_types: Vec<(Option<Symbol>,TypeNodeId)>,
       return_type: TypeNodeId,
   }
   ```

2. **Type Unification Rules**: Modify the type unification algorithm to handle named parameters:
   - When unifying a function type with named parameters and one without, preserve the names
   - When unifying two function types with different parameter names, choose one set of names (preferably from the context that's more specific)
   - When a function is passed as an argument to another function, parameter names might be dropped as they're not relevant in that context

### Phase 2: Type Checking Extensions

1. **Function Application Rules**:
   - Extend function application type checking to allow a tuple or record as a single argument to a multi-parameter function
   - For tuples, check that the number of elements and their types match the function parameters
   - For records, check that field names match parameter names and their types are compatible
   - Implement structural subtyping for records to allow extra fields

2. **Error Reporting**:
   - Provide clear error messages for mismatched parameter names or types
   - Suggest corrections when there's a close match (e.g., parameter name typos)

### Phase 3: MIR Generation

1. **App Node Processing**:
   - Modify the MIR generation for `App` nodes to detect parameter pack cases
   - When a tuple is passed to a multi-parameter function:
     ```
     // From: add((100, 200))
     // To: add(get_element(tuple, 0), get_element(tuple, 1))
     ```
   
   - When a record is passed to a multi-parameter function:
     ```
     // From: add({a: 100, b: 200})
     // To: add(get_field(record, "a"), get_field(record, "b"))
     ```

2. **Optimization**:
   - If the tuple or record is a literal constructed at the call site, potentially optimize to avoid creating the intermediate structure
   - When parameter packs are used with the pipe operator, ensure efficient code generation

### Phase 4: Testing and Documentation

1. **Test Cases**:
   - Basic function calls with tuple and record parameter packs
   - Parameter packs with pipe operators
   - Higher-order functions with parameter packs
   - Edge cases and error conditions

2. **Documentation**:
   - Add parameter pack feature documentation
   - Provide examples showing the different ways to call functions
   - Document type system changes and any performance implications

## Active Decisions and Considerations for Parameter Pack

1. **Design Decisions**:
   - **Implicit vs. Explicit**: Parameter packing will be implicit, meaning the compiler automatically detects and unpacks tuples and records
   - **Type Checking Strategy**: Using structural compatibility to determine if a tuple or record can be unpacked as function arguments
   - **Relationship with Pipe Operators**: Optimizing for the common case of using parameter packs with pipe operators

2. **Performance Considerations**:
   - **Runtime Efficiency**: Ensuring parameter unpacking doesn't introduce significant overhead
   - **Compile-time Impact**: Minimizing the impact on type checking and compilation times
   - **MIR Optimization**: Implementing optimizations to eliminate unnecessary intermediate structures

3. **Usability Considerations**:
   - **Error Messages**: Providing clear and helpful error messages for parameter pack mismatches
   - **IDE Support**: Ensuring LSP can provide appropriate hints and completions for parameter packs
   - **Documentation**: Clearly documenting the feature to prevent confusion

## Example Use Cases for Parameter Pack

```mimium
// Define a function with multiple parameters
fn mix(input: float, dry: float, wet: float) -> float {
  input * dry + input * wet
}

// Traditional call
let result1 = mix(signal, 0.7, 0.3)

// Using tuple parameter pack
let params = (signal, 0.7, 0.3)
let result2 = mix(params) // Equivalent to mix(signal, 0.7, 0.3)

// Using record parameter pack
let settings = {input: signal, dry: 0.7, wet: 0.3}
let result3 = mix(settings) // Equivalent to mix(signal, 0.7, 0.3)

// Using with pipe operator for enhanced readability
let result4 = (signal, 0.7, 0.3) |> mix
let result5 = {input: signal, dry: 0.7, wet: 0.3} |> mix

// Using with function that takes a function as parameter
fn process_audio(audio_fn: (float, float, float) -> float, signal: float) -> float {
  audio_fn(signal, 0.5, 0.5)
}
```

## Integration Testing for Parameter Pack

To ensure the Parameter Pack feature works correctly across the entire compiler pipeline, we need comprehensive integration tests. These tests should cover different usage scenarios and verify that parameter packs work correctly with existing language features.

### Test Case Categories

1. **Basic Function Calls with Parameter Pack**:
   - Test calling multi-parameter functions with tuple parameter packs
   - Test calling multi-parameter functions with record parameter packs
   - Test error handling for mismatched parameter types or names

2. **Parameter Pack with Pipe Operators**:
   - Test using tuples with pipe operators: `(a, b) |> function`
   - Test using records with pipe operators: `{x: a, y: b} |> function`
   - Test chaining multiple pipe operations with parameter packs

3. **Higher-order Functions**:
   - Test passing functions that accept parameter packs to higher-order functions
   - Test returning functions that accept parameter packs from other functions

4. **Mixed Parameter Usage**:
   - Test functions that have a mix of regular parameters and parameter packs
   - Test nested parameter packs (parameter packs within parameter packs)

5. **Edge Cases**:
   - Test with empty tuples or records
   - Test with tuples/records containing complex types (e.g., functions, other tuples/records)
   - Test with large numbers of parameters

### Example Integration Test

```mimium
// Integration test for parameter pack
fn test_parameter_pack() -> bool {
  // Test function that takes multiple parameters
  fn add3(a: float, b: float, c: float) -> float {
    a + b + c
  }

  // Test basic tuple parameter pack
  let result1 = add3((1.0, 2.0, 3.0))
  
  // Test basic record parameter pack
  let result2 = add3({a: 1.0, b: 2.0, c: 3.0})
  
  // Test pipe operator with tuple
  let result3 = (1.0, 2.0, 3.0) |> add3
  
  // Test pipe operator with record
  let result4 = {a: 1.0, b: 2.0, c: 3.0} |> add3
  
  // All results should be 6.0
  let all_pass = result1 == 6.0 && result2 == 6.0 && result3 == 6.0 && result4 == 6.0
  
  // Test higher-order function
  fn apply_to_values(f: (float, float, float) -> float, values: (float, float, float)) -> float {
    f(values)  // Double parameter pack: f is called with unpacked values
  }
  
  let result5 = apply_to_values(add3, (1.0, 2.0, 3.0))
  
  return all_pass && result5 == 6.0
}

// Test error cases (these should generate compile-time errors)
fn test_parameter_pack_errors() {
  fn add2(a: float, b: float) -> float {
    a + b
  }
  
  // Error: wrong number of elements in tuple
  // let error1 = add2((1.0, 2.0, 3.0))
  
  // Error: missing parameter in record
  // let error2 = add2({a: 1.0})
  
  // Error: wrong parameter name in record
  // let error3 = add2({x: 1.0, y: 2.0})
}
```

### Test Implementation Plan

1. **Create a dedicated test file**:
   - Create a new test file at `/tests/parameter_pack.rs` for unit tests
   - Create a new test file at `/tests/integration/parameter_pack_test.mm` for integration tests

2. **Test Types**:
   - Unit tests for type system changes
   - Unit tests for MIR generation
   - Integration tests using complete mimium programs

3. **Test Environment**:
   - Use the existing test infrastructure to compile and run mimium programs
   - Add assertions to verify expected results
   - Test both compilation and runtime behavior

4. **Performance Testing**:
   - Benchmark parameter pack overhead compared to regular function calls
   - Test performance impact of complex parameter pack usage
   - Verify optimizations work correctly

The integration tests will be a crucial part of verifying that the parameter pack feature works correctly and meets the design requirements. They will help identify any issues with type inference, MIR generation, or runtime behavior before the feature is released.

## Next Steps for Parameter Pack Implementation

1. **Type System Updates**:
   - Update function type representation to include parameter names
   - Implement updated type unification rules
   - Add test cases for type system changes

2. **Type Checker Extensions**:
   - Implement parameter pack type checking for function applications
   - Update error reporting for parameter pack mismatches
   - Add test cases for type checking

3. **MIR Generation**:
   - Implement parameter unpacking in MIR generation
   - Add optimizations for parameter packs
   - Add test cases for MIR generation

4. **Integration Testing**:
   - Test parameter packs with existing language features
   - Benchmark performance impact
   - Test edge cases and error conditions

5. **Documentation**:
   - Update language reference with parameter pack documentation
   - Add examples demonstrating parameter pack usage
   - Document design decisions and implementation details
