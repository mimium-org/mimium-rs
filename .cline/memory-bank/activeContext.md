# Active Context: mimium-rs

## Current Work Focus

The current focus is on implementing Record type for the mimium programming language based on GitHub issue #99. This will provide a way to define structured data with named fields, enhancing the language's expressiveness and data modeling capabilities.

Previous work focused on implementing a Language Server Protocol (LSP) server for the mimium programming language to provide IDE features such as syntax error reporting, code completion, and hover information to improve the developer experience.

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
