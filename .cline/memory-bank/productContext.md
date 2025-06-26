# Product Context: mimium-rs

## Purpose and Vision

mimium (MInimal Musical MedIUM) is a programming language specialized for describing and generating music. It aims to provide a simple yet powerful environment for musicians, sound designers, and audio programmers to create and manipulate sound in a programmatic way.

The vision for mimium is to bridge the gap between traditional music composition and programming, offering a functional programming approach to audio synthesis and processing that is both expressive and efficient.

## Theoretical Foundation

mimium is based on a computation system called λ<sub>mmm</sub> (lambda-triple-m), which extends the Simply Typed Lambda Calculus (STLC). It uses a call-by-value evaluation strategy, the simplest evaluation strategy where all arguments are copied when a function is called, creating new values.

The language implements its own virtual machine (VM) and instruction set to execute λ<sub>mmm</sub> at practical speeds. While this approach may not be as performant as some other music programming environments like Faust, it covers most practical real-time use cases.

## Unique Features

### Signal Feedback with `self`

Inside a function, the special keyword `self` references the last value returned by the function. This enables creating stateful functions that maintain internal state between calls, which is essential for many audio processing algorithms.

Example:
```mimium
fn counter(increment){
    self+increment
}
```

### Scheduling with the `@` Operator

The `@` operator allows for temporal recursion patterns by delaying function execution. By appending `@` and a numeric value to a `void` function, you can schedule when it will be executed (in samples).

Example:
```mimium
fn updater(){
    freq = (freq + 1.0)*1000
    println(freq)
    updater@(now+1.0*samplerate)
}
updater@1.0
```

This pattern is known as Temporal Recursion and is used in languages like Extempore.

## Target Users

1. **Musicians and Composers**: Who want to explore algorithmic composition and sound synthesis
2. **Sound Designers**: Creating complex sound effects and audio processing chains
3. **Audio Programmers**: Building audio applications and plugins
4. **Researchers**: Exploring new approaches to computer music
5. **Educators**: Teaching audio programming concepts

## User Experience Goals

1. **Simplicity**: Provide a clean, intuitive syntax that is accessible to users with limited programming experience
2. **Expressiveness**: Enable complex audio manipulations with concise code
3. **Real-time Feedback**: Immediate auditory feedback during development
4. **Integration**: Seamless integration with existing audio workflows and tools
5. **IDE Support**: Modern development environment with code completion, error highlighting, and other productivity features

## Problem Space

mimium addresses several challenges in the audio programming space:

1. **Complexity**: Traditional audio programming often requires deep knowledge of DSP and low-level concepts
2. **Performance**: Audio code must be efficient and deterministic to avoid glitches
3. **Abstraction**: Balancing high-level expressiveness with low-level control
4. **Tooling**: Lack of specialized development tools for audio programming
5. **Learning Curve**: Steep learning curve for existing audio programming languages

## Key Features

1. **Functional Programming Model**: First-class functions, closures, and immutable data
2. **Real-time Audio Processing**: Low-latency audio output
3. **Stateful Functions**: Using the `self` keyword for maintaining state
4. **Temporal Recursion**: Using the `@` operator for scheduling
5. **Standard Library**: Including oscillators, filters, and other audio processing functions
6. **Language Server Protocol**: IDE integration for code completion, error reporting, etc.

## Competitive Landscape

mimium exists in an ecosystem with several other audio programming languages and environments:

1. **SuperCollider**: A platform for audio synthesis and algorithmic composition
2. **Pure Data**: A visual programming language for multimedia
3. **ChucK**: A strongly-timed audio programming language
4. **Faust**: A functional programming language for real-time signal processing
5. **Extempore**: A live programming language with temporal recursion patterns

mimium differentiates itself through its:
- Functional programming paradigm based on lambda calculus
- Unique features like the `self` keyword and `@` operator
- Rust-based implementation for safety and performance
- Modern tooling and IDE integration
- Balance between expressiveness and efficiency

## Success Metrics

The success of mimium can be measured by:

1. **User Adoption**: Number of active users and community growth
2. **Creative Output**: Music and sound design created with mimium
3. **Performance**: Audio processing efficiency and stability
4. **Extensibility**: Ecosystem of libraries and tools built around mimium
5. **Documentation**: Quality and comprehensiveness of learning resources
