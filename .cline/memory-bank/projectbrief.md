# Project Brief: mimium-rs

## Overview

mimium-rs is a Rust implementation of the mimium (MInimal Musical MedIUM) programming language, a domain-specific language designed for sound and music. The language focuses on providing a functional programming approach to audio processing and synthesis.

## Core Components

The project is organized as a Rust workspace with multiple crates:

- **mimium-lang**: Core language implementation (parser, compiler, runtime)
- **mimium-audiodriver**: Audio output implementation
- **mimium-cli**: Command-line interface
- **mimium-midi**: MIDI support
- **mimium-symphonia**: Audio file handling
- **mimium-scheduler**: Scheduler for audio processing
- **mimium-guitools**: GUI tools for visualization
- **mimium-web**: Web support
- **mimium-test**: Testing utilities
- **mimium-lsp**: Language Server Protocol implementation (newly added)

## Current Focus

The current focus is on implementing a Language Server Protocol (LSP) server for the mimium language to provide IDE features such as:

- Syntax error reporting
- Code completion
- Hover information
- (Future) Go to definition, find references, etc.

## Goals

1. Provide a robust and efficient implementation of the mimium language
2. Enable seamless integration with modern development environments
3. Support real-time audio processing and synthesis
4. Create a user-friendly experience for musicians and sound designers

## Technical Requirements

- Rust-based implementation
- Low-latency audio processing
- Cross-platform support
- Integration with standard audio APIs
