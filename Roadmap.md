
# Roadmap

## For version 3

- [x] Array Type
- [ ] Record Type(https://github.com/mimium-org/mimium-rs/issues/99)
- [ ] More stable support for web platform
- [ ] Multi-stage computation (Hygienic Macro)
- [ ] Generics
- [ ] Module System
- [ ] Package Manager

## Further & Other Plans

- [ ] Native & WASM backend with Cranelift
- [ ] Transpilation to C++ using [Faust's Signal API](https://faustdoc.grame.fr/tutorials/signal-api/)
- [ ] Dynamic loading of plugins
- [ ] effect system for managing stateful function and IO

---

## Finished Topics

### For version 2 (Rust re-implementation)

- [x] Basic Data Types
  - [x] AST
  - [x] MIR
  - [x] VM Instructions
- [x] Aggregate Types
  - [x] Tuple (Vector) types
- [x] Compilers
  - [x] Stateful Functions
    - [x] Feedback with `self`
    - [x] Delay and mem
  - [x] Parser
  - [x] MIR Generation
    - [x] Type Inference
    - [x] Code Generation
  - [x] VM Code Generation 
- [x] Runtime
  - [x] Audio Driver Backend
    - [x] CPAL implmentation
  - [x] Logical Scheduler
    - [x] auto de-allocation of unused closure
    - [x] destructive assignment of closure upvalues
    - [x] schedule (`@`) operator
  - [x] Runtime value
    - [x] `now`
    - [x] `samplerate`
  - [x] VM
    - [x] Closure upvalue implementation
    - [x] StateStorage implementation
  - [x] simple file include
  - [x] simple audio file reader function
    - [ ] ~~array(slice) type & automatic interporation~~