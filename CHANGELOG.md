# CHANGELOG

## v3.0.0

### Added feature

- Added Live State Updating feature, which enables live coding on mimium. When you save the mimium file that you're running on, the compiler automatically renew instances without audio interruption.(#159)
- Added a global stage decalaration syntax `#stage(main) / #stage(macro)`.(#154)
- Now a destructive assignment in stage 0 evaluation is allowed. With this feature, standard library function `unoise` is newly implemented with this manner.(#162)
- Added record update syntax `{rec_var <- field1 = 100, field2 = 200}`(#158)

### Breaking changes

- `gen_sampler_mono` was replaced with `Sampler_mono!("file_path")` macro.(#161)
- `make_probe` function was replaced with `Probe("name")` macro.(#156)

## v2.4.2

- Fixed language server crashes

## v2.4.1

- Added a hygienic macro feature with multi stage computation paradigm(experimental).(#136)
- Added record type and parameter-packing feature(experimental).(#128,#134,#130)
- Added basic language server support(#151)
- Added a simple slider ui(#147)

- (For developers, the directory structure is significantly changes to organize many crates)

## v2.3.1

(2.3.0 is skipped due to the release workflow)

### Added feature

- Basic implementation for Array has done(#124)
- `cargo-dist` is switched to the forked version by Astral(#125 by @yutannihilation)
- Now minimal rust edition is required to be **2024**(due to the limitation by `coreaudio-sys` crate).

### Others

- Improved CI caching (#120 by @yutannihilation)
- Experimenting development w/ LLM. Added `.clinerules` and `.cline/` for the basic bank for models. Added some documentation comment using this.(#119)

## v2.2.7

### Bugfix

- Added option to control the initial return value of the function which uses `self` and changed the default behavior.(#118)
- Fixed singe-line comment lexer consumes linebreak and causes parse error (#119)

### Other

- Added manually triggerable release workflow for Github Action CI

## v2.2.6

### Bugfix

- Fixed an incorrect lookup order of local variables(#116)
- Fixed type checker so that the block with curly brace creates local variable scope(#117)

### Other

- Now integration test is done in wasm build too, using node.js as filesystem host

## v2.2.5

### Bugfix

This release fixes several degradations introduces in the web platform.

- Fixed audio driver crash
- Fixed file loader is not working on native platform

## v2.2.4

### New Features

- Basic setting for WASM build is completed(#112). The actual entrypoint for webaudio is [mimium-webaudio](https://github.com/mimium-org/mimium-webaudio). The editor webcomponent is now working on [mimium-web-component](https://github.com/tomoyanonymous/mimium-web-component). Example of the mimium-webaudio usage is on there.

### Bug fix

- Fixed the type inference bug in the let declaration.(#114)

## v2.2.3

### Library update

- added `delay.mmm` which contains ping-pong delay
- added `reverb.mmm` which contains freeverb

### Bug fix

- Fixed incorrect format in parsing error.
- Fixed incorrect implementation in `core.mmm`


## v2.2.2

### Bug fix

- fixed incorrect type unifications in function type.
- fixed higher-order function type inference.
- fixed possible infinite loop `include` tries to expand itself.(Note that mutually recursive inclusion is still possible)
- Better error report for `include` 

## v2.2.1

updated examples

## v2.2.0

### Improvement

- `include("file.mmm")` directive can now find files in `${HOME}/.mimium/lib` (#109).
- added "reactive.mmm" example

### Bug fix

- Fixed incorrect inplementation of `log` function(#107)

## v2.1.1

### Bug fix

- Fixed another bug incomment tokenizer so that the line comment on the last line can be parsed correctly (#106)
- Fixed crash on WSL by trying to load midi plugin though it is not available on wsl(#105)

## v2.1.0

### Improvement

- Error reporting is improved so that the multiple errors can be reported(#84).
- The maximum size of PopState/ShiftState is now 24bit(#102)

### Bug fix

- Fixed bug in comment parser (#97)
- Other small refactors(#103)

## v2.0.0

TODO

## v0.4.0

### New Language Feature

Struct type and type alias are introduced(#56).

```rust
type stereo = (float,float)
type composite = {signal:stereo , id:float}
fn test2(){
    ogya = composite{(100,200), 100}
    ho,fu = ogya.signal
    he = ogya.id
    println(ho)
    println(fu)
    println(he)
}
test2()
```

Still struct type has several limitations.

- Embedding function type variable onto struct (and tuple) cannot be used for closure.
- destructive assignment for struct variable by dot operator is not implemented yet(only a parser is implemented).

### Bugfixes

- Fixed a behaviour of CLI when it could not find an input file path(#62,by @t-sin).

### Refactoring

- Some internal helper functions such as recursive variants are refined.

## v0.3.1(2021-02-25)

### Bugfixes

- fixed a bug in code generator when if statement has void block(#51)
- added missing `==` and `!=` infix operators (#53)

### Refactoring

- `ExecutionEngine` class has been made by splitting from `Runtime` class toward implementing interpreter backend and environment variables feature.

### New Function

Though this release is a patch release, a subtle new feature is added.

- `mem`, a 1-sample delay function has been added. A simple example of biquad filter using this function is added to `mimium-core/filter.mmm`(#44).

### Other updates

A list of all contributers is added to readme by using [all-contributors](https://allcontributors.org/).

## v0.3.0 (2021-02-03)

### New language features

#### Tuple and Array type

New aggregate type, tuple and array have been added.

Tuple type can be constructed with parenthesis and comma.
Currently, the only way to get value of tuple type is like C++'s structural binding.

```rust
# Tuple Construction
triples = (100,200,300)//type signature is (float,float,float)

one,two,three = triples
```

In the future, dot access operator like `triples.0` will be added.

Array type can be constructed with angle brackets and comma.
Currently All the array is mutable , fixed sized and declared as a private global variable in llvm module.
As a unique feature, an interpolation with floating pointer index is supported as same as reading audio files.

```rust
# Array Construction
myarr = [100,200,300]
internalv = myarr[0]//zero-based index
myarr[2] = 400//the array is now [100,200,400]
interp = myarr[0.5]//the result will be 150(linear interpolation)
```

#### Multichannel support for dsp function

Now mimium supports more than mono output, stereo and more many channels.

Numbers of inputs and outputs are determined by a type of `dsp function`.
For example, stereo panning can be written like below.

```rust
fn panner(input:float,pan:float) -> (float,float){
    return (input*pan,input*(1-pan))
}

fn dsp(input:(float,float))->(float,float){
    src = random()*0.2
    res = panner(src,sin(5*now/48000)+1*0.5)
    return res
}
```

This is the breaking change because, 1. input parameter for `dsp` was `time` before v0.2 (this was a temporary solution before `now` was implemented), 2. output type for `dsp` was `float` but now the type for `dsp` should be `(tuple of float)->(tuple of float)`, a function that takes 1 variable with tuple of floats and returns tuple of floats.
Even if you want to process mono, the output type should be a tuple of 1 float.

Some examples were rewritten to match with this language spec.

### Using mimium as C++ library

Dependencies of library headers and frontend(application instance and CLI) were tidied up.
We can now use mimium as C++ Library.
You can import mimium easily by using CMake.
A minimal example is on https://github.com/mimium-org/mimium-libimport-example.

### other changes

- Supported version of LLVM is now 11.
- Code_Of_Conduct.md was added.

## v0.2.0 (2020-12-24)

### Improvements

- Windows build(on MSYS2) is ready. Check GitHub Actions workflow if you want to build.
- `delay(input,delaytime)` primitive function has been added.
  - maximum delay time is currently fixed to 44100 samples.
- (experimental) a simple macro preprocessor has been added.
  - you can now include other source files with `include "otherfile.mmm"` put on a global context.
- Refactored Mid-Level internal representation.

## v0.1.5 (2020-09-28)

### Fixed bugs

- fixed a compile error when if statement contains only a single fcall inside braces(Thanks @t-sin).

### Improvements

- Automatically copies changelog to release body in GitHub actions

### Other

Now new features such as delay primitive function which will be included in v0.2.0 are under development.

## v0.1.4 (2020-09-11)

### Fixed bugs

- fixed compile error with a combination of clang & libstdc++.
- fixed crashes on returning if as expression in function.

## Improvement

- introduced address sanitizer and simple fuzzing test in develop environments.
- memory allocations on user-code are freed on a destructor call of runtime.
  - heap allocations by user-code was not freed in previous versions but it's not a serious problem because a program itself are exited when a destructor of runtime is called. This improvement makes sense mostly for a fuzzing test that iterates compilations many times in same process.

## v0.1.3 (2020-09-09)

### fixed bugs

- fixed unusual crash when if-statement used on linux
- fixed operator precedence of if statement when used as expression

```rust
// mathmatical operators have higher precedence than if or else!
myvar = if(cond) 100+20 else 200*50
```

## v0.1.2 (2020-09-07)

### fixed bugs

- Previous release did not contain llvm generation for if statement. Now if statement/expression works correctly.
- fixed a bug that comment-out which ends with a line break could not be parsed correctly.

### Refactoring

MIR(Mid-Level Representation)-related classes' implementation has been made more simplified.
As like ast classes refined in v0.1.0, data does not have member functions and is initiallized with aggregate initialization.

## v0.1.1 (2020-09-05)

### Fixed Bug

This release mainly fixed a parser.

Now, a specification of **block** , statements inside curly-brackets is similar to Rust.

The block can be treated as just an **expression**, like function call or infix operations, when you put a single expression on last line of statements in block, or put `return` statement. That value becomes the block's value itself.

Thus, all examples below is valid.

```rust
fn test(x,y){
    localvar = x*y+100
    localvar //last line is expression.
}
fn test(x,y){
    localvar = x*y+100
    return localvar //last line is return statement.
}

testvar = { velylocal = 100
            verylocal*2004} * 300 // this is also valid...
```

By this change, `if` statement can be both of an expression and a statement. When the contents of then/else statement do not have return value, it is just treated as like function of `void` type.

## v0.1.0 (2020-08-21)

This release is mostly refactoring of internal processings, especialy AST(Abstract Syntax Tree)-related classes which was written in very early stage of development and is completely different style of implementation compared to newly introduced class such as MIR and Type related classes. A dynamic polymorphism in this compiler development is unified to the way using `std::variant`, not an class inheritance.

The release does neither contains new features nor breaking changes but fixed many instabilities, particularly incorrectness of type inference.

Also, build on Ubuntu has prepared. You can check dependencies from [github action workflow page](https://github.com/mimium-org/mimium/actions/runs/217956579/workflow). Seeking people who can test on windows!

## v0.0.0 (2020-02-15)

first release! no arrays, no structs, no include/modules. This is a "proof of concept" version of mimium.

### known bugs

- Calling same functions that uses `self` more than twice, instantiation of self will not be performed properly. (can avoid this behaviour by defining alias of function)
- in `dsp` function, at least 1 global variable should be used.

### limitations

- `if` can have only a expression, not multiple statements(it's like conditional operator(`(cond)? then_expr : else_expr`) in other languages.)
- array access `[]` can be used only for audio files loaded by using `loadwav(path)`. No boundary checks for array access. You can get a size of the array by `loadwavsize(path)`.
