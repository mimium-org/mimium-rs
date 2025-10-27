# mimium 

main: [![Test(main)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml/badge.svg?branch=main)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml) dev: [![Test(dev)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml/badge.svg?branch=dev)](https://github.com/tomoyanonymous/mimium-rs/actions/workflows/ci.yaml)

A programming language as an infrastructure for sound and music.

<p align="center" style="display:flex; justify-content:center;">
<img src = "mimium_logo_slant.svg" width="300" alt="An icon of the mimium. The word “mimium” is written in small caps, white letters at an angle on a gray diamond-shaped background with a gradient. The vertical bars of the letters are evenly spaced, making it look like a pedestrian crossing." />
</p>

https://mimium.org

---

mimium(*MInimal-Musical-medIUM*) is a programming language for sound and music.

mimium is made to be an infrastructure for distributing music in a form of a source code, not only a tool for musicians and programmers.

Its semantics are inspired from several modern programming languages for sound such as *[Faust](https://faust.grame.fr)*, *[Kronos](https://kronoslang.io/)* and *[Extempore](https://extemporelang.github.io/)*.

A minimal example below generates a sinewave of 440Hz.

```rust
// minimal.mmm
let twopi = 3.141595*2.0
fn dsp(){
  sin(now * 440.0 * twopi / samplerate)
}
```

## Unique Features

### Functional Approach

A special keyword `self` can be used in function, which is a last return value of the function.
This enables an easy and clean expression of feedback connection of signal chain as a function pipe.

```rust
fn lpf(input,fb){    
     (1.0-fb)*input + fb*self
}
```

Also, the language design is based on the call by value lambda calculus with multi-stage computation and the higher-order functions are supported to express generative signal graph like replicatiing multiple oscillators.

```rust
//10 Sequentially connected oscillators
#stage(macro)
fn cascade(n,gen){
    if (n>0.0){
        let multiplier = 1.0-(1.0/(n*3)) |> lift_f
        `|rate| rate + ($gen)(rate/3)* 0.5 * rate* $multiplier  
                |> cascade!(n - 1.0 ,gen) 
    }else{
        `|rate| ($gen)(rate)
    }
}
#stage(main)
let osc = ...
fn dsp(){
    let out =  50 |> cascade!(10,`osc) 
    (out,out)
}
```

mimium is a statically typed language but the most of type annotations can be omitted by the type inference system. If you are interested in the theoritical background of mimium, see [the paper about mimium](https://matsuuratomoya.com/en/research/lambdammm-ifc-2024/).

### Live Coding

mimium can describe digital signal processing algorithm from very low-level, like Faust. Moreover, mimium can update source code without audio interruption, which enables **full-scratch dsp livecoding** performance.

### Extensibility

mimium's VM design is inspired by Lua, that can be easily embedded on Rust application through the plugin system. External functions(closures) defined in Rust can be easily called from mimium.

## Installation

The easiest way to start mimium is using [Visual Studio Code Extension](https://github.com/mimium-org/mimium-language). The extention will automatically download CLI tool `mimium-cli` and language server (support for syntactic highlight and error reporting) when installed.

You can run `.mmm` file from the command palette.

Also you can download the latest CLI tool [mimium-cli](https://github.com/tomoyanonymous/mimium-rs/releases) from GitHub Release through shell script.

## Development

See [Development](./Development) section.

## Contributing

There's no concrete way for contributing to the mimium project for now but any type of contribution (bugfix, code refactoring, documentation, showing the usecases, etc).

(However, because the mimium is still early stage of the development and there's much things to do, the proposal or request for new feature without Pull Request may not be accepted.)

Take a look at [Code of Conduct](./CODE_OF_CONDUCT) before you make contribution.

## [License](LICENSE)

©️ the mimium development community.

The source code is licensed under the [Mozilla Puclic License 2.0 (MPL2.0)](LICENSE).

## Original Author

Tomoya Matsuura/松浦知也 <https://matsuuratomoya.com/en>

## Acknowledgements

This project is supported grants and scholarships as follows.

- 2019 Exploratory IT Human Resources Project ([The MITOU Program](https://www.ipa.go.jp/jinzai/mitou/portal_index.html)) by IPA: INFORMATION-TECHNOLOGY PROMOTION AGENCY, Japan.
- Kakehashi Foundation (2022)
- JSPS Kakenhi 23K12059 "Civil Engineering of Music, as a Practice and Critics between Music and Engineering"(2023-2025)

### Contributers

This list contains the contributers from v1 development, documentation and financial sponsors(via github sponsor).

#### Source Code Contributions

- [Hiroaki Yutani](https://github.com/yutannihilation)
- [karnpapon](https://github.com/karnpapon)
- [Shinichi Tanaka(t-sin)](https://github.com/t-sin)
- [kyo](https://github.com/syougikakugenn)
- [Inqb8tr-jp](https://github.com/Inqb8tr-jp)
- [zakuro9715](https://github.com/zakuro9715)

#### Other forms of Contributions

- [Baku Hashimoto](https://baku89.com)
- [Yuichi Yogo](https://github.com/yuichkun)
- [Ayumu Nagamatsu](http://ayumu-nagamatsu.com/)
- [zigen](https://horol.org/)


## Known Bugs

See [GitHub Issues with "bug" tag](https://github.com/tomoyanonymous/mimium-rs/issues?q=is%3Aissue+is%3Aopen+label%3Abug).

## [Roadmap](./Roadmap.md)