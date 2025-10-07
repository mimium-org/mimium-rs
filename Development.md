# Document for Developers

mimium compiler & runtimes are developed with Rust language.

## Prepare Development Environment

On windows, you need to install git and Visual Studio.

On macOS, you need to install XCode from AppStore and execute `xcode-select --install` on the terminal.

### Install Rust with rustup

To install rust language toolchains, follow the instruction of this page.

https://www.rust-lang.org/tools/install

#### Install additional cargo tools.

We uses several additional tools to improve developing experience and automate a deployment process.

You can install them with the command below.

```sh
cargo install clippy cargo-dist cargo-release
```

### Install IDE

Install Visual Studio Code.

https://code.visualstudio.com/

### Clone Repository

```sh
git clone https://github.com/tomoyanonymous/mimium-rs.git
```

Open `mimium-rs.code-workspace` with VSCode. The workspace contains recommended extensions, notification to install them will come up when you open the workspace first time.

- mimium-language(Syntax highlight for mimium language)
- rust-analyzer (Rust Language Server)
- CodeLLDB (Better debugger)
- Even better TOML(Better language support for Cargo.toml)

## Repository structure

- `crates/lib/mimium-lang` the main repository of the development.
- `crates/lib/mimium-test` repository for integration tests. 
- `crates/lib/mimium-web` Wrapper API for webassembly.
- `crates/lib/plugins` Builtin-plugins for mimium
  - `mimium-scheduler` core scheduling functionalities for `@` operator.
  - `mimium-midi` simple rmidi binding
  - `mimium-symphonia` Sampler implememntation using symphonia decoder
  - `mimium-guitools` GUI application tools for `Slider` and `Probe` macro
- `crates/bin/mimium-bintools` Wrapping 3 binary crates to distribute in one folder
 - `crates/bin/mimium-cli` A main frontend of mimium compiler.
 - `crates/bin/mimium-fmt` A code formatter(experimental, not compatible with handling code trivia such as comments)
 - `crates/bin/mimium-language-server` A language server for IDE, compatible with semantic highlighting and error reporting.
- `examples` basic examples of mimium codes.
- `lib` core mimium libraries that will be bundled with main binaries.


## How to Run & Debug

You can run built `mimium-cli` by running `cargo run -- targetfile.mmm`. Note that you need to be in `mimium-cli` directory.

Note that the binary with debug configuration is slow, you may hear the glitch noise (because of audio driver underrun). You should try with release build when you are trying to check audio with `--release` option.

You can also debug the binary with LLDB from the debug menu on the left sidebar of VSCode. It has several config options but mostly you use "Debug executable 'mimium-CLI'". You can change target `.mmm` file and optional arguments by modifying the line of `"args": ["mimium-cli/examples/sinewave.mmm"].`.

(Make sure not committing this change.)

You can emit abstract syntax tree (lisp style) with `--emit-ast` option, Mid-level intermediate representation with `--emit-mir`, Bytecodes for VM with `--emit-bytecode`. These are also useful for debugging.


## How to bump release (for the core maintainer)

Merge `dev` branch into `main` on your local repository, and write changelog.

You should write the version at h2 level. It should be reflected on release note on succeeding action.

You can trigger Github Actions Workflow to trigger release manually.

It internally execute `cargo-release`. You have to specify newer version number like `2.0.0-alpha2` as a workflow input.

```sh
cargo release 2.0.0-alpha2 --execute
```

The version should follow SemVer rule and do not require `v` prefix.

Note that this command will modify the version in the root `Cargo.toml` and make a commit and tag for them, and pushes it onto the remote.

Also it internally executes `cargo publish` to upload crates into crate.io, so make sure you have a write permission token to publish. (Set `CRATEIO_TOKEN` for repository secrets.)

For the wasm build, it executes `wasm-pack publish --target web`. It also requires npm publish token. (Set `NPM_PUBLISH_TOKEN` secret.)

If tagged commit is pushed to github, the another workflow is triggered.

The workflow uses `cargo-dist` to publish binary on a github release.

Do not forget re-merge commits on `main` into `dev` branch after main release is done.

## Versioning Strategy

We generally follow the rules of [Semantic Versioning](https://semver.org/), but for software such as compilers, it is unclear whether a major update should be performed when destructive changes are included in the **language specification (syntax or semantics)** or when they are included in the **public API of the software toolchain (compiler and runtime)**. In mimium, minor upgrades may currently include changes that break compatibility with the software's public API. This is because there are still few use cases where this toolchain is integrated into other systems.