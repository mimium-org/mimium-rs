# Binary packages for mimium

This directory is binary packages for mimium.

- mimium-cli -- main compiler
- mimium-language-server -- language server for IDE support (see also [VSCode Extension](https://github.com/mimium-org/mimium-language) )
- mimium-fmt -- experimental code formatter

Due to the limitation of the package distribution system `cargo dist`, these 3 pckages are not defiened as binary crate but defined as library crate, and we have another "mimium-bintools" package which packs these binary executables as one crates.