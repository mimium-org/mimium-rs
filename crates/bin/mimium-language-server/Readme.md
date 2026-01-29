# mimium Language Server

## How to use on VSCode

If you on VSCode, you can install binaries including this language server by installing mimium-language extension(it automatically downloads binaries on first launch).

On the other environment, download from github release page(mimium-bintools contains mimium-language-server).

You can format code with Option+Shift+F (from Command Palette, select "Format Document")

## Currently supported features

- Error diagnostics(syntactic error/type checking error)
- Semantic highlighting using compiler's parser
- Code Formatting through mimium-fmt(partial formatting for selected range is not supported for now)