# mimium Language Server

## How to use on VSCode

If you on VSCode, you can install binaries including this language server by installing mimium-language extension(it automatically downloads binaries on first launch).

On the other environment, download from github release page(mimium-bintools contains mimium-language-server).

You can format code with Option+Shift+F (from Command Palette, select "Format Document")

## Currently supported features

- Error diagnostics(syntactic error/type checking error)
- Semantic highlighting using compiler's parser
- Code Formatting through mimium-fmt(partial formatting for selected range is not supported for now)

## Analysis execution mode

`mimium-language-server` supports two analysis modes:

- `worker` (default): run parse/typecheck in a separate process (`mimium-language-server-worker`)
- `inprocess`: run parse/typecheck in the language server process

You can switch the mode with environment variable:

- `MIMIUM_LS_ANALYSIS_MODE=worker`
- `MIMIUM_LS_ANALYSIS_MODE=inprocess`

When `worker` mode is used, the worker binary path is resolved in this order:

1. `MIMIUM_LS_WORKER` (explicit absolute/relative path)
2. `mimium-language-server-worker` in the same directory as the language server executable

If worker mode is enabled and the worker cannot be launched, analysis does not fall back automatically.
In that case, set `MIMIUM_LS_ANALYSIS_MODE=inprocess` explicitly.