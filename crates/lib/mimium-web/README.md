# mimium-web

`mimium-web` is the WebAssembly wrapper crate for running mimium in browser environments.

## Build

```sh
wasm-pack build crates/lib/mimium-web --target web
```

Generated package files are placed in `crates/lib/mimium-web/pkg`.

## Browser loading configuration

When using mimium in the browser, there are two URL settings:

- stdlib preload base URL: `Context::init_lib_cache_with_base_url(...)`
- user module preload base URL: `Context::set_module_base_url(...)`

Both settings accept:

- Raw content URL (`https://raw.githubusercontent.com/...`)
- GitHub page URL (`https://github.com/<owner>/<repo>/tree/<ref>/...`)
- GitHub blob URL (`https://github.com/<owner>/<repo>/blob/<ref>/...`)

GitHub `tree/blob` URLs are normalized internally to `raw.githubusercontent.com` before fetching files.

### Example

```js
import init, { Config, Context } from './pkg/mimium_web.js';

await init();

const config = Config.new();
config.sample_rate = 48000;
config.buffer_size = 1;

const ctx = new Context(config);

await ctx.init_lib_cache_with_base_url(
  'https://github.com/mimium-org/mimium-rs/tree/v4.0.0-alpha/lib'
);

ctx.set_module_base_url(
  'https://github.com/mimium-org/mimium-rs/tree/v4.0.0-alpha/examples/'
);

await ctx.compile(sourceCode);
```

## Compile API behavior

- `compile(...)` and `recompile(...)`
  - run stdlib preload
  - run network dependency preload for `mod` and `include`
- `compile_direct(...)` and `recompile_direct(...)`
  - skip preload steps and compile immediately

Use direct APIs only when all dependencies are already available in virtual cache.

## Testing

E2E tests are in `crates/lib/mimium-web/e2e` and can be run with:

```sh
cd crates/lib/mimium-web
npm run test:e2e
```

## Chrome AudioWorklet note (network loading)

In Chrome, `AudioWorkletGlobalScope` may not provide `fetch` / `XMLHttpRequest`.
For network-based library loading, use this flow:

1. Main thread: preload over network
2. Main thread: export virtual cache JSON
3. Send JSON to AudioWorklet via `postMessage`
4. Worklet: import virtual cache JSON
5. Worklet: call `compile_direct(...)` / `recompile_direct(...)`

APIs for this flow:

- `Context::export_virtual_file_cache_json()`
- `Context::import_virtual_file_cache_json(payload)`

This keeps network access on the main thread while still allowing module/include loading in AudioWorklet.
