# mimium-symphonia

An external function implementation to read audio files in mimium using [Symphonia](https://github.com/pdeljanov/Symphonia/) crate.

## Example

```rust
//Sampler_mono! macro returns a closure that takes playback position in samples as an argument
let sampler = Sampler_mono!("test.wav")
fn counter(){
    self+1
}
fn dsp(){
    counter() |> sampler
}
```

## Current status

- The `Sampler_mono!` macro loads audio files at compile time and caches them in the plugin instance.
- An argument for `Sampler_mono!` is a path relative to the current working directory (typically the crate root during compilation), or an absolute path.
 - Currently only string literals are supported as arguments (parametric file loading is not supported).
- Supported file formats are the same as [what `symphonia` can decode](https://github.com/pdeljanov/Symphonia?tab=readme-ov-file#codecs-decoders)
- Currently, only mono files are supported (other formats will cause panic on runtime).
- An index for sampler is a raw position of samples (samplerate mismatch between audio driver and audio file are not handled. Resampler is not implemented as a part of symphonia, but there's an example code of resampler in symphonia-play application, it can be derived in the future).
- An index does not need to be an integer; boundary checks and linear interpolation are performed on read (access out of range returns 0.0).
- The audio data loaded by the macro is cached and reused across re-compilations based on the file path.