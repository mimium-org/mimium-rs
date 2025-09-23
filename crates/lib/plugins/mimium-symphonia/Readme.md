# mimium-symphonia

An external function implementation to read audio files in mimium using [Symphonia](https://github.com/pdeljanov/Symphonia/) crate.

## Example

```rust
//gen_sampler_mono returns higher-order function that takes playback position in samples as an argument
let sampler = gen_sampler_mono("test.wav")
fn counter(){
    self+1
}
fn dsp(){
    counter() |> sampler
}
```

*`gen_sampler_mono` should be called only on global evaluation context.*

## Current status

- An argument for `gen_sampler_mono` is a relative path from the source `.mmm`, or an absolute path.
 - Currently there's no operation is supported on string type (only the literal is supported) in mimium, parametric file loading can not be realized for now.
- Supported file format are the same as [what `symphonia` can decode](https://github.com/pdeljanov/Symphonia?tab=readme-ov-file#codecs-decoders)
- Currently, only mono files are supported (other formats will cause panic on runtime).
- An index for sampler is a raw position of samples (now, samplerate mismatch between audio driver and audio file are not handled. Resampler is not implemented as a part of symphonia, but there's an example code of resampler in symphonia-play application, it can be derived in the future).
- An index are not needed to be integer, a boundary check and linear interpolation are performed on read.(Aceess out of range returns 0.0 .)
- `gen_sampler_mono` function internally creates external Rust closure that holds Vec as internal data(The allocated external closure will never be released in the current implementation. )