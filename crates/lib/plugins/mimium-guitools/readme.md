# mimium-guitools

This plugin provides various ways to interact gui through `egui` crate. Works only on native architecture.

## OSC support (optional feature)

Enable the `osc` feature to accept OpenSoundControl updates for sliders:

```toml
mimium-guitools = { version = "*", features = ["osc"] }
```

When enabled, the GUI listens on `127.0.0.1:9000` by default (override with `MIMIUM_GUITOOLS_OSC_BIND`).
`Control!("Params", {freq=440, phase=0.1})` can then be updated by sending OSC messages such as:

- `/Params/freq 440.0`
- `/Params/phase 0.1`

## Plot signals

```rust
fn osc(freq){
    ...
}
let probe1 = make_probe("probe_label1")//when more than 1 probes are created, gui window will be launched on the startup.

fn dsp(){
    let r = probe1(osc(440)) // probe closure returns original value.
    (r,r)
}

```
