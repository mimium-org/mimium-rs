# mimium-guitools

This plugin provides various ways to interact gui through `egui` crate. Works only on native architecture.

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

