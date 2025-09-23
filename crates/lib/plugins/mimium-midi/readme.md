# mimium MIDI Plugin

MIDIPlugin provides 2APIs: `set_midi_port("port_name")` and `bind_midi_note_mono(channel,default_note,default_velocity)`.

`bind_midi_note_mono` returns getter function of (float,float) value which is updated asynchronously by midi note event.

(NoteOff is treated as NoteOn with 0 velocity).

Processing for raw MIDI events like midi plugin in VST cannot be realized for now.

(Note that MIDI devices are not available for WSL. I tested only on macOS.)


```rust
let _ = set_midi_port("from Max 1")
fn osc(freq){
   ...
}
fn midi_to_hz(note){
    440.0*  (2.0 ^((note-69.0)/12.0))
}
let boundval = bind_midi_note_mono(0.0,69.0,127.0);
fn dsp(){
    let (note,vel) = boundval();

    let sig = note |> midi_to_hz |> osc 
    let r = sig * (vel /127.0);
    (r,r)
}
```