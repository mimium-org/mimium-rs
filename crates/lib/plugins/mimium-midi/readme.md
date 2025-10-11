# mimium MIDI Plugin

MIDIPlugin provides APIs for MIDI input handling.

## Functions

- `set_midi_port("port_name")`: Set the MIDI input port to use

## Macros

- `midi_note_mono!(channel, default_note, default_velocity)`: Returns a record `{pitch:float, velocity:float}` that is updated asynchronously by MIDI note events

(NoteOff is treated as NoteOn with 0 velocity).

Processing for raw MIDI events like midi plugin in VST cannot be realized for now.

(Note that MIDI devices are not available for WSL. I tested only on macOS.)

## Example (New API with macro)

```mimium
fn osc(freq){
   ...
}
fn midi_to_hz(note){
    440.0*  (2.0 ^((note-69.0)/12.0))
}
fn dsp(){
    let note_data = midi_note_mono!(0,69,127);
    let sig = note_data.pitch |> midi_to_hz |> osc 
    let r = sig * (note_data.velocity /127.0);
    (r,r)
}
```