# mimium-fmt

A code formatter for the mimium programming language.

## Features

- **CST-based formatting** (default): Preserves comments and original source structure
- **Configurable line width**: Control when expressions break across lines
- **Configurable indentation**: Set your preferred indentation size

## Usage

```bash
# Format a file with default settings (width: 80, indent: 4)
mimium-fmt example.mmm

# Format with custom width
mimium-fmt --width 100 example.mmm

# Format with custom indentation
mimium-fmt --indent-size 2 example.mmm

# Read from stdin
cat example.mmm | mimium-fmt
```

## Options

| Option | Default | Description |
|--------|---------|-------------|
| `--width <WIDTH>` | 80 | Target line width for formatting |
| `--indent-size <SIZE>` | 4 | Number of spaces for indentation |
| `--ast` | - | Use legacy AST-based formatter (does not preserve comments) |

## Example

Input:

```mimium
fn synth(note,vel){let sig=note|>midi_to_hz|>osc
let gain=vel/127.0
sig*gain}
```

Output (width=80):

```mimium
fn synth(note, vel){
    let sig = note |> midi_to_hz |> osc
    let gain = vel / 127.0
    sig * gain
}
```

Output (width=20):

```mimium
fn synth(note, vel){
    let sig = note
        |> midi_to_hz
        |> osc
    let gain = vel /
        127.0
    sig * gain
}
```

## Library Usage

```rust
use mimium_fmt::{pretty_print_cst, pretty_print};

// CST-based formatting (recommended)
let formatted = pretty_print_cst(source, &Some(path), 80)?;

// Legacy AST-based formatting
let formatted = pretty_print(source, &Some(path), 80)?;
```
