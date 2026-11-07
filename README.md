# CHIP-8 emulator

A CHIP-8 emulator written in CHICKEN Scheme 5 with SDL2.

## Install dependencies
Requires CHICKEN 5 and the SDL2 development libraries.
```
chicken-install sdl2 srfi-151 srfi-18 miscmacros bitstring test args
```

## RUN TESTS

`csi tests/run.scm`

## RUN
```
csi -s src/main.scm -i 1-chip8-logo.ch8
```

### Keys
- `0`-`9`, `a`-`f`: CHIP-8 keypad
- `r`: reset and reload the current ROM
- `q` / `Esc`: quit
