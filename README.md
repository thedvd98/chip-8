# CHIP-8 emulator

A CHIP-8 emulator written in CHICKEN Scheme 5 with SDL2.

## Development on Guix

`manifest.scm` provides CHICKEN 5, the C toolchain and SDL2. The eggs are not
packaged in Guix, so they are installed into the project-local directory
`.chicken/eggs` (ignored by git).

```
guix shell -m manifest.scm
make eggs        # first time only
make test
make run ROM=PONG
```

`make help` lists all targets. To use `csi`/`chicken-install` directly in the
shell, run `. scripts/env.sh` first. With `guix shell --pure`, keep the display
variables so SDL can open a window:
`guix shell --pure -m manifest.scm --preserve='^DISPLAY$' --preserve='^WAYLAND_DISPLAY$' --preserve='^XDG_RUNTIME_DIR$'`

For a reproducible environment, pin the channels with
`guix describe -f channels > channels.scm` and use
`guix time-machine -C channels.scm -- shell -m manifest.scm`.

## Install dependencies (other systems)
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
