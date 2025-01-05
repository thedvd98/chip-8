(import scheme
        (chicken load)
        test
        bitstring)

(load-relative "./src/common.scm")
(import (chip8 common))

(load-relative "./src/cpu.scm")
(import (chip8 cpu))

(load-relative "./src/disassembler.scm")
(import (chip8 disassembler))

(load-relative "./src/emulator.scm")

(import (chip8 emulator))
