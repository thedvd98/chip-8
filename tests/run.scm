(import scheme
        (chicken load)
        test
        bitstring)

(load-relative "../src/common.scm")
(import (chip8 common))

(load-relative "../src/cpu.scm")
(import (chip8 cpu))

(load-relative "../src/disassembler.scm")
(import (chip8 disassembler))

(load-relative "../src/emulator.scm")

(import (chip8 emulator))
;;(load-relative "../src/main.scm")

(test-group
  "CPU Instructions"
   (test 
     "inst ret" #x302 (let ((main-cpu (init-cpu)))
      (begin
        (push main-cpu #x300)
        (emulate-si #u8(#x00 #xEE) main-cpu)
        (get-pc main-cpu))))
    (test
      "inst CLS" #x202 (let ((main-cpu (init-cpu)))
      (begin
        (emulate-si #u8(#x00 #xE0) main-cpu)
        (get-pc main-cpu))))
  )

(test-exit)
