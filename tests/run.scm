(import scheme
        (chicken load)
        test)

(load-relative "../src/common.scm")
(import (chip8 common))

(load-relative "../src/cpu.scm")
(import (prefix (chip8 cpu) "cpu:"))

(load-relative "../src/disassembler.scm")
(import (chip8 disassembler))

(load-relative "../src/emulator.scm")

(import (chip8 emulator))
;;(load-relative "../src/main.scm")

(test-group
  "CPU Instructions emulate-si"
   (test 
     "inst ret" #x302 (let ((main-cpu (cpu:init-cpu)))
      (begin
        (cpu:push main-cpu #x300)
        (emulate-si #u8(#x00 #xEE) main-cpu)
        (cpu:get-pc main-cpu))))
    (test
      "inst CLS" #x202 (let ((main-cpu (cpu:init-cpu)))
      (begin
        (emulate-si #u8(#x00 #xE0) main-cpu)
        (cpu:get-pc main-cpu))))
    (test-group
      "instruction call"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (emulate-si #u8(#x03 #x00) cpu)
          (test "call jump to address" #x300 (cpu:get-pc cpu))
          (test "call push caller address" #x200 (cpu:pop cpu)))))
    (test-group
      "inst skip next instruction if VX == NN"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #x33)
          (emulate-si #u8(#x31 #x33) cpu)
          (test "skip instruction if equal constant" #x204 (cpu:get-pc cpu)))))
    (test-group
      "inst skip next instruction if VX != NN"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #x13)
          (emulate-si #u8(#x41 #x14) cpu)
          (test "skip instruction if different" #x204 (cpu:get-pc cpu)))))
    (test-group
      "Skip next instruction if VX == VY"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #x12)
          (cpu:set-register cpu 2 #x12)
          (emulate-si #u8(#x51 #x10) cpu)
          (test "skip instruction if two registers equal" #x204 (cpu:get-pc cpu)))))
    (test-group
      "mov VX, value"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (emulate-si #u8(#x63 #x10) cpu)
          (test "VX content" #x10 (cpu:get-register cpu 3))
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "VX += NN"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #x12)
          (emulate-si #u8(#x71 #x11) cpu)
          (test "VX += NN" #x23 (cpu:get-register cpu 1))
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "VX = VY"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 2 #x12)
          (emulate-si #u8(#x81 #x20) cpu)
          (test "VX = VY" #x12 (cpu:get-register cpu 1))
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "VX |= VY"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #b110010)
          (cpu:set-register cpu 2 #b001011)
          (emulate-si #u8(#x81 #x21) cpu)
          (test "VX |= VY" #b111011 (cpu:get-register cpu 1))
          (test "carry flag" 0 (cpu:get-carry-flag cpu))
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "VX &= VY"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #b110010)
          (cpu:set-register cpu 2 #b001011)
          (emulate-si #u8(#x81 #x22) cpu)
          (test "VX &= VY" #b000010 (cpu:get-register cpu 1))
          (test "carry flag" 0 (cpu:get-carry-flag cpu))
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "VX ^= VY"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #b110010)
          (cpu:set-register cpu 2 #b001011)
          (emulate-si #u8(#x81 #x23) cpu)
          (test "VX ^= VY" #b111001 (cpu:get-register cpu 1))
          (test "carry flag" 0 (cpu:get-carry-flag cpu))
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "VX += VY"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #x14)
          (cpu:set-register cpu 2 #x36)
          (emulate-si #u8(#x81 #x24) cpu)
          (test "VX += VY"  #x4a (cpu:get-register cpu 1))
          (test "carry flag" 0 (cpu:get-carry-flag cpu))
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "VX -= VY"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #x36)
          (cpu:set-register cpu 2 #x12)
          (emulate-si #u8(#x81 #x25) cpu)
          (test "VX -= VY" #x24 (cpu:get-register cpu 1))
          (test "carry flag" 1 (cpu:get-carry-flag cpu)) ;; TODO check what should I get in reality?
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "VX >>= 1"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #b110010)
          (emulate-si #u8(#x81 #x06) cpu)
          (test "VX >>= VY" #b11001 (cpu:get-register cpu 1))
          (test "carry flag" 0 (cpu:get-carry-flag cpu))
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "another VY -= VX"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #x36)
          (cpu:set-register cpu 2 #x12)
          (emulate-si #u8(#x82 #x17) cpu)
          (test "VY -= VX" #x24 (cpu:get-register cpu 2))
          (test "carry flag" 1 (cpu:get-carry-flag cpu)) ;; TODO check what should I get in reality?
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "VX <<= 1"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #b110010)
          (emulate-si #u8(#x81 #x0E) cpu)
          (test "VX <<= VY" #b1100100 (cpu:get-register cpu 1))
          (test "carry flag" 0 (cpu:get-carry-flag cpu))
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "inst skip next instruction if VX != VY"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (cpu:set-register cpu 1 #x13)
          (cpu:set-register cpu 2 #x00)
          (emulate-si #u8(#x91 #x20) cpu)
          (test "skip instruction if different" #x204 (cpu:get-pc cpu)))))
    (test-group
      "I = NNN"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (emulate-si #u8(#xA5 #x43) cpu)
          (test "I" #x543 (cpu:cpu-I cpu))
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group
      "JMP to NNN"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (emulate-si #u8(#xB3 #x43) cpu)
          (test "pc increment" #x343 (cpu:get-pc cpu)))))
    (test-group ;; TODO improve test
      "RAND"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (emulate-si #u8(#xD1 #x41) cpu)
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group ;; TODO improve test
      "draw sprite"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (emulate-si #u8(#xD2 #x34) cpu)
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group ;; TODO placeholder test
      "skip if key pressed == VX"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (emulate-si #u8(#xE1 #x9E) cpu)
          (test "pc increment" #x202 (cpu:get-pc cpu)))))
    (test-group ;; TODO placeholder test
      "skip if key pressed != VX"
      (let ((cpu (cpu:init-cpu)))
        (begin
          (emulate-si #u8(#xE1 #xA1) cpu)
          (test "pc increment" #x204 (cpu:get-pc cpu)))))
  )

(define-syntax cpu-test
  (syntax-rules ()
    ((_ name cpu (setup ...) expected check)
       (begin
         (set! cpu (cpu:init-cpu))
         setup ...
         (test name expected check)
         ))))

(test-group
  "CPU module"
  (cpu-test "fetch nothing" cpu ((load-program-into-memory "tests/rom_empty" cpu)) #u8(#x00 #x00) (cpu:fetch cpu))
  (cpu-test "fetch rom0" cpu ((load-program-into-memory "tests/rom0" cpu)) #u8(#x6a #x02) (cpu:fetch cpu))
  )

(test-exit)
