;;(include "cpu.scm")

(module
  (chip8 disassembler)
  (disassemble disassemble-si)
  (import scheme
          (chip8 common)
          (chip8 cpu))

  ;; Need an #u8 array of two elements e.g. #u8(#x00 #xEE)
  (define (disassemble-si instruction pc)
    (define (fmt format . args)
      (apply sprintf
             (string-append "0x~X\t" format)
             (cons pc args)))
    (define (f-const var)
      (tohex var))
    (define (f-reg var)
      (string-append "v" (number->string var 16)))
    (define (f-addr a)
      (tohex a))
    (bitmatch instruction
              (((#x00EE 16))
               (fmt "ret"))
              (((#x00E0 16))
               (fmt "cls"))
              (((#x0 4) (address 12)) ;; call machine code at address
               (fmt "call mc ~A" (f-addr address)))
              (((#x1 4) (address 12)) ;; jump at address
               (fmt "jp ~A" (f-addr address)))
              (((#x2 4) (address 12)) ;; Call subroutine
               (fmt "call ~A" (f-addr address)))
              (((#x3 4) (X 4) (NN 8)) ;; Skip next instruction if VX == NN
               (fmt "se ~A, ~A" (f-reg X) (f-const NN)))
              (((#x4 4) (X 4) (NN 8))
               (fmt "sne ~A, ~A" (f-reg X) (f-const NN)))
              (((#x5 4) (X 4) (Y 4) (#x0 4))
               (fmt "se ~A, ~A" (f-reg X) (f-reg Y)))
              (((#x6 4) (X 4) (NN 8))
               (fmt "ld ~A, ~A" (f-reg X) (f-const NN)))
              (((#x7 4) (X 4) (NN 8))
               (fmt "add ~A, ~A" (f-reg X) (f-const NN)))
              (((#x8 4) (X 4) (Y 4) (#x0 4))
               (fmt "ld ~A, ~A" (f-reg X) (f-reg Y)))
              (((#x8 4) (X 4) (Y 4) (#x1 4))
               (fmt "or ~A, ~A" (f-reg X) (f-reg Y)))
              (((#x8 4) (X 4) (Y 4) (#x2 4))
               (fmt "and ~A, ~A" (f-reg X) (f-reg Y)))
              (((#x8 4) (X 4) (Y 4) (#x3 4))
               (fmt "xor ~A, ~A" (f-reg X) (f-reg Y)))
              (((#x8 4) (X 4) (Y 4) (#x4 4))
               (fmt "add ~A, ~A" (f-reg X) (f-reg Y)))
              (((#x8 4) (X  4) (Y 4) (#x5 4))
               (fmt "sub ~A, ~A" (f-reg X) (f-reg Y)))
              (((#x8 4) (X  4) (Y 4) (#x6 4))
               (fmt "shr ~A, ~A" (f-reg X) (f-reg Y)))
              (((#x8 4) (X  4) (Y 4) (#x7 4))
               (fmt "subn ~A, ~A" (f-reg X) (f-reg Y)))
              (((#x8 4) (X  4) (Y 4) (#xE 4))
               (fmt "shl ~A, ~A" (f-reg X) (f-reg Y)))
              (((#x9 4) (X  4) (Y 4) (#x0 4))
               (fmt "sne ~A, ~A" (f-reg X) (f-reg Y)))
              (((#xA 4) (NNN  12))
               (fmt "ld i, ~A"  (f-addr NNN)))
              (((#xB 4) (NNN  12))
               (fmt "jp v0 + ~A" (f-addr 12)))
              (((#xC 4) (X 4) (NN  8))
               (fmt "rnd ~A, ~A" (f-reg X) (f-const NN)))
              (((#xD 4) (X 4) (Y 4) (N 4))
               (fmt "drw ~A, ~A, ~A" (f-reg X) (f-reg Y) (f-const N)))
              (((#xE 4) (X 4) (#x9 4) (#xE 4))
               (fmt "skp key, ~A" (f-reg X)))
              (((#xE 4) (X 4) (#xA 4) (#x1 4))
               (fmt "sknp key, ~A" (f-reg X)))
              (((#xF 4) (X 4) (#x0 4) (#x7 4))
               (fmt "ld ~A, dt" (f-reg X)))
              (((#xF 4) (X 4) (#x0 4) (#xA 4))
               (fmt "ld ~A, key (wait for key)" (f-reg X)))
              (((#xF 4) (X 4) (#x1 4) (#x5 4))
               (fmt "ld dt, ~A" (f-reg X)))
              (((#xF 4) (X 4) (#x1 4) (#x8 4))
               (fmt "ld st, ~A" (f-reg X)))
              (((#xF 4) (X 4) (#x1 4) (#xE 4))
               (fmt "add i, ~A" (f-reg X)))
              (((#xF 4) (X 4) (#x2 4) (#x9 4))
               (fmt "ld f, ~A" (f-reg X)))
              (((#xF 4) (X 4) (#x3 4) (#x3 4))
               (fmt "ld b, ~A" (f-reg X)))
              (((#xF 4) (X 4) (#x5 4) (#x5 4))
               (fmt "ld [i], ~A" (f-reg X)))
              (((#xF 4) (X 4) (#x6 4) (#x5 4))
               (fmt "ld ~A, [i]" (f-reg X)))
              (else
                (bitmatch instruction
                          (((a 4) (b 4) (c 4) (d 4))
                           (fmt "[NOT IMPLEMENTED] ~s ~s ~s ~s \n"  a b c d)
                           )))))

  (define (disassemble program-file)
    (define c (init-cpu))
    (define (iter size)
      (cond
        ((>= (cpu-PC c) (+ PROGRAM_MEMORY_START size)) (print "END"))
        (else
          (print (disassemble-si (fetch c) (cpu-PC c)))
          (incr-pc c)
          (iter size))))
    (let ((size(load-program-into-memory program-file c)))
      (iter size)))
  )