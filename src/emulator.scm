;;(include "cpu.scm")
;;(include "disassembler.scm")

;; EMULATOR
(module
  (chip8 emulator)
  (emulate-si emulate emulate-instruction load-program-into-memory)
  (import scheme
          (chip8 common)
          (chip8 cpu)
          (chip8 disassembler)
          srfi-18)

  (define (emulate-si instruction *CPU*)
    (print (disassemble-si instruction (cpu-PC *CPU*)))
    (bitmatch instruction
              (((#x00EE 16))
               (let ((addr (pop *CPU*)))
                 (set-pc *CPU* addr)
                 (incr-pc *CPU*))
               )
              (((#x00E0 16))
               (clear-screen *CPU*)
               (incr-pc *CPU*))
              (((#x0 4) (address 12)) ;; call machine code at address
               (push *CPU* (get-pc *CPU*))
               (set-pc *CPU* address))
              (((#x1 4) (address 12)) ;; jump at address
               ;;(print "jump to " (tohex address))
               (set-pc *CPU* address))
              (((#x2 4) (address 12)) ;; Call subroutine
               (push *CPU* (get-pc *CPU*))
               (set-pc *CPU* address))
              (((#x3 4) (X 4) (NN 8)) ;; Skip next instruction if VX == NN
               (if (= (get-register *CPU* X) NN)
                 (incr-pc *CPU*)
                 '())
               (incr-pc *CPU*))
              (((#x4 4) (X 4) (NN 8))
               (if (not (= (get-register *CPU* X) NN))
                 (incr-pc *CPU*)
                 '())
               ;;(print "Skip next instruction if V" X " != " NN)
               (incr-pc *CPU*))
              (((#x5 4) (X 4) (Y 4) (#x0 4))
               ;;(print "Skip next instruction if V" X " == V" Y)
               (if (= (get-register *CPU* X) (get-register *CPU* Y))
                 (incr-pc *CPU*)
                 '())
               (incr-pc *CPU*))
              (((#x6 4) (X 4) (NN 8))
               (set-register *CPU* X NN)
               ;(print "V" X " = " NN)
               (incr-pc *CPU*))
              (((#x7 4) (X 4) (NN 8))
               (set-register *CPU* X
                             (bitwise-and (+ (get-register *CPU* X) NN)
                                          #xff))
               ;;(print "V" X " += " NN " => " (get-register *CPU* X))
               (incr-pc *CPU*))
              (((#x8 4) (X 4) (Y 4) (#x0 4))
               (set-register *CPU* X
                             (get-register *CPU* Y))
               ;(print "V" X " = V" Y)
               (incr-pc *CPU*))
              (((#x8 4) (X 4) (Y 4) (#x1 4))
               (set-register *CPU* X
                             (bitwise-ior (get-register *CPU* X)
                                          (get-register *CPU* Y)))
               (set-carry-flag *CPU* 0)
               ;(print "V" X " |= V" Y)
               (incr-pc *CPU*))
              (((#x8 4) (X 4) (Y 4) (#x2 4))
               (set-register *CPU* X
                             (bitwise-and (get-register *CPU* X)
                                          (get-register *CPU* Y)))
               (set-carry-flag *CPU* 0)
               ;(print "V" X " &= V" Y)
               (incr-pc *CPU*))
              (((#x8 4) (X 4) (Y 4) (#x3 4))
               (set-register *CPU* X
                             (bitwise-xor (get-register *CPU* X)
                                          (get-register *CPU* Y)))
               (set-carry-flag *CPU* 0)
               ;(print "V" X " ^= V" Y)
               (incr-pc *CPU*))
              (((#x8 4) (X 4) (Y 4) (#x4 4))
               (let ((sum (+ (get-register *CPU* X)
                             (get-register *CPU* Y))))
                 (if (> sum 255)
                   (begin (set-register *CPU* X
                                        (bitwise-and sum 255))
                          (set-carry-flag *CPU* 1))
                   (begin (set-register *CPU* X sum)
                          (set-carry-flag *CPU* 0))))
               ;(print "V" X " += V" Y)
               (incr-pc *CPU*))
              (((#x8 4) (X  4) (Y 4) (#x5 4))
               (let ((subtraction (- (get-register *CPU* X)
                                     (get-register *CPU* Y))))
                 (if (< subtraction 0)
                   (begin (set-register *CPU* X
                                        (bitwise-and subtraction 255))
                          (set-carry-flag *CPU* 0))
                   (begin (set-register *CPU* X subtraction)
                          (set-carry-flag *CPU* 1))))
               ;(print "V" X " -= V" Y)
               (incr-pc *CPU*))
              (((#x8 4) (X  4) (Y 4) (#x6 4))
               (let ((VX (get-register *CPU* X)))
                 (begin
                   (set-register *CPU* X (arithmetic-shift VX -1))
                   (set-carry-flag *CPU* (bitwise-and
                                           VX 1)))
                 ;;(print "V" X " >>= 1 -> " (get-register *CPU* X))
                 )
               (incr-pc *CPU*))
              (((#x8 4) (X  4) (Y 4) (#x7 4))
               (let ((subtraction (- (get-register *CPU* Y)
                                     (get-register *CPU* X))))
                 (if (< subtraction 0)
                   (begin (set-register *CPU* X
                                        (bitwise-and subtraction 255))
                          (set-carry-flag *CPU* 0))
                   (begin (set-register *CPU* X subtraction)
                          (set-carry-flag *CPU* 1))))
               ;;(print "V" X " -= V" Y)
               (incr-pc *CPU*))
              (((#x8 4) (X  4) (Y 4) (#xE 4))
               (let ((VX (get-register *CPU* X)))
                 (begin
                   (set-register *CPU* X (bitwise-and (arithmetic-shift VX 1) #xff))
                   (set-carry-flag *CPU* (bit-field VX 7 8))))
               ;;(print "V" X " <<= 1"))
               (incr-pc *CPU*))
              (((#x9 4) (X  4) (Y 4) (#x0 4))
               (if (not (= (get-register *CPU* X) (get-register *CPU* Y)))
                 (incr-pc *CPU*)
                 '())
               ;;(print "Skip the next instruction if VX != VY")
               (incr-pc *CPU*))
              (((#xA 4) (NNN  12))
               ;;(print "I = 0x" (tohex NNN))
               (cpu-I-set! *CPU* NNN)
               (incr-pc *CPU*))
              (((#xB 4) (NNN  12))
               (jump-to *CPU* (+ (get-register *CPU* 0) NNN)))
              (((#xC 4) (X 4) (NN  8))
               (set-register *CPU* X
                             (bitwise-and NN
                                          (pseudo-random-integer 255)))
               ;;(print "rand and " NN)
               (incr-pc *CPU*))
              (((#xD 4) (X 4) (Y 4) (N 4))
               (draw-sprite *CPU* (get-register *CPU* X) (get-register *CPU* Y) N)
               ;;(print "DRAW X: " (get-register *CPU* X) ", Y: " (get-register *CPU* Y) " height: " N)
               (incr-pc *CPU*))
              (((#xE 4) (X 4) (#x9 4) (#xE 4))
               (let ((k (key-pressed *CPU* (get-register *CPU* X))))
                 (if k ;; TODO test key()
                   (incr-pc *CPU*)
                   '())
                 ;(print "skip if KEY(" k ") == V" X " (" (get-register *CPU* X) ")")
                 )
               (incr-pc *CPU*))
              (((#xE 4) (X 4) (#xA 4) (#x1 4))
               (let ((k (key-pressed *CPU* (get-register *CPU* X))))
                 (if (not k) ;; TODO test key()
                   (incr-pc *CPU*)
                   '())
                 ;(print "skip if KEY(" k ") != V" X " (" (get-register *CPU* X) ")")
                 )
               (incr-pc *CPU*))
              (((#xF 4) (X 4) (#x0 4) (#x7 4))
               (set-register *CPU* X (cpu-delay-timer *CPU*))
               ;;(print "V" X " = get_delay")
               (incr-pc *CPU*))
              (((#xF 4) (X 4) (#x0 4) (#xA 4))
               ;;(print "WAITNG FOR KEY PRESS")
               (let ((k (vector-index (cpu-keys *CPU*) #t)))
                 (if k
                   (begin
                     ;;(print "KEY PRESSED V" X " = get_key => " k)
                     (set-register *CPU* X k)
                     (incr-pc *CPU*))
                   '()
                   )
                 ))
              (((#xF 4) (X 4) (#x1 4) (#x5 4))
               ;;(print "delay_timer = V" X)
               (cpu-delay-timer-set! *CPU* (get-register *CPU* X))
               (incr-pc *CPU*))
              (((#xF 4) (X 4) (#x1 4) (#x8 4))
               ;; TODO implement sound_timer
               ;;(print "sound_timer = V" X)
               (cpu-sound-timer-set! *CPU* (get-register *CPU* X))
               (incr-pc *CPU*))
              (((#xF 4) (X 4) (#x1 4) (#xE 4))
               (cpu-I-set! *CPU* (+ (cpu-I *CPU*)
                                    (get-register *CPU* X)))
               ;(print "I += V" X)
               (incr-pc *CPU*))
              (((#xF 4) (X 4) (#x2 4) (#x9 4))
               (let ((sprite_addr (font-location (get-register *CPU* X))))
                 ;;(print "I = SpriteAddress VX " (get-register *CPU* X) " -> addr: 0x" (tohex sprite_addr)) ;; TODO come funzionano gli sprite
                 (cpu-I-set! *CPU* sprite_addr))
               (incr-pc *CPU*))
              (((#xF 4) (X 4) (#x3 4) (#x3 4))
               (let ((num (get-register *CPU* X)))
                 (let ((n1 (digit num 2))
                       (n2 (digit num 1))
                       (n3 (digit num 0)))
                   (set-memory *CPU* (cpu-I *CPU*) n1)
                   (set-memory *CPU* (+ (cpu-I *CPU*) 1) n2)
                   (set-memory *CPU* (+ (cpu-I *CPU*) 2) n3)))
               ;;(print "set_BCD V" X)
               (incr-pc *CPU*))
              (((#xF 4) (X 4) (#x5 4) (#x5 4))
               (reg-dump *CPU* X)
               (incr-pc *CPU*))
              (((#xF 4) (X 4) (#x6 4) (#x5 4))
               (reg-load *CPU* X)
               (incr-pc *CPU*))
              (else
                (bitmatch instruction
                          (((a 4) (b 4) (c 4) (d 4))
                           (printf "[NOT IMPLEMENTED] ~s: ~s ~s ~s ~s \n" (cpu-PC *CPU*) a b c d)
                           (exit)
                           )))))
  ;;  The uppermost 256 bytes (0xF00-0xFFF) are reserved for display refresh,
  ;; and the 96 bytes below that (0xEA0-0xEFF) were reserved for the call stack, internal use, and other variables. 

  (define (load-file file)
    (call-with-input-file file
                          (lambda (port) (read-u8vector #f port))))



  ;; Returns nothing
  (define (emulate *CPU*)
    (define (iter)
      (cond
        ((> (cpu-PC *CPU*) (u8vector-length (cpu-memory *CPU*))) (print "END"))
        ((not (cpu-running *CPU*)) (print "CPU not running"))
        ((cpu-pause *CPU*)
         (thread-sleep! 0.01) ;; do nothing
         (iter))
        (else
          (emulate-si (fetch *CPU*) *CPU*)
          (thread-sleep! 0.002) ;; 500Hz
          (iter))))
    (iter))

  (define (emulate-instruction *CPU*)
    (cond
      ((> (cpu-PC *CPU* ) (u8vector-length (cpu-memory *CPU*))) (print "END " (tohex (cpu-PC *CPU*))))
      (else
        (let ((instr (fetch *CPU*)))
          (emulate-si instr *CPU*)))))


  (define (pong *CPU*)
    (load-program-into-memory "PONG" *CPU*)
    (emulate *CPU*))

  (define (test-keypad *CPU*)
    (load-program-into-memory "6-keypad.ch8" *CPU*)
    (emulate *CPU*))

  (define (test *CPU*)
    (load-program-into-memory "1-chip8-logo.ch8" *CPU*)
    (emulate *CPU*))

  (define (test-1 *CPU*)
    (load-program-into-memory "test_opcode.ch8" *CPU*)
    (emulate *CPU*))

  (define (test-flags *CPU*)
    (load-program-into-memory "4-flags.ch8" *CPU*)
    (emulate *CPU*))
  )
