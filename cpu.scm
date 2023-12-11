;;https://wiki.call-cc.org/eggref/5/bitstring
;;; http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
;; https://wiki.call-cc.org/man/5/Module%20srfi-4

(import bitstring
        (chicken random)
        srfi-4  ;; u8vector
        srfi-151) ;; bitwise opeartions

(define fontset
  #u8(#xf0 #x90 #x90 #x90 #xf0   ; 0
           #x20 #x60 #x20 #x20 #x70   ; 1
           #xf0 #x10 #xf0 #x80 #xf0   ; 2
           #xf0 #x10 #xf0 #x10 #xf0   ; 3
           #x90 #x90 #xf0 #x10 #x10   ; 4
           #xf0 #x80 #xf0 #x10 #xf0   ; 5
           #xf0 #x80 #xf0 #x90 #xf0   ; 6
           #xf0 #x10 #x20 #x40 #x40   ; 7
           #xf0 #x90 #xf0 #x90 #xf0   ; 8
           #xf0 #x90 #xf0 #x10 #xf0   ; 9
           #xf0 #x90 #xf0 #x90 #x90   ; A
           #xe0 #x90 #xe0 #x90 #xe0   ; B
           #xf0 #x80 #x80 #x80 #xf0   ; C
           #xe0 #x90 #x90 #x90 #xe0   ; D
           #xf0 #x80 #xf0 #x80 #xf0   ; E
           #xf0 #x80 #xf0 #x80 #x80)) ; F

(define SCREEN_HEIGHT 64)
(define SCREEN_WIDTH 32)
(define screen-memory
  (make-vector SCREEN_HEIGHT
                 (make-u8vector SCREEN_WIDTH 0)))

(define PC 0)
(define I 0) ;; address register 12 bit
(define SP #xEA0)

(define registers
  (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(define (set-pc newpc)
  (set! PC newpc))
(define (get-pc)
  PC)

(define (set-register X val)
  (vector-set! registers X val))
(define (get-register X)
  (vector-ref registers X))
(define (set-carry-flag val)
  (set-register 15 val))
(define (get-carry-flag val)
  (get-register 15 val))
(define (set-I val)
  (set! I val))
(define (get-I)
  I)
(define (get-sp)
  SP)
(define (inc-sp)
  (+ SP 1))
(define (dec-sp)
  (- SP 1))

(define (fetch mem)
  (list->u8vector (list (u8vector-ref mem PC)
                        (u8vector-ref mem (+ PC 1)))))
(define (incr-pc)
  (set-pc (+ (get-pc) 2)))

;; stack only used to store return addresses
(define (push mem addr)
  (u8vector-set! mem (get-sp) addr)
  (inc-sp)
  (print "push MEM"))

(define (pop)
  (dec-sp)
  (get-sp))

(define (clear-screen)
  (let ((ext-len (vector-length screen-memory))
        (int-len (u8vector-length (vector-ref screen-memory 0))))
    (do ((i 0 (+ i 1)))
      ((= i ext-len) '())
      (do ((j 0 (+ j 1)))
          ((= j int-len) '())
        (u8vector-set! (vector-ref screen-memory i) j 0)))))

(define (emulate-si instruction mem)
  (bitmatch instruction
            (((#x00EE 16))
             (set-pc (pop)) ;; TODO check everything is file
             )
            (((#x00E0 16))
             (clear-screen)
             (incr-pc))
            (((#x0 4) (address 12)) ;; call machine code at address
             (push mem address)
             (set-pc address))
            (((#x1 4) (address 12)) ;; jump at address
             (set-pc address))
            (((#x2 4) (address 12)) ;; Call subroutine
             (push mem address)
             (set-pc address))
            (((#x3 4) (X 4) (NN 8)) ;; Skip next instruction if VX == NN
             (set-pc (+ (get-pc) 2))
             (incr-pc))
            (((#x4 4) (X 4) (NN 8))
             (print "Skip next instruction if V" X " != " NN)
             (incr-pc))
            (((#x5 4) (X 4) (Y 4) (#x0))
             (print "Skip next instruction if V" X " == V" Y)
             (incr-pc))
            (((#x6 4) (X 4) (NN 8))
             (set-register X NN)
             (print "V" X " = " NN)
             (incr-pc))
            (((#x7 4) (X 4) (NN 8))
             (set-register X
                           (+ (get-register X) NN))
             (print "V" X " += " NN)
             (incr-pc))
            (((#x8 4) (X 4) (Y 4) (#x0 4))
             (set-register X
                           (get-register Y))
             (print "V" X " = V" Y)
             (incr-pc))
            (((#x8 4) (X 4) (Y 4) (#x1 4))
             (set-register X
                           (bitwise-ior (get-register X)
                                        (get-register Y)))
             (print "V" X " |= V" Y)
             (incr-pc))
            (((#x8 4) (X 4) (Y 4) (#x2 4))
             (set-register X
                           (bitwise-and (get-register X)
                                        (get-register Y)))
             (print "V" X " &= V" Y)
             (incr-pc))
            (((#x8 4) (X 4) (Y 4) (#x3 4))
             (set-register X
                           (bitwise-xor (get-register X)
                                        (get-register Y)))
             (print "V" X " ^= V" Y)
             (incr-pc))
            (((#x8 4) (X 4) (Y 4) (#x4 4))
             (let ((sum (+ (get-register X)
                           (get-register Y))))
               (if (> sum 255)
                   (begin (set-register X
                                        (bitwise-and sum 255))
                          (set-carry-flag 1))
                   (begin (set-register X sum)
                          (set-carry-flag 0))))
             (print "V" X " += V" Y)
             (incr-pc))
            (((#x8 4) (X  4) (Y 4) (#x5 4))
             (let ((subtraction (- (get-register X)
                                   (get-register Y))))
               (if (< subtraction 0)
                   (begin (set-register X
                                        (bitwise-and subtraction 255))
                          (set-carry-flag 0))
                   (begin (set-register X subtraction)
                          (set-carry-flag 1))))
             (print "V" X " -= V" Y)
             (incr-pc))
            (((#x8 4) (X  4) (Y 4) (#x6 4))
             (let ((VX (get-register X)))
               (begin (set-carry-flag (bitwise-and
                                       VX 1))
                      (set-register X (bit-field VX 1 8)))
             (print "V" X " >>= 1")))
             (((#x8 4) (X  4) (Y 4) (#x7 4))
              (let ((subtraction (- (get-register Y)
                                    (get-register X))))
                (if (< subtraction 0)
                    (begin (set-register X
                                         (bitwise-and subtraction 255))
                           (set-carry-flag 0))
                    (begin (set-register X subtraction)
                           (set-carry-flag 1))))
              (print "V" X " -= V" Y)
              (incr-pc))
             (((#x8 4) (X  4) (Y 4) (#xE 4))
              (let ((VX (get-register X)))
                (begin (set-carry-flag (bit-field VX 7 8))
                       (set-register X (bit-field VX 0 7)))
                (print "V" X " <<= 1"))
              (incr-pc))
             (((#x9 4) (X  4) (Y 4) (#x0 4))
              (print "Skip the next instruction if VX != VY")
              (incr-pc))
             (((#xA 4) (NNN  12))
              (set-I NNN)
              (print "I = " NNN)
              (incr-pc))
             (((#xB 4) (NNN  12))
              (print "Jump to " NNN)
              (incr-pc))
             (((#xC 4) (X 4) (NN  8))
              (set-register X
                            (bitwise-and NN
                                         (pseudo-random-integer 255)))
              (print "rand and " NN)
              (incr-pc))
             (((#xD 4) (X 4) (Y 4) (N 4))
              (print "DRAW " X " " Y " height: " N)
              (incr-pc))
             (((#xE 4) (X 4) (#x9 4) (#xE 4))
              (print "if key() == V" X)
              (incr-pc))
             (((#xE 4) (X 4) (#xA 4) (#x1 4))
              (print "if key() != V" X)
              (incr-pc))
             (((#xF 4) (X 4) (#x0 4) (#x7 4))
              (print "V" X " = get_delay")
              (incr-pc))
             (((#xF 4) (X 4) (#x0 4) (#xA 4))
              (print "V" X " = get_key")
              (incr-pc))
             (((#xF 4) (X 4) (#x1 4) (#x5 4))
              (print "delay_timer = V" X)
              (incr-pc))
             (((#xF 4) (X 4) (#x1 4) (#x8 4))
              (print "sound_timer = V" X)
              (incr-pc))
             (((#xF 4) (X 4) (#x1 4) (#xE 4))
              (set-I (+ (get-I)
                        (get-register X)))
              (print "I += V" X)
              (incr-pc))
             (((#xF 4) (X 4) (#x2 4) (#x9 4))
              (print "I = SpriteAddress V" X)
              (incr-pc))
             (((#xF 4) (X 4) (#x3 4) (#x3 4))
              (print "set_BCD V" X)
              (incr-pc))
             (((#xF 4) (X 4) (#x3 4) (#x3 4))
              (print "set_BCD V" X)
              (incr-pc))
             (((#xF 4) (X 4) (#x5 4) (#x5 4))
              (print "reg_dump V" X " " I)
              (incr-pc))
             (((#xF 4) (X 4) (#x6 4) (#x5 4))
              (print "reg_load V" X " " I)
              (incr-pc))
            (else
             (bitmatch instruction
                       (((a 4) (b 4) (c 4) (d 4))
                        (print "[NOT IMPLEMENTED]: " a " " b " " c " " d))))))
;;  The uppermost 256 bytes (0xF00-0xFFF) are reserved for display refresh,
;; and the 96 bytes below that (0xEA0-0xEFF) were reserved for the call stack, internal use, and other variables. 

(define (load-file file)
  (call-with-input-file file
    (lambda (port) (read-u8vector #f port))))

(define (init-cpu)
  (set! PC 0)
  (set-pseudo-random-seed! "djfadjfkjsdafkqpmvb")
  )

(define (emulate mem)
  (init-cpu)
  (define (iter)
    (cond
     ((>= PC (u8vector-length mem)) (print "END"))
     (else
      (emulate-si (fetch mem) mem)
      (iter)
      )))
  (iter))

(define (emulate-instruction mem)
  (cond
   ((>= PC (u8vector-length mem)) (print "END"))
   (else
    (emulate-si (fetch mem) mem)
    )))
