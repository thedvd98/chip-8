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

(define PC 0)
(define I 0)

(define registers
  (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

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

(define (fetch mem)
  (list->u8vector (list (u8vector-ref mem PC)
                        (u8vector-ref mem (+ PC 1)))))

(define (emulate-si instruction)
  (bitmatch instruction
            (((#x00EE 16))
             (print "return"))
            (((#x00E0 16))
             (print "clear screen"))
            (((#x0 4) (address 12))
             (print "call machine code at " address))
            (((#x1 4) (address 12))
             (print "Jump " address))
            (((#x2 4) (address 12))
             (print "Call " address))
            (((#x3 4) (X 4) (NN 8))
             (print "Skip next instruction if V" X " == " NN))
            (((#x4 4) (X 4) (NN 8))
             (print "Skip next instruction if V" X " != " NN))
            (((#x5 4) (X 4) (Y 4) (#x0))
             (print "Skip next instruction if V" X " == V" Y))
            (((#x6 4) (X 4) (NN 8))
             (set-register X NN)
             (print "V" X " = " NN))
            (((#x7 4) (X 4) (NN 8))
             (set-register X
                           (+ (get-register X) NN))
             (print "V" X " += " NN))
            (((#x8 4) (X 4) (Y 4) (#x0 4))
             (set-register X
                           (get-register Y))
             (print "V" X " = V" Y))
            (((#x8 4) (X 4) (Y 4) (#x1 4))
             (set-register X
                           (bitwise-ior (get-register X)
                                        (get-register Y)))
             (print "V" X " |= V" Y))
            (((#x8 4) (X 4) (Y 4) (#x2 4))
             (set-register X
                           (bitwise-and (get-register X)
                                        (get-register Y)))
             (print "V" X " &= V" Y))
            (((#x8 4) (X 4) (Y 4) (#x3 4))
             (set-register X
                           (bitwise-xor (get-register X)
                                        (get-register Y)))
             (print "V" X " ^= V" Y))
            (((#x8 4) (X 4) (Y 4) (#x4 4))
             (let ((sum (+ (get-register X)
                           (get-register Y))))
               (if (> sum 255)
                   (begin (set-register X
                                        (bitwise-and sum 255))
                          (set-carry-flag 1))
                   (begin (set-register X sum)
                          (set-carry-flag 0))))
             (print "V" X " += V" Y))
            (((#x8 4) (X  4) (Y 4) (#x5 4))
             (let ((subtraction (- (get-register X)
                                   (get-register Y))))
               (if (< subtraction 0)
                   (begin (set-register X
                                        (bitwise-and subtraction 255))
                          (set-carry-flag 0))
                   (begin (set-register X subtraction)
                          (set-carry-flag 1))))
             (print "V" X " -= V" Y))
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
              (print "V" X " -= V" Y))
             (((#x8 4) (X  4) (Y 4) (#xE 4))
              (let ((VX (get-register X)))
                (begin (set-carry-flag (bit-field VX 7 8))
                       (set-register X (bit-field VX 0 7)))
                (print "V" X " <<= 1")))
             (((#x9 4) (X  4) (Y 4) (#x0 4))
              (print "Skip the next instruction if VX != VY"))
             (((#xA 4) (NNN  12))
              (set-I NNN)
              (print "I = " NNN))
             (((#xB 4) (NNN  12))
              (print "Jump to " NNN))
             (((#xC 4) (X 4) (NN  8))
              (set-register X
                            (bitwise-and NN
                                         (pseudo-random-integer 255)))
              (print "rand and " NN))
             (((#xD 4) (X 4) (Y 4) (N 4))
              (print "DRAW " X " " Y " height: " N))
             (((#xE 4) (X 4) (#x9 4) (#xE 4))
              (print "if key() == V" X))
             (((#xE 4) (X 4) (#xA 4) (#x1 4))
              (print "if key() != V" X))
             (((#xF 4) (X 4) (#x0 4) (#x7 4))
              (print "V" X " = get_delay"))
             (((#xF 4) (X 4) (#x0 4) (#xA 4))
              (print "V" X " = get_key"))
             (((#xF 4) (X 4) (#x1 4) (#x5 4))
              (print "delay_timer = V" X))
             (((#xF 4) (X 4) (#x1 4) (#x8 4))
              (print "sound_timer = V" X))
             (((#xF 4) (X 4) (#x1 4) (#xE 4))
              (set-I (+ (get-I)
                        (get-register X)))
              (print "I += V" X))
             (((#xF 4) (X 4) (#x2 4) (#x9 4))
              (print "I = SpriteAddress V" X))
             (((#xF 4) (X 4) (#x3 4) (#x3 4))
              (print "set_BCD V" X))
             (((#xF 4) (X 4) (#x3 4) (#x3 4))
              (print "set_BCD V" X))
             (((#xF 4) (X 4) (#x5 4) (#x5 4))
              (print "reg_dump V" X " " I))
             (((#xF 4) (X 4) (#x6 4) (#x5 4))
              (print "reg_load V" X " " I))
            (else
             (bitmatch instruction
                       (((a 4) (b 4) (c 4) (d 4))
                        (print "[NOT IMPLEMENTED]: " a " " b " " c " " d))))))

(define (emulate mem)
  (set! PC 0)
  (set-pseudo-random-seed! "djfadjfkjsdafkqpmvb")
  (define (iter)
    (cond
    ((>= PC (u8vector-length mem)) (print "END"))
    (else
     (emulate-si (fetch mem))
     (set! PC (+ PC 2))
     (iter)
     )))
  (iter))
