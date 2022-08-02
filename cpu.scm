;;https://wiki.call-cc.org/eggref/5/bitstring

;;; http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
;; https://wiki.call-cc.org/man/5/Module%20srfi-4

(import bitstring srfi-4)

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
(define registers
  (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(define (set-register registers X val)
  (vector-set! registers X val))
(define (get-register registers X)
  (vector-ref registers X))

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
             (set-register registers X NN)
             (print "V" X " = " NN))
            (((#x7 4) (X 4) (NN 8))
             (set-register registers
                           X
                           (+ (get-register registers X) NN))
             (print "V" X " += " NN))
            (((#x8 4) (X 4) (Y 4) (\x0 4))
             (set-register registers
                           X
                           (get-register registers Y))
             (print "V" X " = V" Y))
            (else
             (print "Not implemented instruction: " instruction))))

(define (emulate mem)
  (cond
   ((>= PC (u8vector-length mem)) (print "END"))
   (else
    (emulate-si (fetch mem))
    (set! PC (+ PC 2))
    (emulate mem)
    )))
