                                        ;https://wikiMcall-cc.org/eggref/5/bitstring
;;; http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
;; https://wiki.call-cc.org/man/5/Module%20srfi-4

(import bitstring
        (chicken random)
        (chicken format) ;; fprintf magari lo uso dopo
        srfi-4  ;; u8vector
        srfi-151) ;; bitwise opeartions

(define (tohex n)
  (string-append "0x" (number->string n 16)))


(define fontset
  #u8(#xf0 #x90 #x90 #x90 #xf0          ; 0
           #x20 #x60 #x20 #x20 #x70     ; 1
           #xf0 #x10 #xf0 #x80 #xf0     ; 2
           #xf0 #x10 #xf0 #x10 #xf0     ; 3
           #x90 #x90 #xf0 #x10 #x10     ; 4
           #xf0 #x80 #xf0 #x10 #xf0     ; 5
           #xf0 #x80 #xf0 #x90 #xf0     ; 6
           #xf0 #x10 #x20 #x40 #x40     ; 7
           #xf0 #x90 #xf0 #x90 #xf0     ; 8
           #xf0 #x90 #xf0 #x10 #xf0     ; 9
           #xf0 #x90 #xf0 #x90 #x90     ; A
           #xe0 #x90 #xe0 #x90 #xe0     ; B
           #xf0 #x80 #x80 #x80 #xf0     ; C
           #xe0 #x90 #x90 #x90 #xe0     ; D
           #xf0 #x80 #xf0 #x80 #xf0     ; E
           #xf0 #x80 #xf0 #x80 #x80)) ; F

(define-constant PROGRAM_MEMORY_START 512)
(define-constant SCREEN_HEIGHT 32)
(define-constant SCREEN_WIDTH 64)
(define-constant PIXEL_ON 1)
(define-constant PIXEL_OFF 0)
(define-constant SPRITE_LENGTH 8) ;; in pixel

(define-record cpu
  memory screen-memory PC SP I registers key delay_timer sound_timer)

(define-record-printer (cpu c out)
  (fprintf out "#,(cpu PC=~s SP=~s I=~s registers=~s)"
           (tohex (cpu-PC c))
           (tohex (cpu-SP c))
           (tohex (cpu-I c))
           (cpu-registers c)))

(define (init-cpu)
  (define (load-fontset-to-memory cpu)
    (define (iter n max)
      (cond ((>= n max) '())
            (else
             (u8vector-set! (cpu-memory cpu) n (u8vector-ref fontset n))
             (iter (+ n 1) max))
            ))
    (iter 0 (u8vector-length fontset)))
  (define (create-screen-memory)
    (define (loop i li)
      (if (< i SCREEN_HEIGHT)
          (loop (+ i 1) (cons (make-u8vector SCREEN_WIDTH 0) li))
          li
          ))
    (loop 0 '()))
  (let ((memory (make-u8vector 4096 0))
        (screen-memory (list->vector (create-screen-memory)))
        (registers (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
        (pc PROGRAM_MEMORY_START)
        (i 0)
        (sp #xEA0)
        (key 0)
        (delay_timer 0)
        (sound_timer 0))
    (set-pseudo-random-seed! "djfadjfkjsdafkqpmvb")
    (define *CPU*
      (make-cpu memory screen-memory pc sp i registers key delay_timer sound_timer))
    (load-fontset-to-memory *CPU*)
    *CPU*
    ))


(define (set-pc cpu newpc)
  (cpu-PC-set! cpu newpc))
(define (get-pc cpu)
  (cpu-PC cpu))

(define (set-register cpu X val)
  (vector-set! (cpu-registers cpu) X val))
(define (get-register cpu X)
  (vector-ref (cpu-registers cpu) X))

(define (set-carry-flag cpu val)
  (set-register cpu 15 val))
(define (get-carry-flag cpu val)
  (get-register cpu 15 val))

(define (dec-sp)
  (set! SP (- SP 1)))

(define (set-memory cpu pos value)
  (u8vector-set! (cpu-memory cpu) pos value))
(define (get-memory cpu pos)
  (u8vector-ref (cpu-memory cpu) pos))

(define (fetch cpu)
  (list->u8vector (list (u8vector-ref (cpu-memory cpu) (cpu-PC cpu))
                        (u8vector-ref (cpu-memory cpu) (+ (cpu-PC) 1)))))
(define (incr-pc cpu)
  (set-pc cpu (+ (get-pc cpu) 2)))

(define (skip-next-inst)
  (incr-pc cpu)
  (incr-pc cpu))

(define (jump-to NNN)
  (set-pc cpu NNN))


;; stack only used to store return addresses of 12bit
(define (push cpu addr)
  ;; TESTME
  (define (inc-sp cpu)
    (cpu-SP-set! cpu (+ (cpu-SP) 1)))
  (print "push MEM addr:" addr)
  (let ((msb (bitwise-and (arithmetic-shift addr -8) #x0F))
        (lsb (bitwise-and addr #xFF)))
    (print (cpu-SP cpu))
    (u8vector-set! (cpu-memory cpu) (cpu-SP cpu) lsb)       ;; 8bit
    (u8vector-set! (cpu-memory cpu) (+ (cpu-SP cpu) 1) msb) ;; 4bit
)
  (inc-sp))

(define (pop cpu)
  (define (dec-sp cpu)
    (cpu-SP-set! cpu (- (cpu-SP) 1)))
  (let ((msb (arithmetic-shift
              (u8vector-ref (cpu-memory cpu) (- (cpu-SP cpu) 0))
              8))
        (lsb (u8vector-ref (cpu-memory cpu) (- (cpu-SP cpu) 1))))
    (dec-sp)
    (bitwise-ior msb lsb)))

(define (reg-dump cpu X)
  (define (iter start_addr reg_n X)
    (cond ((> reg_n X) '())
          (else (set-memory cpu start_addr (get-register cpu reg_n))
                (iter (+ start_addr 1) (+ reg_n 1) X))))
  (iter (cpu-I cpu) 0 X))

(define (reg-load cpu X)
  (define (iter start_addr reg_n X)
    (cond ((> reg_n X) '())
          (else (set-register cpu reg_n (get-memory (cpu-memory cpu) start_addr))
                (iter (+ start_addr 1) (+ reg_n 1) X))))
  (iter (cpu-I cpu) 0 X))

(define (get-screen-pixel cpu x y)
  (u8vector-ref (vector-ref (cpu-screen-memory cpu) y) x))

(define (set-screen-pixel cpu x y value)
  (let ((bit (if value
                 1
                 0)))
    (if (or (>= x SCREEN_WIDTH) (>= y SCREEN_HEIGHT) (< x 0) (< y 0))
        #f
        (u8vector-set!
         (vector-ref (cpu-screen-memory cpu) y) x (bitwise-xor bit (get-screen-pixel cpu x y))))))

(define (clear-screen cpu)
  (let ((ext-len (vector-length (cpu-screen-memory cpu)))
        (int-len (u8vector-length (vector-ref (cpu-screen-memory cpu) 0))))
    (do ((i 0 (+ i 1)))
        ((= i ext-len) '())
      (do ((j 0 (+ j 1)))
          ((= j int-len) '())
        (u8vector-set! (vector-ref (cpu-screen-memory cpu) i) j 0)))))

(define (digit num position)
  (modulo (quotient num
                    (expt 10 position))
          10))

;; n = altezza sprite
(define (draw-sprite cpu vx vy n)
  (do ((b 0 (add1 b)))
      ((>= (+ b vy) (+ vy n)) #t)
    (let ((bits (bits->vector (get-memory cpu (+ b (cpu-I cpu))) 8)))
      (do ((a 0 (add1 a)))
          ((>= (+ a vx) (+ vx SPRITE_LENGTH)) #t)
        (set-screen-pixel cpu (+ a vx) (+ b vy) (vector-ref bits (- 7 a)))))))

(define (font-location vx)
  (let ((font-length 5))
    (+ 5 (* font-length vx))))

(define (emulate-si instruction cpu)
  (bitmatch instruction
            (((#x00EE 16))
             (let ((addr (pop cpu)))
               (print "POP addr: 0x" (tohex addr))
               (set-pc cpu addr)
               (incr-pc cpu))
             )
            (((#x00E0 16))
             (clear-screen cpu)
             (incr-pc cpu))
            (((#x0 4) (address 12)) ;; call machine code at address
             (push cpu (get-pc cpu))
             (set-pc cpu address))
            (((#x1 4) (address 12)) ;; jump at address
             (print "jump to 0x" (tohex address))
             (set-pc cpu address))
            (((#x2 4) (address 12)) ;; Call subroutine
             (push cpu (get-pc cpu))
             (set-pc cpu address))
            (((#x3 4) (X 4) (NN 8)) ;; Skip next instruction if VX == NN
             (if (= (get-register cpu X) NN)
                 (incr-pc cpu)
                 '())
             (incr-pc cpu))
            (((#x4 4) (X 4) (NN 8))
             (if (not (= (get-register cpu X) NN))
                 (incr-pc cpu)
                 '())
             (print "Skip next instruction if V" X " != " NN)
             (incr-pc cpu))
            (((#x5 4) (X 4) (Y 4) (#x0 4))
             (print "Skip next instruction if V" X " == V" Y)
             (if (= (get-register cpu X) (get-register Y))
                 (incr-pc cpu)
                 '())
             (incr-pc cpu))
            (((#x6 4) (X 4) (NN 8))
             (set-register cpu X NN)
             (print "V" X " = " NN)
             (incr-pc cpu))
            (((#x7 4) (X 4) (NN 8))
             (set-register cpu X
                           (bitwise-and (+ (get-register cpu X) NN)
                                        #xff))
             (print "V" X " += " NN " => " (get-register cpu X))
             (incr-pc cpu))
            (((#x8 4) (X 4) (Y 4) (#x0 4))
             (set-register cpu X
                           (get-register cpu Y))
             (print "V" X " = V" Y)
             (incr-pc cpu))
            (((#x8 4) (X 4) (Y 4) (#x1 4))
             (set-register cpu X
                           (bitwise-ior (get-register cpu X)
                                        (get-register cpu Y)))
             (print "V" X " |= V" Y)
             (incr-pc cpu))
            (((#x8 4) (X 4) (Y 4) (#x2 4))
             (set-register cpu X
                           (bitwise-and (get-register cpu X)
                                        (get-register cpu Y)))
             (print "V" X " &= V" Y)
             (incr-pc cpu))
            (((#x8 4) (X 4) (Y 4) (#x3 4))
             (set-register cpu X
                           (bitwise-xor (get-register cpu X)
                                        (get-register cpu Y)))
             (print "V" X " ^= V" Y)
             (incr-pc cpu))
            (((#x8 4) (X 4) (Y 4) (#x4 4))
             (let ((sum (+ (get-register cpu X)
                           (get-register cpu Y))))
               (if (> sum 255)
                   (begin (set-register cpu X
                                        (bitwise-and sum 255))
                          (set-carry-flag cpu 1))
                   (begin (set-register cpu X sum)
                          (set-carry-flag cpu 0))))
             (print "V" X " += V" Y)
             (incr-pc cpu))
            (((#x8 4) (X  4) (Y 4) (#x5 4))
             (let ((subtraction (- (get-register cpu X)
                                   (get-register cpu Y))))
               (if (< subtraction 0)
                   (begin (set-register cpu X
                                        (bitwise-and subtraction 255))
                          (set-carry-flag cpu 0))
                   (begin (set-register cpu X subtraction)
                          (set-carry-flag cpu 1))))
             (print "V" X " -= V" Y)
             (incr-pc cpu))
            (((#x8 4) (X  4) (Y 4) (#x6 4))
             (let ((VX (get-register cpu X)))
               (begin (set-carry-flag cpu (bitwise-and
                                       VX 1))
                      (set-register cpu X (arithmetic-shift VX -1)))
               (print "V" X " >>= 1"))
             (incr-pc cpu))
            (((#x8 4) (X  4) (Y 4) (#x7 4))
             (let ((subtraction (- (get-register cpu Y)
                                   (get-register cpu X))))
               (if (< subtraction 0)
                   (begin (set-register cpu X
                                        (bitwise-and subtraction 255))
                          (set-carry-flag cpu 0))
                   (begin (set-register cpu X subtraction)
                          (set-carry-flag cpu 1))))
             (print "V" X " -= V" Y)
             (incr-pc cpu))
            (((#x8 4) (X  4) (Y 4) (#xE 4))
             (let ((VX (get-register cpu X)))
               (begin (set-carry-flag cpu (bit-field VX 7 8))
                      (set-register cpu X (bitwise-and (arithmetic-shift VX 1) #xff)))
               (print "V" X " <<= 1"))
             (incr-pc cpu))
            (((#x9 4) (X  4) (Y 4) (#x0 4))
             (if (not (= (get-register cpu X) (get-register Y)))
                 (incr-pc cpu)
                 '())
             (print "Skip the next instruction if VX != VY")
             (incr-pc cpu))
            (((#xA 4) (NNN  12))
             (print "I = 0x" (tohex NNN))
             (cpu-I-set! cpu NNN)
             (incr-pc cpu))
            (((#xB 4) (NNN  12))
             (jump-to (+ (get-register cpu 0) NNN)))
            (((#xC 4) (X 4) (NN  8))
             (set-register cpu X
                           (bitwise-and NN
                                        (pseudo-random-integer 255)))
             (print "rand and " NN)
             (incr-pc cpu))
            (((#xD 4) (X 4) (Y 4) (N 4))
             (draw-sprite cpu (get-register cpu X) (get-register Y) N)
             (print "DRAW X: " (get-register cpu X) ", Y: " (get-register Y) " height: " N)
             (incr-pc cpu))
            (((#xE 4) (X 4) (#x9 4) (#xE 4))
             (if (= (get-key) (get-register cpu X)) ;; TODO test key()
                 (incr-pc cpu)
                 '())
             (print "skip if key() == V" X)
             (incr-pc cpu))
            (((#xE 4) (X 4) (#xA 4) (#x1 4))
             (if (not (= (get-key) (get-register cpu X))) ;; TODO test key()
                 (incr-pc cpu)
                 '())
             (print "if key() != V" X)
             (incr-pc cpu))
            (((#xF 4) (X 4) (#x0 4) (#x7 4))
             (set-register cpu X 999) ;; TODO implement get_delay
             (print "V" X " = get_delay")
             (incr-pc cpu))
            (((#xF 4) (X 4) (#x0 4) (#xA 4))
             (set-register cpu X (get-key)) ;; TODO test get_key
             (print "V" X " = get_key")
             (incr-pc cpu))
            (((#xF 4) (X 4) (#x1 4) (#x5 4))
             ;; TODO implement delay_timer
             (print "delay_timer = V" X)
             (incr-pc cpu))
            (((#xF 4) (X 4) (#x1 4) (#x8 4))
             ;; TODO implement sound_timer
             (print "sound_timer = V" X)
             (incr-pc cpu))
            (((#xF 4) (X 4) (#x1 4) (#xE 4))
             (cpu-I-set! cpu (+ (cpu-I cpu)
                       (get-register cpu X)))
             (print "I += V" X)
             (incr-pc cpu))
            (((#xF 4) (X 4) (#x2 4) (#x9 4))
             (let ((sprite_addr (font-location (get-register cpu X))))
               (print "I = SpriteAddress VX " (get-register cpu X) " -> addr: 0x" (tohex sprite_addr)) ;; TODO come funzionano gli sprite
               (cpu-I-set! cpu sprite_addr))
             (incr-pc cpu))
            (((#xF 4) (X 4) (#x3 4) (#x3 4))
             (let ((num (get-register cpu X)))
               (let ((n1 (digit num 2))
                     (n2 (digit num 1))
                     (n3 (digit num 0)))
                 (set-memory cpu (cpu-I cpu) n1)
                 (set-memory cpu (+ (cpu-I cpu) 1) n2)
                 (set-memory cpu (+ (cpu-I cpu) 2) n3)))
             (print "set_BCD V" X)
             (incr-pc cpu))
            (((#xF 4) (X 4) (#x5 4) (#x5 4))
             (print "reg_dump V" X " " I)
             (reg-dump cpu X)
             (incr-pc cpu))
            (((#xF 4) (X 4) (#x6 4) (#x5 4))
             (print "reg_load V" X " " I)
             (reg-load cpu X)
             (incr-pc cpu))
            (else
             (bitmatch instruction
                       (((a 4) (b 4) (c 4) (d 4))
                        (print "[NOT IMPLEMENTED]: " a " " b " " c " " d)
                        (exit)
                        )))))
;;  The uppermost 256 bytes (0xF00-0xFFF) are reserved for display refresh,
;; and the 96 bytes below that (0xEA0-0xEFF) were reserved for the call stack, internal use, and other variables. 

(define (load-file file)
  (call-with-input-file file
    (lambda (port) (read-u8vector #f port))))

(define (load-program-into-memory file cpu)
  (call-with-input-file file
    (lambda (port) (read-u8vector! #f (cpu-memory cpu) port PROGRAM_MEMORY_START))))

(define (emulate cpu)
  (define (iter)
    (cond
     ((>= PC (u8vector-length (cpu-memory cpu))) (print "END"))
     (else
      (emulate-si (fetch (cpu-memory cpu)) (cpu-memory cpu))
      (iter)
      )))
  (iter))

(define (emulate-instruction cpu)
  (cond
   ((>= PC (u8vector-length (cpu-memory cpu))) (print "END"))
   (else
    (let ((instr (fetch (cpu-memory cpu))))
      (emulate-si instr (cpu-memory cpu)))
    )))

(define (pong cpu)
  (load-program-into-memory "PONG" (cpu-memory cpu))
  (emulate (cpu-memory cpu)))

(define (test-keypad cpu)
  (load-program-into-memory "6-keypad.ch8" (cpu-memory cpu))
  (emulate (cpu-memory cpu)))

(define (test cpu)
  (load-program-into-memory "1-chip8-logo.ch8" (cpu-memory cpu))
  (emulate (cpu-memory cpu)))

(define (test-1 cpu)
  (load-program-into-memory "test_opcode.ch8" (cpu-memory cpu))
  (emulate (cpu-memory cpu)))

(define (test-flags cpu)
  (load-program-into-memory "4-flags.ch8" (cpu-memory cpu))
  (emulate (cpu-memory cpu)))

