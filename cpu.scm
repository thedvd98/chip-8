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
(define-constant TIMER_START 60)

(define-record cpu
  memory screen-memory PC SP I registers keys delay-timer sound-timer running pause)

(define (mem-to-string mem start end)
  (define (iter start end)
    (if (< start end)
        (begin (printf "~s: ~s\n"
                 (tohex start)
                 (tohex (u8vector-ref mem start)))
               (iter (+ start 1) end))
        ))
  (iter start end))

(define-record-printer (cpu c out)
  (fprintf out "#,(cpu PC=~s SP=~s I=~s registers=~s)"
           (tohex (cpu-PC c))
           (tohex (cpu-SP c))
           (tohex (cpu-I c))
           (cpu-registers c)))
(define (create-screen-memory)
  (define (loop i li)
    (if (< i SCREEN_HEIGHT)
        (loop (+ i 1) (cons (make-u8vector SCREEN_WIDTH 0) li))
        li
        ))
  (loop 0 '()))
(define (load-fontset-to-memory *CPU*)
  (define (iter n max)
    (cond ((>= n max) '())
          (else
           (u8vector-set! (cpu-memory *CPU*) n (u8vector-ref fontset n))
           (iter (+ n 1) max))
          ))
  (iter 0 (u8vector-length fontset)))

(define (init-cpu)
  (let ((memory (make-u8vector 4096 0))
        (screen-memory (list->vector (create-screen-memory)))
        (registers (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
        (pc PROGRAM_MEMORY_START)
        (i 0)
        (sp #xEA0)
        (keys (vector #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))
        (delay_timer 0)
        (sound_timer 0)
        (running #t)
        (pause #f))
    (set-pseudo-random-seed! "djfadjfkjsdafkqpmvb")
    (define *CPU*
      (make-cpu memory screen-memory pc sp i registers keys delay_timer sound_timer running pause))
    (load-fontset-to-memory *CPU*)
    *CPU*
    ))

(define (reset c)
  (cpu-memory-set! c (make-u8vector 4096 0))
  (cpu-screen-memory-set! c (list->vector (create-screen-memory)))
  (vector-fill! (cpu-registers c) 0)
  (cpu-PC-set! c PROGRAM_MEMORY_START)
  (cpu-I-set! c 0)
  (cpu-SP-set! c #xEA0)
  (vector-fill! (cpu-keys c) #f)
  (cpu-delay-timer-set! c 0)
  (cpu-sound-timer-set! c 0)
  (load-fontset-to-memory c)
  (cpu-running-set! c #t)
  (cpu-pause-set! c #f))

(define (pause c)
  (cpu-pause-set! c #t))
(define (unpause c)
  (cpu-pause-set! c #f))

(define (vector-index vec value)
  (define (loop i)
    (cond
     ((>= i (vector-length vec)) #f)
     ((eq? (vector-ref vec i) value) i)
     (else
      (loop (+ i 1)))))
  (loop 0))

(define (key-pressed *CPU* X)
  (if (vector-ref (cpu-keys *CPU*) X)
      #t
      #f))

(define (key-down *CPU* k)
  (vector-set! (cpu-keys *CPU*) k #t))
(define (key-up *CPU* k)
  (vector-set! (cpu-keys *CPU*) k #f))

(define (set-pc *CPU* newpc)
  (cpu-PC-set! *CPU* newpc))
(define (get-pc *CPU*)
  (cpu-PC *CPU*))

(define (set-register *CPU* X val)
  (vector-set! (cpu-registers *CPU*) X val))
(define (get-register *CPU* X)
  (vector-ref (cpu-registers *CPU*) X))

(define (set-carry-flag *CPU* val)
  (set-register *CPU* 15 val))
(define (get-carry-flag *CPU* val)
  (get-register *CPU* 15 val))

(define (set-memory *CPU* pos value)
  (u8vector-set! (cpu-memory *CPU*) pos value))
(define (get-memory *CPU* pos)
  (u8vector-ref (cpu-memory *CPU*) pos))

(define (fetch *CPU*)
  (list->u8vector (list (u8vector-ref (cpu-memory *CPU*) (cpu-PC *CPU*))
                        (u8vector-ref (cpu-memory *CPU*) (+ (cpu-PC *CPU*) 1)))))
(define (incr-pc *CPU*)
  (set-pc *CPU* (+ (get-pc *CPU*) 2)))

(define (jump-to *CPU* NNN)
  (set-pc *CPU* NNN))

(define (delay-timer c)
  (let ((dt (cpu-delay-timer c)))
    (if (<= dt 0)
        '()
        (cpu-delay-timer-set! c (- dt 1)))))

;; stack only used to store return addresses of 12bit
(define (push *CPU* addr)
  ;; TESTME
  (define (inc-sp *CPU*)
    (cpu-SP-set! *CPU* (+ (cpu-SP *CPU*) 1)))
  (let ((msb (bitwise-and (arithmetic-shift addr -8) #x0F)) ; 4 bit
        (lsb (bitwise-and addr #xFF)))  ;; 8 bit
    (u8vector-set! (cpu-memory *CPU*) (cpu-SP *CPU*) lsb)
    (u8vector-set! (cpu-memory *CPU*) (+ (cpu-SP *CPU*) 1) msb))
  (inc-sp *CPU*)
  (inc-sp *CPU*))

(define (pop *CPU*)
  (define (dec-sp *CPU*)
    (cpu-SP-set! *CPU* (- (cpu-SP *CPU*) 1)))
  (let ((msb (arithmetic-shift
              (u8vector-ref (cpu-memory *CPU*) (- (cpu-SP *CPU*) 1))
              8))
        (lsb (u8vector-ref (cpu-memory *CPU*) (- (cpu-SP *CPU*) 2))))
    (dec-sp *CPU*)
    (dec-sp *CPU*)
    (bitwise-ior msb lsb)))

(define (reg-dump *CPU* X)
  (define (iter start_addr reg_n X)
    (cond ((> reg_n X) '())
          (else (set-memory *CPU* start_addr (get-register *CPU* reg_n))
                (iter (+ start_addr 1) (+ reg_n 1) X))))
  (iter (cpu-I *CPU*) 0 X))

(define (reg-load *CPU* X)
  (define (iter start_addr reg_n X)
    (cond ((> reg_n X) '())
          (else (set-register *CPU* reg_n (get-memory *CPU* start_addr))
                (iter (+ start_addr 1) (+ reg_n 1) X))))
  (iter (cpu-I *CPU*) 0 X))

(define (get-screen-pixel *CPU* x y)
  (u8vector-ref (vector-ref (cpu-screen-memory *CPU*) y) x))

(define (set-screen-pixel *CPU* x y value)
  (let ((bit (if value 1 0))
        (x (modulo x SCREEN_WIDTH))
        (y (modulo y SCREEN_HEIGHT)))
    (if (or (>= x SCREEN_WIDTH) (>= y SCREEN_HEIGHT) (< x 0) (< y 0))
        (printf "OUT of screen x:~s, y:~s\n" x y)
        (let ((got-screen-pixel (get-screen-pixel *CPU* x y)))
          (if (= (and bit got-screen-pixel) 1)
              (begin
                ;;(printf "collision x:~s, y:~s\n\n" x y)
                (set-carry-flag *CPU* 1)))
          (u8vector-set!
           (vector-ref (cpu-screen-memory *CPU*) y)
           x
           (bitwise-xor bit got-screen-pixel))))))

(define (clear-screen *CPU*)
  (let ((ext-len (vector-length (cpu-screen-memory *CPU*)))
        (int-len (u8vector-length (vector-ref (cpu-screen-memory *CPU*) 0))))
    (do ((i 0 (+ i 1)))
        ((= i ext-len) '())
      (do ((j 0 (+ j 1)))
          ((= j int-len) '())
        (u8vector-set! (vector-ref (cpu-screen-memory *CPU*) i) j 0)))))

;; TODO FIXME Err2 CLIPPING sprite wrap
;; n = altezza sprite
(define (draw-sprite *CPU* vx vy n)
  ;;(printf "DRAW-SPRITE => vx: ~s, vy: ~s, n: ~s\n" vx vy n)
  (set-carry-flag *CPU* 0)
  (do ((b 0 (add1 b)))
      ((>= (+ b vy) (+ vy n)) #t)
    (let ((bits (bits->vector (get-memory *CPU* (+ b (cpu-I *CPU*))) 8)))
      (do ((a 0 (add1 a)))
          ((>= (+ a vx) (+ vx SPRITE_LENGTH)) #t)
        (let ((x (+ a vx))
              (y (+ b vy)))
          (set-screen-pixel *CPU* x y (vector-ref bits (- 7 a))))))))

(define (font-location vx)
  (let ((font-length 5))
    (+ 5 (* font-length vx))))

(define (digit num position)
  (modulo (quotient num
                    (expt 10 position))
          10))


;; EMULATOR
(define (emulate-si instruction *CPU*)
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
                      (set-carry-flag *CPU* (bit-field VX 7 8)))
               (print "V" X " <<= 1"))
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
             (print "WAITNG FOR KEY PRESS")
             (let ((k (vector-index (cpu-keys *CPU*) #t)))
               (if k
                   (begin
                     (print "KEY PRESSED V" X " = get_key => " k)
                     (set-register *CPU* X k)
                     (incr-pc *CPU*))
                   '()
                   )
               ))
            (((#xF 4) (X 4) (#x1 4) (#x5 4))
             (print "delay_timer = V" X)
             (cpu-delay-timer-set! *CPU* (get-register *CPU* X))
             (incr-pc *CPU*))
            (((#xF 4) (X 4) (#x1 4) (#x8 4))
             ;; TODO implement sound_timer
             (print "sound_timer = V" X)
             (cpu-sound-timer-set! *CPU* (get-register *CPU* X))
             (incr-pc *CPU*))
            (((#xF 4) (X 4) (#x1 4) (#xE 4))
             (cpu-I-set! *CPU* (+ (cpu-I *CPU*)
                       (get-register *CPU* X)))
             ;(print "I += V" X)
             (incr-pc *CPU*))
            (((#xF 4) (X 4) (#x2 4) (#x9 4))
             (let ((sprite_addr (font-location (get-register *CPU* X))))
               (print "I = SpriteAddress VX " (get-register *CPU* X) " -> addr: 0x" (tohex sprite_addr)) ;; TODO come funzionano gli sprite
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
             (print "set_BCD V" X)
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

(define (load-program-into-memory file *CPU*)
  (call-with-input-file file
    (lambda (port) (read-u8vector! #f (cpu-memory *CPU*) port PROGRAM_MEMORY_START))))

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

