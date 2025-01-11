(load-relative "common.scm")

(load-relative "cpu.scm")

(load-relative "disassembler.scm")

(load-relative "emulator.scm")

(import (chicken io)
        (chicken format)
        (chicken port)
        (chicken load)
        srfi-4 srfi-18
        (chicken condition)
        miscmacros
        args)

(load-relative "common.scm")
(load-relative "cpu.scm")
(load-relative "disassembler.scm")
(load-relative "./emulator.scm")

(import (chicken process-context)
        (chip8 cpu)
        (chip8 emulator)
        (chip8 disassembler)) ;; for command line args

;(cond-expand
; (chicken-4 (use (prefix sdl2 "sdl2:")))
; (chicken-5 (import (prefix sdl2 "sdl2:"))))
(import (prefix sdl2 "sdl2:"))

;; for passing to wait-event
;; delay-fn must be a procedure which accepts a number of milliseconds to sleep
(define (thread-delay! ms) (thread-sleep! (* ms 0.001)))

(define WIN_HEIGHT 400)
(define WIN_WIDTH 600)

;; Initialize SDL

(sdl2:set-main-ready!)
;;(sdl2:init! '(everything)) ;; or whatever init flags your program needs
(sdl2:init! '(video events)) ;; or whatever init flags your program needs

(on-exit sdl2:quit!)

;; Call sdl2:quit! and then call the original exception handler if an
;; unhandled exception reaches the top level.
;(current-exception-handler
; (let ((original-handler (current-exception-handler)))
;   (lambda (exception)
;     (sdl2:quit!)
;     (original-handler exception))))

;; Install a custom exception handler that will call quit! and then
;; call the original exception handler. This ensures that quit! will
;; be called even if an unhandled exception reaches the top level.

(set! (sdl2:event-state 'text-editing) #f)
(set! (sdl2:event-state 'text-input) #f)
(set! (sdl2:event-state 'mouse-wheel) #f)
(set! (sdl2:event-state 'finger-down) #f)
(set! (sdl2:event-state 'finger-up) #f)
(set! (sdl2:event-state 'finger-motion) #f)
(set! (sdl2:event-state 'multi-gesture) #f)

(define pixel-width 6)
(define pixel-height 6)

(define (resize-event-handler win-width win-height)
  (set! pixel-width
    (round
      (/ win-width SCREEN_WIDTH)))
  (set! pixel-height
    (round
      (/ win-height SCREEN_HEIGHT)))
  (print "pixel-width: " pixel-width))

(define (draw-square win x y)
  (sdl2:fill-rect!
   (sdl2:window-surface win)
   (sdl2:make-rect (* pixel-width x) (* pixel-height y) pixel-width pixel-height)
   (sdl2:make-color 255 255 255)))

(define (draw-grid win mem h w)
  (do ((j 0 (add1 j)))
      ((>= j h))
    (do ((i 0 (add1 i)))
        ((>= i w))
      (let ((el (u8vector-ref (vector-ref mem j) i)))
        (if (= el 1)
            (draw-square win i j))))))

(define (draw-screen screen-memory win)
  (draw-grid win screen-memory SCREEN_HEIGHT SCREEN_WIDTH))

(define (update-screen win)
  (sdl2:fill-rect! (sdl2:window-surface win)
                   #f
                   (sdl2:make-color 0 0 0))
  (draw-screen (cpu-screen-memory main-cpu) win)
  (sdl2:update-window-surface! win))

;; MAIN

(define main-cpu (init-cpu))

(define (main-loop)
  (let ((event (sdl2:make-event))
        (window (sdl2:create-window! "Chip-8 emulator" 'centered 0 WIN_WIDTH WIN_HEIGHT '(shown resizable)))
        (done #f)
        (verbose? #f))
    (while (not done)
           (let ((ev (sdl2:wait-event-timeout! 16 event thread-delay!)))
             (when verbose?
               (print ev))
             (if (sdl2:quit-event? event)
                 (begin
                   (set! done #t)))
             (case (sdl2:event-type event)
               ((quit)
                (cpu-running-set! main-cpu #f)
                (set! done #t))
               ((key-up)
                (case (sdl2:keyboard-event-sym event)
                  ((a b c d e f)
                   (let ((key (sdl2:keyboard-event-sym-raw event)))
                     (key-up main-cpu (- key 87))
                     (print (- key 87))
                     ;;(thread-sleep! 0.016)
                     ))
                  ((n-0 n-1 n-2 n-3 n-4 n-5 n-6 n-7 n-8 n-9)
                   (let ((key (sdl2:keyboard-event-sym-raw event)))
                     (key-up main-cpu (- key 48))
                     (print (- key 48)))
                   ;;(thread-sleep! 0.016)
                   )))
               ((key-down)
                (case (sdl2:keyboard-event-sym event)
                  ((escape q)
                   (cpu-running-set! main-cpu #f)
                   (set! done #t))
                  ((r)
                   (pause main-cpu)
                   (reset main-cpu)
                   (load-program-into-memory "br8kout.ch8" main-cpu)
                   (unpause main-cpu))
                  ((v)
                   (print "verbose"))
                  ((a b c d e f)
                   (let ((key (sdl2:keyboard-event-sym-raw event)))
                     (key-down main-cpu (- key 87))
                     (print (- key 87))))
                  ((n-0 n-1 n-2 n-3 n-4 n-5 n-6 n-7 n-8 n-9)
                   (let ((key (sdl2:keyboard-event-sym-raw event)))
                     (key-down main-cpu (- key 48))
                     (print (- key 48))))))
               ((window)
                (case (sdl2:window-event-event event)
                  ((resized)
                   (resize-event-handler (sdl2:window-event-data1 event) (sdl2:window-event-data2 event)))
                  ))))
           (update-screen window)))
  (sdl2:quit!))

(define (emulator-timer)
  (define start_time (sdl2:get-ticks))
  (define septimems 19)

  (let ((done #f))
    (while (and (not done) (cpu-running main-cpu))
           (let ((t (sdl2:get-ticks)))
             (if (>= (- t start_time) septimems)
                 (begin
                   (set! start_time t)
                   (delay-timer main-cpu))))
           (thread-sleep! 0.001))))

(define (start-main-loop-in-thread!)
  (*main-loop-thread* (thread-start! main-loop)))

(define (start-emulator-in-thread!)
  (*emulation-thread* (thread-start! emulator)))

(define *main-loop-thread* (make-parameter #f))
(define *emulation-thread* (make-parameter #f))
(define *timer-thread* (make-parameter #f))

(define (emulator-main romfile)
  (begin
    (define (emulator)
      (emulate main-cpu))


    (print "Iinitializing memory..")
    (condition-case
      (load-program-into-memory romfile main-cpu)
      ((exn i/o file) (begin
                        (print "exn file: error file")
                        (exit 3)))
      ((exn i/o) (print "exn i/o: "))
      (var () (print-error-message var)))

    (print "Starting threads")
    ;; :( FIXME
    (*main-loop-thread* (thread-start! main-loop))
    (*emulation-thread* (thread-start! emulator))
    (*timer-thread* (thread-start! emulator-timer))

    (thread-join! (*main-loop-thread*))
    (print "main loop ended")
    (thread-join! (*emulation-thread*))
    (print "emulation ended")
    (thread-join! (*timer-thread*))
    (print "timer ended")))

(define cmdline-opts
  (list (args:make-option (i infile)
			  #:required "chip8 rom file")
	(args:make-option (h help)
			  #:none "display this text" (usage))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " -i infile")
      (newline)
      (print (args:usage cmdline-opts))))
  (exit 1))

(receive (options operands)
    (args:parse (command-line-arguments) cmdline-opts)
  (emulator-main (alist-ref 'infile options)))

