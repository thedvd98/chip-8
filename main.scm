(import (chicken io)
        (chicken format)
        srfi-4 srfi-12 srfi-18
        miscmacros)
(import (chicken process-context)) ;; for command line args

(include "emulator.scm")

(cond-expand
 (chicken-4 (use (prefix sdl2 "sdl2:")))
 (chicken-5 (import (prefix sdl2 "sdl2:"))))

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
(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (sdl2:quit!)
     (original-handler exception))))

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

(define-constant grid-size 6)

(define (draw-square win x y)
  (sdl2:fill-rect!
   (sdl2:window-surface win)
   (sdl2:make-rect (* grid-size x) (* grid-size y) grid-size grid-size)  ; Adjust the size (20x20) according to your requirements
   (sdl2:make-color 255 255 255)))  ; Adjust the color (RGB) as needed

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
  (define event (sdl2:make-event))

  (define window
    (sdl2:create-window! "Chip-8 emulator" 'centered 0 WIN_WIDTH WIN_HEIGHT '(shown resizable)))
  ;; (let ((n (command-line-arguments)))
  ;;   (print "arguments: " n)
  ;;   (if (> (length n) 0)
  ;;       (let ((filename (car n)))
  ;;         (load-program-into-memory filename main-cpu))
  ;;       (load-program-into-memory "6-keypad.ch8" main-cpu)))


  (let ((done #f)
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
                     (print (- key 48))))))))
           (update-screen window)
           ))
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

(define (emulator-main)
  (define (emulator)
    (emulate main-cpu))
  ;; (load-program-into-memory "6-keypad.ch8" main-cpu)
  ;; (load-program-into-memory "PONG" main-cpu)
  ;;(load-program-into-memory "4-flags.ch8" main-cpu)
  ;;(load-program-into-memory "3-corax+.ch8" main-cpu)
  (load-program-into-memory "5-quirks.ch8" main-cpu)
  ;;(load-program-into-memory "br8kout.ch8" main-cpu)
  ;;(load-program-into-memory "flightrunner.ch8" main-cpu)

  (*main-loop-thread* (thread-start! main-loop))
  (*emulation-thread* (thread-start! emulator))
  (*timer-thread* (thread-start! emulator-timer))

  (thread-join! (*main-loop-thread*))
  (print "main loop ended")
  (thread-join! (*emulation-thread*))
  (print "emulation ended")
  (thread-join! (*timer-thread*))
  (print "timer ended")
  )
