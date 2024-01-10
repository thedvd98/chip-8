(import (chicken io)
        (chicken format)
        srfi-4 srfi-12 srfi-18
        miscmacros)

(include "cpu.scm")

(cond-expand
  (chicken-4 (use (prefix sdl2 "sdl2:")))
  (chicken-5 (import (prefix sdl2 "sdl2:"))))

;; for passing to wait-event
;; delay-fn must be a procedure which accepts a number of milliseconds to sleep
(define (thread-delay! ms) (thread-sleep! (* ms 0.001)))

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

(define WIN_HEIGHT 400)
(define WIN_WIDTH 600)


(define window
  (sdl2:create-window! "Hello, World!" 'centered 0 WIN_WIDTH WIN_HEIGHT '(shown resizable)))

(printf "Window position: ~A, size: ~A, max size: ~A, min size: ~A~N"
        (receive (sdl2:window-position window))
        (receive (sdl2:window-size window))
        (receive (sdl2:window-maximum-size window))
        (receive (sdl2:window-minimum-size window)))
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

(define (draw-square x y)
  (sdl2:fill-rect!
   (sdl2:window-surface window)
   (sdl2:make-rect (* grid-size x) (* grid-size y) grid-size grid-size)  ; Adjust the size (20x20) according to your requirements
   (sdl2:make-color 255 255 255)))  ; Adjust the color (RGB) as needed

(define (draw-grid mem h w)
  (do ((j 0 (add1 j)))
      ((>= j h))
    (do ((i 0 (add1 i)))
        ((>= i w))
      (let ((el (u8vector-ref (vector-ref mem j) i)))
        (if (= el 1)
            (draw-square i j))))))
(define (draw-screen)
  (draw-grid screen-memory SCREEN_HEIGHT SCREEN_WIDTH))

(define (update-screen)
  (sdl2:fill-rect! (sdl2:window-surface window)
                   #f
                   (sdl2:make-color 0 0 0))
  (draw-screen)
  (sdl2:update-window-surface! window))

;; MAIN

(define (main-loop)
  (define event (sdl2:make-event))

  ;; Schedule quit! to be automatically called when your program exits normally.
    (let ((done #f)
        (verbose? #f))
    (while (not done)
      (update-screen)
      (thread-sleep! 0.25)
      (let ((ev (sdl2:wait-event! event thread-delay!)))
        (when verbose?
          (print ev))
        (if (sdl2:quit-event? ev)
            (sdl2:quit!))
        (case (sdl2:event-type ev)
          ((window)
           '())
          ((quit)
           (set! done #t))
          ((key-down)
           (case (sdl2:keyboard-event-sym ev)
             ((escape q)
              (set! done #t))
             ((v)
              (print "verbose"))))))))
  (sdl2:quit!))

(define (start-main-loop-in-thread!)
  (*main-loop-thread* (thread-start! main-loop)))

(define *main-loop-thread* (make-parameter #f))

