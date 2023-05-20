(import (chicken io)
        srfi-4)

(include "cpu.scm")

(define (load-file file)
  (call-with-input-file file
    (lambda (port) (read-u8vector #f port))))
