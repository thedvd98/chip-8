(module
  chip8.common
  *
  (import (chicken module))
  (reexport
    scheme
    (chicken base)
    (chicken random)
    (chicken format) ;; printf...
    bitstring
    srfi-4  ;; u8vector
    srfi-151)

  (define (tohex n)
    (string-append "0x" (number->string n 16)))

  (define (digit num position)
    (modulo (quotient num
                      (expt 10 position))
            10))
  )
