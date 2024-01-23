(import bitstring
        (chicken random)
        (chicken format) ;; printf...
        srfi-4  ;; u8vector
        srfi-151) ;; bitwise opeartions

(define (tohex n)
  (string-append "0x" (number->string n 16)))

(define (digit num position)
  (modulo (quotient num
                    (expt 10 position))
          10))
