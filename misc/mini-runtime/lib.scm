;;;; lib.scm


(define (##sys#interrupt-hook reason state) #f)
(define (##sys#error-hook code loc . args) (##core#inline "C_halt" "error"))

(##core#inline "C_halt" "yo!")
