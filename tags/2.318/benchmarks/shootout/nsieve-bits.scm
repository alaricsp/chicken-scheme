;;; The Great Computer Language Shootout
;;; http://shootout.alioth.debian.org/
;;
;; Adapted from CMUCL code by Dima Dorfman; bit-vector stuff by Alex Shinn;
;; cobbled together by felix

(declare
  (uses format lolevel)
  (disable-interrupts)
  (fixnum)
  (block) )

(define (make-bit-vector size . o)
  (let* ((fill (if (and (pair? o) (car o)) #b11111111 0))
         (len (quotient (+ size 7) 8))
         (res (make-byte-vector len fill)))
    (if (zero? fill)
      res
      (let ((off (remainder size 8)))
        (if (not (zero? off))
          (byte-vector-set! res (- len 1) (- (arithmetic-shift 1 off) 1)))
        res))))

(define (bit-vector-ref vec i)
  (let ((byte (quotient i 8))
        (off (remainder i 8)))
    (and (< byte (byte-vector-length vec))
         (not (zero? (bitwise-and (byte-vector-ref vec byte)
                                  (arithmetic-shift 1 off)))))))

(define (bit-vector-set! vec i x)
  (let ((byte (quotient i 8))
        (off (remainder i 8))
        (len (byte-vector-length vec)))
    (cond
      ((< byte len)
       (byte-vector-set! vec byte
                      (if x
                        (bitwise-ior (byte-vector-ref vec byte)
                                     (arithmetic-shift 1 off))
                        (bitwise-and (byte-vector-ref vec byte)
                                     (bitwise-not (arithmetic-shift 1 off)))))
       vec)
      ((not x) vec)
      (else (bit-vector-set! (bit-vector-grow vec i) i x)))))

(define (nsievebits m)
  (let ((a (make-bit-vector m #t)))
    (define (clear i)
      (do ([j (+ i i) (+ j i)])
	  ((>= j m))
	(bit-vector-set! a j #f) ) )
    (let ([c 0])
      (do ([i 2 (add1 i)])
	  ((>= i m) c)
	(when (bit-vector-ref a i)
	  (clear i)
	  (set! c (add1 c)) ) ) ) ) )

(define (test n)
  (let ((m (* 10000 (arithmetic-shift 1 n))))
    (format #t "Primes up to ~8d~8d~%" m (nsievebits m))))

(define (main args)
  (let ([n (string->number (car args))])
    (when (>= n 0) (test n))
    (when (>= n 1) (test (- n 1)))
    (when (>= n 2) (test (- n 2)))))

(main (command-line-arguments))
