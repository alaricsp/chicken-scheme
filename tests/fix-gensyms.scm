(define varnum 0)
(define vartable '())

(define (find-var var pre post varp)
    (let ((l   (assoc var vartable)))
        (if l
            (string-append pre (cdr l) post)
            (let ((n   (conc varp varnum)))
                (set! varnum (+ 1 varnum))
                (set! vartable (cons (cons var n) vartable))
                (string-append pre n post)))))

(define (get-var v)
    (cond ((= 0 (string-length v))
              v)
          ((char=? #\( (string-ref v 0)) 
              (if (and (<= 7 (string-length v) 8)
                       (string=? "f_" (substring v 1 3)))
                  (find-var (substring v 1 7) "(" (substring v 7) "f_")
                  v))
          ((char-lower-case? (string-ref v 0))
              (let loop1 ((i   0)
                          (l   (string-length v)))
                  (cond ((>= i l)
                            v)
                        ((char-lower-case? (string-ref v i))
                            (loop1 (+ 1 i) l))
                        ((char-numeric? (string-ref v i))
                            (let loop2 ((i   i)
                                        (n   i)
                                        (l   l))
                                (cond ((>= i l)
                                          (find-var v "" "" (substring v 0 n)))
                                      ((char-numeric? (string-ref v i))
                                          (loop2 (+ 1 i) n l))
                                      (else
                                          (find-var (substring v 0 i)
                                                    "" (substring v i)
                                                    (substring v 0 n))))))
                        (else
                            v))))
          (else
              v)))

(define (clean-gensyms filein fileout)
    (let ((i   (open-input-file filein))
          (o   (open-output-file fileout)))
        (let loop ((l   (read-line i)))
            (cond ((eof-object? l)
                      (close-input-port i)
                      (close-output-port o)
                      #t)
                  (else
                      (display
                          (string-intersperse
                              (map get-var (string-split l " " #t))
                              " ")
                          o)
                      (newline o)
                      (loop (read-line i)))))))

(define (main args)
    (clean-gensyms (car args) (cadr args))
    (exit 0))

(main (command-line-arguments))
