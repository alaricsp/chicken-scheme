;;;: test-irregex.scm


(use extras regex)

(include "test.scm")

(import irregex)

(define (subst-matches matches subst)
  (define (submatch n)
    (if (vector? matches)
        (irregex-match-substring matches n)
        (list-ref matches n)))
  (and
   matches
   (call-with-output-string
     (lambda (out)
       (call-with-input-string subst
         (lambda (in)
           (let lp ()
             (let ((c (read-char in)))
               (cond
                ((not (eof-object? c))
                 (case c
                   ((#\&)
                    (display (or (submatch 0) "") out))
                   ((#\\)
                    (let ((c (read-char in)))
                      (if (char-numeric? c)
                          (display
                           (or (submatch (string->number (string c))) "")
                           out)
                          (write-char c out))))
                   (else
                    (write-char c out)))
                 (lp)))))))))))

(define (test-re matcher line)
  (apply 
   (lambda (pattern input result subst output)
     (let ((name (sprintf "~A  ~A  ~A" pattern input result)))
       (cond
	((equal? "c" result)
	 (test-error name (matcher pattern input)))
	((equal? "n" result)
	 (test-assert name (not (matcher pattern input))))
	((equal? "y" result)
	 (test-assert name (matcher pattern input)))
	(else
	 (test-equal name
		     (subst-matches (matcher pattern input) subst)
		     result)))))
   (string-split line "\t" #t)))


(test-begin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(for-each
 (lambda (opts)
   (test-group (sprintf "irregex - ~S" opts)
     (with-input-from-file "re-tests.txt"
       (lambda ()
         (port-for-each
          (lambda (line)
            (test-re (lambda (pat str)
                       (irregex-search (apply irregex pat opts) str))
                     line))
          read-line)))))
 '((small) (fast)))

(test-group "regex"
   (with-input-from-file "re-tests.txt"
     (lambda ()
       (port-for-each
        (lambda (line) (test-re string-search line))
        read-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "utils"
  (test-equal "replace" 
      (irregex-replace "[aeiou]" "hello world" "*")
      "h*llo world")
  (test-equal "replace/all"
      (irregex-replace/all "[aeiou]" "hello world" "*")
      "h*ll* w*rld"))

(test-end)
(test-exit)

