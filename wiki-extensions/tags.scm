; This extension implements support for basic tags.
;
; See *extensions* at the end for a list of all them.

(require-extension srfi-40)

; Deprecated:
(define (unsupported name output-format text props)
  (string->stream (format #f "[[~A is unsupported for ~A]]~%" name output-format)))

; Deprecated:
(define (tag-span-case name latex texi odf)
  `(,name
     (code-span
       ,(lambda (params text props)
          (let ((output-format ((cadr (assoc 'output-format props)))))
            ((case output-format
               ((html) html-tag)
               ((latex) latex)
               ((odf) odf)
               ((texi) texi))
             name
             output-format
             text
             props))))))

; Deprecated:
(define (html-tag name format text props)
  (stream-append
    (stream-cons #\< (symbol->stream name))
    (stream-cons #\> ((cadr (assoc 'parse props)) text))
    (stream-cons* #\< #\/ (symbol->stream name))
    (stream #\>)))

(define (apply-driver f params text props)
  (f ((cadr (assoc 'parse props)) text)))

(define (apply-driver-paragraph f params text props parser-props)
  (f ((cadr (assoc 'parse-paragraph props)) text parser-props)))

(define (tag-span-driver name field func)
  `(,name
     (code-span
       ,(lambda (params text props)
          (func (field (cadr (assoc 'driver props))) params text props)))))

(define *extensions*
  (list
    (tag-span-driver 'pre driver-literal
      (lambda (f params text props)
        (apply-driver-paragraph f params text props '((literal #t)))))
    (tag-span-driver 'center driver-center apply-driver)
    (tag-span-driver 'small driver-small apply-driver)
    (tag-span-driver 'big driver-big apply-driver)
    (tag-span-driver 'h1 driver-header
      (lambda (f params text props) (f text 1)))
    (tag-span-driver 'h2 driver-header
      (lambda (f params text props) (f text 2)))
    (tag-span-driver 'h3 driver-header
      (lambda (f params text props) (f text 3)))
    (tag-span-driver 'h4 driver-header
      (lambda (f params text props) (f text 4)))
    (tag-span-driver 'h5 driver-header
      (lambda (f params text props) (f text 5)))
    (tag-span-driver 'h6 driver-header
      (lambda (f params text props) (f text 6)))
    (tag-span-case 'table unsupported unsupported unsupported)
    (tag-span-case 'tr unsupported unsupported unsupported)
    (tag-span-case 'td unsupported unsupported unsupported)
    (tag-span-case 'th unsupported unsupported unsupported)
    (tag-span-driver 'blockquote driver-blockquote apply-driver)))
