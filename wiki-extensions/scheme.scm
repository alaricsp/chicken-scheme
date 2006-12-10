(define (tag-scheme params text info)
  (let ((env (make-safe-environment parent: default-safe-environment extendable: #t)))
    (safe-environment-set! env 'display display)
    (safe-environment-set! env 'format format)
    (safe-environment-set! env 'newline newline)
    (with-output-to-stream
      (lambda ()
        (with-input-from-string
          (stream->string text)
          (lambda ()
            (let loop ((expr (read)))
              (unless (eof-object? expr)
                (safe-eval expr environment: env fuel: 100000 allocation-limit: 100000)
                (loop (read))))))))))

(set! *extensions*
  `((scheme (code-span ,tag-scheme))))
