(define (tag-nowiki params text info)
  text)

(define *extensions*
  `((nowiki (code-span ,tag-nowiki))))
