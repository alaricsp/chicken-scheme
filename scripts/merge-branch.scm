;;;; merge-branch.scm

(load-relative "tools.scm")

(define-constant +repobase+ "https://chicken.wiki.br/svn/chicken-eggs/chicken")

(define (usage)
  (print " usage: csi -s merge-branch.scm [--dry-run] [--diff[=REVS]] "
	 "[--repourl=URL] [--verbose] BRANCHFROM BRANCHTO [WC]")
  (exit 1) )

(define *dry-run* #f)
(define *diff* #f)

(define (do-merge b1 b2 #!optional (wc "."))
  (run (svn 
	,(if *diff* 
	     `("diff" ,(if (string? *diff*) (conc "-r" *diff*) ""))
	     "merge")
	,(if *dry-run* "--dry-run" "")
	,(make-pathname +repobase+ b2)
	,(make-pathname +repobase+ b1)
	,(if (not *diff*) wc "") ) ) )

(let ((args (simple-args)))
  (if (>= (length args) 2)
      (apply do-merge (car args) (cadr args) (cddr args))
      (usage)))

(exit)
