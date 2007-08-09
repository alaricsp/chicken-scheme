;;; locative-stress-test.scm - by Kon Lovett

(declare (usual-integrations))

;(set-gc-report! #t)

(require-extension srfi-1)
#>
long *ptrs[10];

//#define check(n)  ptrs[n] = o##n; if(!C_in_stackp((C_word)o##n) && !C_in_fromspacep((C_word)o##n)) C_dbg_hook(0);
#define check(n)

long fill_10(long i, long *o0, long *o1, long *o2, long *o3, long *o4,
                     long *o5, long *o6, long *o7, long *o8, long *o9)
{
 check(0)
 check(1)
 check(2)
 check(3)
 check(4)
 check(5)
 check(6)
 check(7)
 check(8)
 check(9)
 *o0=*o1=*o2=*o3=*o4=*o5=*o6=*o7=*o8=*o9=i;
 return i;
}
<#

(define fill-10!
  (foreign-lambda long "fill_10" long 
                  (c-pointer long) (c-pointer long) (c-pointer long)
                  (c-pointer long) (c-pointer long) (c-pointer long)
                  (c-pointer long) (c-pointer long) (c-pointer long)
                  (c-pointer long)))

(let* ((el 1)
       (expected (make-list 10 el)))
  (let loop
      ((i (string->number (:optional (command-line-arguments) "100000"))))
    (unless (eq? i 0)
      (let-location ((o0 long) (o1 long) (o2 long) (o3 long) (o4 long)
		     (o5 long) (o6 long) (o7 long) (o8 long) (o9 long))
	(fill-10! el #$o0 #$o1 #$o2 #$o3 #$o4 #$o5 #$o6 #$o7 #$o8 #$o9)
	(let ((result (list o0 o1 o2 o3 o4 o5 o6 o7 o8 o9)))
	  (if (not (equal? result expected))
	      (error "strange values: " result)
	      (loop (fx- i 1))))))))
