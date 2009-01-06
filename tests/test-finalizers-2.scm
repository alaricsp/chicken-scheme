;;;; test-finalizers-2.scm - test finalizers + GC roots


(use srfi-1)

(define *n* 1000)
(define *count* 0)

#>
static void *
makef(int f, ___scheme_value x)
{
  void *r = f ? CHICKEN_new_finalizable_gc_root() : CHICKEN_new_gc_root();

  CHICKEN_gc_root_set(r, x);
  return r;
}

static void
freef(void *r)
{
  CHICKEN_delete_gc_root(r);
}
<#


(define makef (foreign-lambda c-pointer "makef" bool scheme-object))
(define freef (foreign-lambda void "freef" c-pointer))

(define ((fin f e) x)
  (set! *count* (add1 *count*))
  (assert ((if e even? odd?) (car x)))
  (when e (freef f)))

(print "creating gc roots")

(let* ((x (list-tabulate *n* list))
       (fs (circular-list #t #f))
       (rs (map makef fs x)))
  (for-each 
   (lambda (x f e)
     (set-finalizer! x (fin f e)))
   x rs fs)
  (print "forcing finalizers")
  (##sys#force-finalizers)
  (assert (zero? *count*))
  (print "dropping data")
  (set! x #f)
  (print "forcing finalizables")
  (##sys#force-finalizers)
  (print *count*)
  (assert (= (quotient *n* 2) *count*))
  (print "releasing non-finalizable gc roots")
  (for-each 
   (lambda (f e)
     (unless e (freef f)))
   rs fs)
  (print "forcing remaining")
  (##sys#force-finalizers)
  (assert (= *n* *count*)))
