;;;; test-gc-hooks.scm

#>

static int count = 0;

static void 
gc_start(int mode)
{
  printf(">>>>> GC pre hook - mode=%d, count=%d\n", mode, count++);
}

static void
gc_end(int mode, long ms)
{
  printf("<<<<< GC post hook - mode=%d, count=%d, ms=%ld\n", mode, --count, ms);
}

<#

(set-gc-report! #t)

(foreign-code #<<EOF
C_pre_gc_hook = gc_start;
C_post_gc_hook = gc_end;
EOF
)

(print "major gc ...")
(gc)
(print "minor gc ...")
(gc #f)
(print "alloc ...")
(make-string 10000000)
(print "resize ...")
(##sys#gc '())
(print "major gc ...")
(gc)
(print "minor gc ...")
(gc #f)

(assert (zero? (foreign-value "count" int)))
