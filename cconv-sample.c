/* cconv-sample.c */


#include "chicken.h"


void foo(C_word x1, C_word x2, C_word x3, C_word x4, C_word x5, 
	 C_word x6, C_word x7, C_word x8, C_word x9, C_word x10) C_noret;

void 
foo(C_word x1, C_word x2, C_word x3, C_word x4, C_word x5, 
    C_word x6, C_word x7, C_word x8, C_word x9, C_word x10)
{
  exit(0);
}


void
callfoo()
{
  foo(C_fix(1), C_fix(2), C_fix(3), C_fix(4), C_fix(5), 
      C_fix(6), C_fix(7), C_fix(8), C_fix(9), C_fix(10) );
}
