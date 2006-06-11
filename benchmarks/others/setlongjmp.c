#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

static int n = 0;
static jmp_buf jb;

static void foo()
{
  ++n;
  longjmp(jb, 123);
}

int main(int argc, char *argv[])
{
  int count = argc == 1 ? 10000 : atoi(argv[ 1 ]);
  int i;

  for(i = 0; i < count; ++i) {
    if(!setjmp(jb))
      foo();
  }

  printf("%d\n", n);
  return 0;
}
