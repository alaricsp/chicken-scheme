#include <stdio.h>
#include <stdlib.h>

static void foo()
{
  throw 123;
}

int main(int argc, char *argv[])
{
  int count = argc == 1 ? 10000 : atoi(argv[ 1 ]);
  int n = 0;

  for(int i = 0; i < count; ++i) {
    try {
      foo();
    }
    catch(...) {
      ++n;
    }
  }

  printf("%d\n", n);
  return 0;
}
