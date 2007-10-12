#include <stdio.h>
#include <stdlib.h>
#include <chicken.h>

int main()
{
  CHICKEN_run((void*)CHICKEN_default_toplevel);

  return 0;
}
