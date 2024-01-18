#include <stdio.h>
#include <stdlib.h>

int i = 777;

void cfun(void *fproc())
{
  printf("Printing in C!\n");
  printf("Moving to Fortran...\n");
  fflush(stdout);
  (*fproc)(i);
}
