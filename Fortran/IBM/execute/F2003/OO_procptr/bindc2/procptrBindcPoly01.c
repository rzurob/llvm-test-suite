
#include <stdio.h>
#include <stdlib.h>

int  cfunc(int **p) {

   if ( **p != 34 ) exit(42);

   *p = malloc(sizeof(**p));

   **p = 22;

   return **p;
}
