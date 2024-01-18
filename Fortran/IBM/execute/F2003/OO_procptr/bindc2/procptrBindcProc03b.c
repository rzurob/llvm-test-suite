
#include <stdio.h>
#include <stdlib.h>

void csub(long int **p) {

   if ( **p != 34 ) exit(41);

   *p = malloc(sizeof(**p));
   **p = 23;

}

long int * cfunc(long int **p) {

   if ( **p != 34 ) exit(42);

   *p = malloc(sizeof(**p));
   **p = 22;

   return *p;
}
