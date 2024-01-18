
#include <stdio.h>
#include <stdlib.h>

void csub(int **p) {

   if ( **p != 34 ) exit(41);

   *p = malloc(sizeof(**p));

   **p = 23;

}


int * cfunc(int **p) {

   if ( **p != 34 ) exit(42);

   *p = malloc(sizeof(**p));

   **p = 22;

   return *p;
}
