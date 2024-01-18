
#include <stdio.h>
#include <stdlib.h>

void csub(short int **p) {

   if ( **p != 34 ) exit(41);

   *p = malloc(sizeof(**p));
   **p = 23;

}

short int * cfunc(short int **p) {

   if ( **p != 34 ) exit(42);

   *p = malloc(sizeof(**p));
   **p = 22;

   return *p;
}
