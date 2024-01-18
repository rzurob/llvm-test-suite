
#include <stdio.h>
#include <stdlib.h>

void csub(double **p) {

   if ( **p != 5.0f ) exit(41);

   *p = malloc(sizeof(**p));
   **p = 10.0f;

}

double * cfunc(double **p) {

   if ( **p != 5.0f ) exit(42);

   *p = malloc(sizeof(**p));
   **p = 10.0f;

   return *p;
}
