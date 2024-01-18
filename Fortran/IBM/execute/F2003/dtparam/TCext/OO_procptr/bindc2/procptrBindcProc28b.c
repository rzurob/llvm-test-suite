
#include <stdio.h>
#include <stdlib.h>

void csub(signed char **p) {

   if ( **p != 'A' ) exit(41);

   *p = malloc(sizeof(**p));
   **p = 'C';

}

signed char * cfunc(signed char **p) {

   if ( **p != 'A' ) exit(42);

   *p = malloc(sizeof(**p));
   **p = 'C';

   return *p;
}
