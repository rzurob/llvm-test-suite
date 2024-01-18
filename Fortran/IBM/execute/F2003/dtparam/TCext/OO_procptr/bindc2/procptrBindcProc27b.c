
#include <stdio.h>
#include <stdlib.h>

void csub(char **p) {

   if ( **p != 'A' ) exit(41);

   *p = malloc(sizeof(**p));
   **p = 'C';

}

char * cfunc(char **p) {

   if ( **p != 'A' ) exit(42);

   *p = malloc(sizeof(**p));
   **p = 'C';

   return *p;
}
