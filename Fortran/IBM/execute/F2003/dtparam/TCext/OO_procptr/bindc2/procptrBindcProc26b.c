
#include <stdio.h>
#include <stdlib.h>

void csub(_Bool **p) {

   if ( **p != 1 ) exit(41);

   *p = malloc(sizeof(**p));
   **p = 0;

}

_Bool * cfunc(_Bool **p) {

   if ( **p != 1 ) exit(42);

   *p = malloc(sizeof(**p));
   **p = 0;

   return *p;
}
