
/*
        C code for testcase "fxisopba00.f"
*/

#include <stdio.h>
#include <stdlib.h>

void sub1(signed char **p) {

   if ( **p != 'A' ) exit(21);

   *p = malloc(sizeof(**p));
   **p = 'C';

}

