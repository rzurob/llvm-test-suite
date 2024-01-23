
/*
        C code for testcase "fxisopbb00.f"
*/

#include <stdio.h>
#include <stdlib.h>

void sub1(char **p) {

   if ( **p != 'A' ) exit(21);

   *p = malloc(sizeof(**p));
   **p = 'C';

}

