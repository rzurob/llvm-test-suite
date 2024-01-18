
/*
        C code for testcase "fxbind_c12xxs.f"
*/

#include <stdio.h>
#include <stdlib.h>

int sub1(int **p) {

   if ( **p != 5 ) exit(21);

   *p = malloc(sizeof(**p));
   **p = 10;
   return (**p);

}

