
/*
        C code for testcase "fxisopbb01.f"
*/

#include <stdio.h>
#include <stdlib.h>

struct dts0 {
   char a;
};

void sub1(struct dts0 **p) {

   if ( (*p)->a != 'A' ) exit(21);
   *p = malloc(sizeof(**p));
   (*p)->a = 'C';

}

