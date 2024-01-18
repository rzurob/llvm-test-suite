
/*
        C code for testcase "fxisor04.f" and "fxisor05.f"
*/

#include <stdio.h>
#include <stdlib.h>


void sub1(_Bool *a) {

   if ( *a != 1 ) exit(21);

   *a = 0;

}

void sub2(_Bool a) {

   if ( a != 1 ) exit(23);

   a = 0;

}

void sub3(_Bool *a) {

   if ( *a != 1 ) exit(25);

}

void sub4(_Bool a) {

   if ( a != 1 ) exit(27);

}

void sub5(const _Bool *a) {

   if ( *a != 1 ) exit(29);

}

void sub6(const _Bool a) {

   if ( a != 1 ) exit(31);

}
