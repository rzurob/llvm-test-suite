
/*
        C code for testcase "fxisom04.f" and "fxisom05.f"
*/

#include <stdio.h>
#include <stdlib.h>

void sub1(long double *a) {

   if ( *a != 5.0l ) exit(21);

   *a = *a + 5.0l;

}

void sub2(long double a) {

   if ( a != 5.0l ) exit(23);

   a = a + 5.0l;

}

void sub3(long double *a) {

   if ( *a != 5.0l ) exit(25);

}

void sub4(long double a) {

   if ( a != 5.0l ) exit(27);

}

void sub5(const long double *a) {

   if ( *a != 5.0l ) exit(29);

}

void sub6(const long double a) {

   if ( a != 5.0l ) exit(31);

}
