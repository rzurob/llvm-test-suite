
/*
        C code for testcase "fxisoq04.f" and "fxisoq05.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

void sub1(long double _Complex *a) {

   if ( *a != 5.0l+I*5.0l ) exit(21);

   *a = *a + 5.0l+I*5.0l;

}

void sub2(long double _Complex a) {

   if ( a != 5.0l+I*5.0l ) exit(25);

   a = a + 5.0l+I*5.0l;

}

void sub3(long double _Complex *a) {

   if ( *a != 5.0l+I*5.0l ) exit(29);

}

void sub4(long double _Complex a) {

   if ( a != 5.0l+I*5.0l ) exit(33);

}

void sub5(const long double _Complex *a) {

   if ( *a != 5.0l+I*5.0l ) exit(37);

}

void sub6(const long double _Complex a) {

   if ( a != 5.0l+I*5.0l ) exit(41);

}
