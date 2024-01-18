
/*
        C code for testcase "fxisom02.f" and "fxisom03.f"
*/

#include <stdio.h>
#include <stdlib.h>

long double fnt1(long double *a) {

   if ( *a != 5.0l ) exit(21);

   *a = *a + 5.0l;

   return 0;
}

long double fnt2(long double a) {

   if ( a != 5.0l ) exit(23);

   a = a + 5.0l;

   return 0;
}

long double fnt3(long double *a) {

   if ( *a != 5.0l ) exit(25);

   return 0;
}

long double fnt4(long double a) {

   if ( a != 5.0l ) exit(27);

   return 0;
}

long double fnt5(const long double *a) {

   if ( *a != 5.0l ) exit(29);

   return 0;
}

long double fnt6(const long double a) {

   if ( a != 5.0l ) exit(31);

   return 0;
}
