
/*
        C code for testcase "fxisoo02.f" and "fxisoo03.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

long double _Complex fnt1(long double _Complex *a) {

   if ( *a != 5.0l+I*5.0l ) exit(21);

   *a = *a + 5.0l+I*5.0l;

   return 0;
}

long double _Complex fnt2(long double _Complex a) {

   if ( a != 5.0l+I*5.0l ) exit(25);

   a = a + 5.0l+I*5.0l;

   return 0;
}

long double _Complex fnt3(long double _Complex *a) {

   if ( *a != 5.0l+I*5.0l ) exit(29);

   return 0;
}

long double _Complex fnt4(long double _Complex a) {

   if ( a != 5.0l+I*5.0l ) exit(33);

   return 0;
}

long double _Complex fnt5(const long double _Complex *a) {

   if ( *a != 5.0l+I*5.0l ) exit(37);

   return 0;
}

long double _Complex fnt6(const long double _Complex a) {

   if ( a != 5.0l+I*5.0l ) exit(41);

   return 0;
}
