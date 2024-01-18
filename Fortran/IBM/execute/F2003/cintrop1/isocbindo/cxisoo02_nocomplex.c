
/*
        C code for testcase "fxisoo02.f" and "fxisoo03.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

long double _Complex fnt1(long double _Complex *a) {

   if ( *a != createcomplexl(5.0l,5.0l) ) exit(23);

   *a = *a + createcomplexl(5.0l,5.0l);

   return 0;
}

long double _Complex fnt2(long double _Complex a) {

   if ( a != createcomplexl(5.0l,5.0l) ) exit(27);

   a = a + createcomplexl(5.0l,5.0l);

   return 0;
}

long double _Complex fnt3(long double _Complex *a) {

   if ( *a != createcomplexl(5.0l,5.0l) ) exit(31);

   return 0;
}

long double _Complex fnt4(long double _Complex a) {

   if ( a != createcomplexl(5.0l,5.0l) ) exit(35);

   return 0;
}

long double _Complex fnt5(const long double _Complex *a) {

   if ( *a != createcomplexl(5.0l,5.0l) ) exit(39);

   return 0;
}

long double _Complex fnt6(const long double _Complex a) {

   if ( a != createcomplexl(5.0l,5.0l) ) exit(43);

   return 0;
}
