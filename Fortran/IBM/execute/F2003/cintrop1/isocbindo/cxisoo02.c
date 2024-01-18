
/*
        C code for testcase "fxisoo02.f" and "fxisoo03.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

long double _Complex fnt1(long double _Complex *a) {

#ifdef CMPLX
   if ( *a != 5.0l+I*5.0l ) exit(21);
#else
   if ( *a != createcomplexl(5.0l,5.0l) ) exit(23);
#endif

#ifdef CMPLX
   *a = *a + 5.0l+I*5.0l;
#else
   *a = *a + createcomplexl(5.0l,5.0l);
#endif

   return 0;
}

long double _Complex fnt2(long double _Complex a) {

#ifdef CMPLX
   if ( a != 5.0l+I*5.0l ) exit(25);
#else
   if ( a != createcomplexl(5.0l,5.0l) ) exit(27);
#endif

#ifdef CMPLX
   a = a + 5.0l+I*5.0l;
#else
   a = a + createcomplexl(5.0l,5.0l);
#endif

   return 0;
}

long double _Complex fnt3(long double _Complex *a) {

#ifdef CMPLX
   if ( *a != 5.0l+I*5.0l ) exit(29);
#else
   if ( *a != createcomplexl(5.0l,5.0l) ) exit(31);
#endif

   return 0;
}

long double _Complex fnt4(long double _Complex a) {

#ifdef CMPLX
   if ( a != 5.0l+I*5.0l ) exit(33);
#else
   if ( a != createcomplexl(5.0l,5.0l) ) exit(35);
#endif

   return 0;
}

long double _Complex fnt5(const long double _Complex *a) {

#ifdef CMPLX
   if ( *a != 5.0l+I*5.0l ) exit(37);
#else
   if ( *a != createcomplexl(5.0l,5.0l) ) exit(39);
#endif

   return 0;
}

long double _Complex fnt6(const long double _Complex a) {

#ifdef CMPLX
   if ( a != 5.0l+I*5.0l ) exit(41);
#else
   if ( a != createcomplexl(5.0l,5.0l) ) exit(43);
#endif

   return 0;
}
