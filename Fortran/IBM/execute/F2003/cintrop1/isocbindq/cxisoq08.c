
/*
        C code for testcase "fxisoq08.f" and "fxisoq09.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

long double _Complex fnt1(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(21);
#else
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(23);
#endif
#ifdef CMPLX
      a[i] = (long double)(i+2)+I*(long double)(i+2);
#else
      a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
#endif
   }

   return 0;
}

long double _Complex fnt2(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(25);
#else
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(27);
#endif
   }

   return 0;
}

long double _Complex fnt2a(const long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(29);
#else
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(31);
#endif
   }

   return 0;
}

long double _Complex fnt3(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(33);
#else
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(35);
#endif
#ifdef CMPLX
      a[i] = (long double)(i+2)+I*(long double)(i+2);
#else
      a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
#endif
   }

   return 0;
}

long double _Complex fnt4(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      a[i] = (long double)(i+2)+I*(long double)(i+2);
#else
      a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
#endif
   }

   return 0;
}

long double _Complex fnt5(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(37);
#else
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(39);
#endif
#ifdef CMPLX
         aa[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         aa[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }

   return 0;
}

long double _Complex fnt6(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(41);
#else
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(43);
#endif
      }
   }

   return 0;
}

long double _Complex fnt6a(const long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(45);
#else
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(47);
#endif
      }
   }

   return 0;
}

long double _Complex fnt7(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(49);
#else
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(51);
#endif
#ifdef CMPLX
         aa[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         aa[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }

   return 0;
}

long double _Complex fnt8(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         aa[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         aa[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }

   return 0;
}
