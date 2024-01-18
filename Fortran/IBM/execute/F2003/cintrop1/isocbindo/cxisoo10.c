
/*
        C code for testcase "fxisoo10.f" and "fxisoo11.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

void sub1(long double _Complex *a) {
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

}

void sub2(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(25);
#else
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(27);
#endif
   }

}

void sub2a(const long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(29);
#else
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(31);
#endif
   }

}

void sub3(long double _Complex *a) {
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

}

void sub4(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      a[i] = (long double)(i+2)+I*(long double)(i+2);
#else
      a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
#endif
   }

}

void sub5(long double _Complex aa[][10]) {
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

}

void sub6(long double _Complex aa[][10]) {
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

}

void sub6a(const long double _Complex aa[][10]) {
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

}

void sub7(long double _Complex aa[][10]) {
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

}

void sub8(long double _Complex aa[][10]) {
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

}
