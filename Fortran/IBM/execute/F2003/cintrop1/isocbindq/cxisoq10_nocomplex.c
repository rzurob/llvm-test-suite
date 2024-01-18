
/*
        C code for testcase "fxisoq10.f" and "fxisoq11.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

void sub1(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(23);
      a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
   }

}

void sub2(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(27);
   }

}

void sub2a(const long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(31);
   }

}

void sub3(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(35);
      a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
   }

}

void sub4(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
   }

}

void sub5(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(39);
         aa[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }

}

void sub6(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(43);
      }
   }

}

void sub6a(const long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(47);
      }
   }

}

void sub7(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(51);
         aa[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }

}

void sub8(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }

}
