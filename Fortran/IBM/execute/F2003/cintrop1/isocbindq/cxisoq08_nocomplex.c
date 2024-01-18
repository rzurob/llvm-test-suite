
/*
        C code for testcase "fxisoq08.f" and "fxisoq09.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

long double _Complex fnt1(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(23);
      a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
   }

   return 0;
}

long double _Complex fnt2(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(27);
   }

   return 0;
}

long double _Complex fnt2a(const long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(31);
   }

   return 0;
}

long double _Complex fnt3(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(35);
      a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
   }

   return 0;
}

long double _Complex fnt4(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
   }

   return 0;
}

long double _Complex fnt5(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(39);
         aa[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }

   return 0;
}

long double _Complex fnt6(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(43);
      }
   }

   return 0;
}

long double _Complex fnt6a(const long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(47);
      }
   }

   return 0;
}

long double _Complex fnt7(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(51);
         aa[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }

   return 0;
}

long double _Complex fnt8(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }

   return 0;
}
