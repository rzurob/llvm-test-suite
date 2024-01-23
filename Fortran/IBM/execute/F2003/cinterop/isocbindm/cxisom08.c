
/*
        C code for testcase "fxisom08.f" and "fxisom09.f"
*/

#include <stdio.h>
#include <stdlib.h>

long double fnt1(long double *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1) ) exit(21);
      a[i] = (long double)(i+2);
   }

   return 0;
}

long double fnt2(long double *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1) ) exit(23);
   }

   return 0;
}

long double fnt2a(const long double *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1) ) exit(25);
   }

   return 0;
}

long double fnt3(long double *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1) ) exit(27);
      a[i] = (long double)(i+2);
   }

   return 0;
}

long double fnt4(long double *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = (long double)(i+2);
   }

   return 0;
}

long double fnt5(long double aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1) ) exit(29);
         aa[i][j] = i+j+2;
      }
   }

   return 0;
}

long double fnt6(long double aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1) ) exit(31);
      }
   }

   return 0;
}

long double fnt6a(const long double aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1) ) exit(33);
      }
   }

   return 0;
}

long double fnt7(long double aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1) ) exit(35);
         aa[i][j] = i+j+2;
      }
   }

   return 0;
}

long double fnt8(long double aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = i+j+2;
      }
   }

   return 0;
}
