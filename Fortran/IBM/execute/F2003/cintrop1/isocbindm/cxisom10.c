
/*
        C code for testcase "fxisom10.f" and "fxisom11.f"
*/

#include <stdio.h>
#include <stdlib.h>

void sub1(long double *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1) ) exit(21);
      a[i] = (long double)(i+2);
   }

}

void sub2(long double *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1) ) exit(23);
   }

}

void sub2a(const long double *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1) ) exit(25);
   }

}

void sub3(long double *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1) ) exit(27);
      a[i] = (long double)(i+2);
   }

}

void sub4(long double *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = (long double)(i+2);
   }

}

void sub5(long double aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1) ) exit(29);
         aa[i][j] = i+j+2;
      }
   }

}

void sub6(long double aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1) ) exit(31);
      }
   }

}

void sub6a(const long double aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1) ) exit(33);
      }
   }

}

void sub7(long double aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1) ) exit(35);
         aa[i][j] = i+j+2;
      }
   }

}

void sub8(long double aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = i+j+2;
      }
   }

}
