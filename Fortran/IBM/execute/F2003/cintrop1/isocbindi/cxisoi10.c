
/*
        C code for testcase "fxisoi10.f" and "fxisoi11.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

void sub1(int_fast16_t *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(21);
      a[i] = i+2;
   }

}

void sub2(int_fast16_t *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(23);
   }

}

void sub2a(const int_fast16_t *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(25);
   }

}

void sub3(int_fast16_t *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(27);
      a[i] = i+2;
   }

}

void sub4(int_fast16_t *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = i+2;
   }

}

void sub5(int_fast16_t aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(29);
         aa[i][j] = i+j+2;
      }
   }

}

void sub6(int_fast16_t aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(31);
      }
   }

}

void sub6a(const int_fast16_t aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(33);
      }
   }

}

void sub7(int_fast16_t aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(35);
         aa[i][j] = i+j+2;
      }
   }

}

void sub8(int_fast16_t aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = i+j+2;
      }
   }

}
