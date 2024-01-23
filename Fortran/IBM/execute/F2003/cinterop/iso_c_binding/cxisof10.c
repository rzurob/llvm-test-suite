
/*
        C code for testcase "fxisof10.f" and "fxisof11.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

void sub1(int_least32_t *a, int_least64_t *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(21);
      a[i] = i+2;
      if ( b[i] != i+1 ) exit(23);
      b[i] = i+2;
   }

}

void sub2(int_least32_t *a, int_least64_t *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(25);
      if ( b[i] != i+1 ) exit(27);
   }

}

void sub2a(const int_least32_t *a, const int_least64_t *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(29);
      if ( b[i] != i+1 ) exit(31);
   }

}

void sub3(int_least32_t *a, int_least64_t *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(33);
      a[i] = i+2;
      if ( b[i] != i+1 ) exit(35);
      b[i] = i+2;
   }

}

void sub4(int_least32_t *a, int_least64_t *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = i+2;
      b[i] = i+2;
   }

}

void sub5(int_least32_t aa[][10], int_least64_t bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(37);
         aa[i][j] = i+j+2;
         if ( bb[i][j] != i+j+1 ) exit(39);
         bb[i][j] = i+j+2;
      }
   }

}

void sub6(int_least32_t aa[][10], int_least64_t bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(41);
         if ( bb[i][j] != i+j+1 ) exit(43);
      }
   }

}

void sub6a(const int_least32_t aa[][10], const int_least64_t bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(45);
         if ( bb[i][j] != i+j+1 ) exit(47);
      }
   }

}

void sub7(int_least32_t aa[][10], int_least64_t bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(49);
         aa[i][j] = i+j+2;
         if ( bb[i][j] != i+j+1 ) exit(51);
         bb[i][j] = i+j+2;
      }
   }

}

void sub8(int_least32_t aa[][10], int_least64_t bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = i+j+2;
         bb[i][j] = i+j+2;
      }
   }

}
