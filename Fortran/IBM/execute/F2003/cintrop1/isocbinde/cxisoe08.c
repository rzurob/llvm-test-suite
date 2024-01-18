
/*
        C code for testcase "fxisoe08.f" and "fxisoe09.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

int_least8_t fnt1(int_least8_t *a, int_least16_t *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(21);
      a[i] = i+2;
      if ( b[i] != i+1 ) exit(23);
      b[i] = i+2;
   }

   return 0;
}

int_least8_t fnt2(int_least8_t *a, int_least16_t *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(25);
      if ( b[i] != i+1 ) exit(27);
   }

   return 0;
}

int_least8_t fnt2a(const int_least8_t *a, const int_least16_t *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(29);
      if ( b[i] != i+1 ) exit(31);
   }

   return 0;
}

int_least8_t fnt3(int_least8_t *a, int_least16_t *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(33);
      a[i] = i+2;
      if ( b[i] != i+1 ) exit(35);
      b[i] = i+2;
   }

   return 0;
}

int_least8_t fnt4(int_least8_t *a, int_least16_t *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = i+2;
      b[i] = i+2;
   }

   return 0;
}

int_least8_t fnt5(int_least8_t aa[][10], int_least16_t bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(37);
         aa[i][j] = i+j+2;
         if ( bb[i][j] != i+j+1 ) exit(39);
         bb[i][j] = i+j+2;
      }
   }

   return 0;
}

int_least8_t fnt6(int_least8_t aa[][10], int_least16_t bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(41);
         if ( bb[i][j] != i+j+1 ) exit(43);
      }
   }

   return 0;
}

int_least8_t fnt6a(const int_least8_t aa[][10], const int_least16_t bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(45);
         if ( bb[i][j] != i+j+1 ) exit(47);
      }
   }

   return 0;
}

int_least8_t fnt7(int_least8_t aa[][10], int_least16_t bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(49);
         aa[i][j] = i+j+2;
         if ( bb[i][j] != i+j+1 ) exit(51);
         bb[i][j] = i+j+2;
      }
   }

   return 0;
}

int_least8_t fnt8(int_least8_t aa[][10], int_least16_t bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = i+j+2;
         bb[i][j] = i+j+2;
      }
   }

   return 0;
}
