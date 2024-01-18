
/*
        C code for testcase "fxisoi08.f" and "fxisoi09.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

int_fast16_t fnt1(int_fast16_t *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(21);
      a[i] = i+2;
   }

   return 0;
}

int_fast16_t fnt2(int_fast16_t *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(23);
   }

   return 0;
}

int_fast16_t fnt2a(const int_fast16_t *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(25);
   }

   return 0;
}

int_fast16_t fnt3(int_fast16_t *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(27);
      a[i] = i+2;
   }

   return 0;
}

int_fast16_t fnt4(int_fast16_t *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = i+2;
   }

   return 0;
}

int_fast16_t fnt5(int_fast16_t aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(29);
         aa[i][j] = i+j+2;
      }
   }

   return 0;
}

int_fast16_t fnt6(int_fast16_t aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(31);
      }
   }

   return 0;
}

int_fast16_t fnt6a(const int_fast16_t aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(33);
      }
   }

   return 0;
}

int_fast16_t fnt7(int_fast16_t aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(35);
         aa[i][j] = i+j+2;
      }
   }

   return 0;
}

int_fast16_t fnt8(int_fast16_t aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = i+j+2;
      }
   }

   return 0;
}
