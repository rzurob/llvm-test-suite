
/*
        C code for testcase "fxisor10.f" and "fxisor11.f"
*/

#include <stdio.h>
#include <stdlib.h>


void sub1(_Bool *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 1 ) exit(21);
      a[i] = 0;
   }

}

void sub2(_Bool *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 1 ) exit(23);
   }

}

void sub2a(const _Bool *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 1 ) exit(25);
   }

}

void sub3(_Bool *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 1 ) exit(27);
      a[i] = 0;
   }

}

void sub4(_Bool *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = 0;
   }

}

void sub5(_Bool aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 1 ) exit(29);
         aa[i][j] = 0;
      }
   }

}

void sub6(_Bool aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 1 ) exit(31);
      }
   }

}

void sub6a(const _Bool aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 1 ) exit(33);
      }
   }

}

void sub7(_Bool aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 1 ) exit(35);
         aa[i][j] = 0;
      }
   }

}

void sub8(_Bool aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = 0;
      }
   }

}
