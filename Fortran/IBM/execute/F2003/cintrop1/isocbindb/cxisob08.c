
/*
        C code for testcase "fxisob08.f" and "fxisob09.f"
*/

#include <stdio.h>
#include <stdlib.h>


long fnt1(long *a, long long *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(21);
      a[i] = i+2;
      if ( b[i] != i+1 ) exit(23);
      b[i] = i+2;
   }

   return 0;
}

long fnt2(long *a, long long *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(25);
      if ( b[i] != i+1 ) exit(27);
   }

   return 0;
}

long fnt2a(const long *a, const long long *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(29);
      if ( b[i] != i+1 ) exit(31);
   }

   return 0;
}

long fnt3(long *a, long long *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(33);
      a[i] = i+2;
      if ( b[i] != i+1 ) exit(35);
      b[i] = i+2;
   }

   return 0;
}

long fnt4(long *a, long long *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = i+2;
      b[i] = i+2;
   }

   return 0;
}

long fnt5(long aa[][10], long long bb[][10]) {
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

long fnt6(long aa[][10], long long bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(41);
         if ( bb[i][j] != i+j+1 ) exit(43);
      }
   }

   return 0;
}

long fnt6a(const long aa[][10], const long long bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(45);
         if ( bb[i][j] != i+j+1 ) exit(47);
      }
   }

   return 0;
}

long fnt7(long aa[][10], long long bb[][10]) {
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

long fnt8(long aa[][10], long long bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = i+j+2;
         bb[i][j] = i+j+2;
      }
   }

   return 0;
}