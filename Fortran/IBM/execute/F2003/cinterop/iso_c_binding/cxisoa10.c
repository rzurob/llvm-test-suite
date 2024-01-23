
/*
        C code for testcase "fxisoa10.f" and "fxisoa11.f"
*/

#include <stdio.h>
#include <stdlib.h>


void sub1(int *a, short *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(21);
      a[i] = i+2;
      if ( b[i] != i+1 ) exit(23);
      b[i] = i+2;
   }

}

void sub2(int *a, short *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(25);
      if ( b[i] != i+1 ) exit(27);
   }

}

void sub2a(const int *a, const short *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(29);
      if ( b[i] != i+1 ) exit(31);
   }

}

void sub3(int *a, short *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(33);
      a[i] = i+2;
      if ( b[i] != i+1 ) exit(35);
      b[i] = i+2;
   }

}

void sub4(int *a, short *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = i+2;
      b[i] = i+2;
   }

}

void sub5(int aa[][10], short bb[][10]) {
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

void sub6(int aa[][10], short bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(41);
         if ( bb[i][j] != i+j+1 ) exit(43);
      }
   }

}

void sub6a(const int aa[][10], const short bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(45);
         if ( bb[i][j] != i+j+1 ) exit(47);
      }
   }

}

void sub7(int aa[][10], short bb[][10]) {
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

void sub8(int aa[][10], short bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = i+j+2;
         bb[i][j] = i+j+2;
      }
   }

}
