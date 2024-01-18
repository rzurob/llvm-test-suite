
/*
        C code for testcase "fxisol08.f" and "fxisol09.f"
*/

#include <stdio.h>
#include <stdlib.h>


float fnt1(float *a, double *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1) ) exit(21);
      a[i] = (float)(i+2);
      if ( b[i] != (double)(i+1) ) exit(23);
      b[i] = (double)(i+2);
   }

   return 0;
}

float fnt2(float *a, double *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1) ) exit(25);
      if ( b[i] != (double)(i+1) ) exit(27);
   }

   return 0;
}

float fnt2a(const float *a, const double *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1) ) exit(29);
      if ( b[i] != (double)(i+1) ) exit(31);
   }

   return 0;
}

float fnt3(float *a, double *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1) ) exit(33);
      a[i] = (float)(i+2);
      if ( b[i] != (double)(i+1) ) exit(35);
      b[i] = (double)(i+2);
   }

   return 0;
}

float fnt4(float *a, double *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = (float)(i+2);
      b[i] = (double)(i+2);
   }

   return 0;
}

float fnt5(float aa[][10], double bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1) ) exit(37);
         aa[i][j] = i+j+2;
         if ( bb[i][j] != (double)(i+j+1) ) exit(39);
         bb[i][j] = i+j+2;
      }
   }

   return 0;
}

float fnt6(float aa[][10], double bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1) ) exit(41);
         if ( bb[i][j] != (double)(i+j+1) ) exit(43);
      }
   }

   return 0;
}

float fnt6a(const float aa[][10], const double bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1) ) exit(45);
         if ( bb[i][j] != (double)(i+j+1) ) exit(47);
      }
   }

   return 0;
}

float fnt7(float aa[][10], double bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1) ) exit(49);
         aa[i][j] = i+j+2;
         if ( bb[i][j] != (double)(i+j+1) ) exit(51);
         bb[i][j] = i+j+2;
      }
   }

   return 0;
}

float fnt8(float aa[][10], double bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = i+j+2;
         bb[i][j] = i+j+2;
      }
   }

   return 0;
}
