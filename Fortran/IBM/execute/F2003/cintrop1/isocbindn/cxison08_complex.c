
/*
        C code for testcase "fxison08.f" and "fxison09.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

float _Complex fnt1(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(21);
      a[i] = (float)(i+2)+I*(float)(i+2);
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(25);
      b[i] = (double)(i+2)+I*(double)(i+2);
   }

   return 0;
}

float _Complex fnt2(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(29);
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(33);
   }

   return 0;
}

float _Complex fnt2a(const float _Complex *a, const double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(37);
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(41);
   }

   return 0;
}

float _Complex fnt3(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(45);
      a[i] = (float)(i+2)+I*(float)(i+2);
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(49);
      b[i] = (double)(i+2)+I*(double)(i+2);
   }

   return 0;
}

float _Complex fnt4(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = (float)(i+2)+I*(float)(i+2);
      b[i] = (double)(i+2)+I*(double)(i+2);
   }

   return 0;
}

float _Complex fnt5(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(53);
         aa[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(57);
         bb[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

   return 0;
}

float _Complex fnt6(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(61);
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(65);
      }
   }

   return 0;
}

float _Complex fnt6a(const float _Complex aa[][10], const double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(69);
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(73);
      }
   }

   return 0;
}

float _Complex fnt7(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(77);
         aa[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(81);
         bb[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

   return 0;
}

float _Complex fnt8(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         bb[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

   return 0;
}
