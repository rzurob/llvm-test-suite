
/*
C code for testcase "fxison06.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

void initarr1d(float _Complex *, double _Complex *);
void initarr2d(float _Complex [][10], double _Complex [][10]);

int main() {

   float _Complex fnt1(float _Complex *, double _Complex *);
   float _Complex fnt2(float _Complex *, double _Complex *);
   float _Complex fnt2a(const float _Complex *, const double _Complex *);
   float _Complex fnt3(float _Complex *, double _Complex *);
   float _Complex fnt4(float _Complex *, double _Complex *);
   float _Complex fnt5(float _Complex [], double _Complex []);
   float _Complex fnt6(float _Complex [], double _Complex []);
   float _Complex fnt6a(const float _Complex [], const double _Complex []);
   float _Complex fnt7(float _Complex [], double _Complex []);
   float _Complex fnt8(float _Complex [], double _Complex []);

   float _Complex a[5], aa[5][10], ret;
   double _Complex b[5], bb[5][10];
   int i, j;

/* Test 1 */

   initarr1d(a,b);

   ret = fnt1(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+2)+I*(float)(i+2) ) exit(21);
      if ( b[i] != (double)(i+2)+I*(double)(i+2) ) exit(25);
   }

/* Test 2 */

   initarr1d(a,b);

   ret = fnt2(a,&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(29);
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(33);
   }

/* Test 2a */

   initarr1d(a,b);

   ret = fnt2a(a,&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(37);
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(41);
   }

/* Test 3 */

   initarr1d(a,b);

   ret = fnt3(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+2)+I*(float)(i+2) ) exit(45);
      if ( b[i] != (double)(i+2)+I*(double)(i+2) ) exit(49);
   }

/* Test 4 */

   initarr1d(a,b);

   ret = fnt4(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+2)+I*(float)(i+2) ) exit(53);
      if ( b[i] != (double)(i+2)+I*(double)(i+2) ) exit(57);
   }

/* Test 5 */

   initarr2d(aa,bb);

   ret = fnt5(aa[0],&bb[0][0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(61);
         if ( bb[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(65);
      }
   }

/* Test 6 */

   initarr2d(aa,bb);

   ret = fnt6(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(69);
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(73);
      }
   }

/* Test 6a */

   initarr2d(aa,bb);

   ret = fnt6a(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(77);
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(81);
      }
   }

/* Test 7 */

   initarr2d(aa,bb);

   ret = fnt7(&aa[0][0],bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(85);
         if ( bb[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(89);
      }
   }

/* Test 8 */

   initarr2d(aa,bb);

   ret = fnt8(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(93);
         if ( bb[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(97);
      }
   }

   return 0;

}

void initarr1d(float _Complex *x, double _Complex *y) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x[i] = (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 5; i++ ) {
      y[i] = (double)(i+1)+I*(double)(i+1);
   }

}

void initarr2d(float _Complex xx[][10], double _Complex yy[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         xx[i][j] = (float)(i+j+1)+I*(float)(i+j+1);
      }
   }

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         yy[i][j] = (double)(i+j+1)+I*(double)(i+j+1);
      }
   }

}

