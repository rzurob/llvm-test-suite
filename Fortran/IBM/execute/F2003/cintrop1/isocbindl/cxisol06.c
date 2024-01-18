
/*
	C code for testcase "fxisol06.f"
*/

#include <stdio.h>
#include <stdlib.h>


void initarr1d(float *, double *);
void initarr2d(float [][10], double [][10]);

int main() {

   float fnt1(float *, double *);
   float fnt2(float *, double *);
   float fnt2a(const float *, const double *);
   float fnt3(float *, double *);
   float fnt4(float *, double *);
   float fnt5(float [], double []);
   float fnt6(float [], double []);
   float fnt6a(const float [], const double []);
   float fnt7(float [], double []);
   float fnt8(float [], double []);

   float a[5], aa[5][10], ret;
   double b[5], bb[5][10];
   int i, j;

/* Test 1 */

   initarr1d(a,b);

   ret = fnt1(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+2) ) exit(21);
      if ( b[i] != (double)(i+2) ) exit(23);
   }

/* Test 2 */

   initarr1d(a,b);

   ret = fnt2(a,&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1) ) exit(25);
      if ( b[i] != (double)(i+1) ) exit(27);
   }

/* Test 2a */

   initarr1d(a,b);

   ret = fnt2a(a,&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+1) ) exit(29);
      if ( b[i] != (double)(i+1) ) exit(31);
   }

/* Test 3 */

   initarr1d(a,b);

   ret = fnt3(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+2) ) exit(33);
      if ( b[i] != (double)(i+2) ) exit(35);
   }

/* Test 4 */

   initarr1d(a,b);

   ret = fnt4(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (float)(i+2) ) exit(37);
      if ( b[i] != (double)(i+2) ) exit(39);
   }

/* Test 5 */

   initarr2d(aa,bb);

   ret = fnt5(aa[0],&bb[0][0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+2) ) exit(41);
         if ( bb[i][j] != (double)(i+j+2) ) exit(43);
      }
   }

/* Test 6 */

   initarr2d(aa,bb);

   ret = fnt6(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1) ) exit(45);
         if ( bb[i][j] != (double)(i+j+1) ) exit(47);
      }
   }

/* Test 6a */

   initarr2d(aa,bb);

   ret = fnt6a(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+1) ) exit(49);
         if ( bb[i][j] != (double)(i+j+1) ) exit(51);
      }
   }

/* Test 7 */

   initarr2d(aa,bb);

   ret = fnt7(&aa[0][0],bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+2) ) exit(53);
         if ( bb[i][j] != (double)(i+j+2) ) exit(55);
      }
   }

/* Test 8 */

   initarr2d(aa,bb);

   ret = fnt8(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (float)(i+j+2) ) exit(57);
         if ( bb[i][j] != (double)(i+j+2) ) exit(59);
      }
   }

   return 0;

}

void initarr1d(float *x, double *y) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x[i] = (float)(i+1);
   }

   for ( i = 0; i < 5; i++ ) {
      y[i] = (double)(i+1);
   }

}

void initarr2d(float xx[][10], double yy[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         xx[i][j] = i+j+1;
      }
   }

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         yy[i][j] = i+j+1;
      }
   }

}

