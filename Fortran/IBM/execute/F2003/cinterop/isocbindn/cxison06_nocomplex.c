
/*
C code for testcase "fxison06.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

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
      if ( a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(23);
      if ( b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(27);
   }

/* Test 2 */

   initarr1d(a,b);

   ret = fnt2(a,&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(31);
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(35);
   }

/* Test 2a */

   initarr1d(a,b);

   ret = fnt2a(a,&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(39);
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(43);
   }

/* Test 3 */

   initarr1d(a,b);

   ret = fnt3(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(47);
      if ( b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(51);
   }

/* Test 4 */

   initarr1d(a,b);

   ret = fnt4(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(55);
      if ( b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(59);
   }

/* Test 5 */

   initarr2d(aa,bb);

   ret = fnt5(aa[0],&bb[0][0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(63);
         if ( bb[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(67);
      }
   }

/* Test 6 */

   initarr2d(aa,bb);

   ret = fnt6(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(71);
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(75);
      }
   }

/* Test 6a */

   initarr2d(aa,bb);

   ret = fnt6a(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(79);
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(83);
      }
   }

/* Test 7 */

   initarr2d(aa,bb);

   ret = fnt7(&aa[0][0],bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(87);
         if ( bb[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(91);
      }
   }

/* Test 8 */

   initarr2d(aa,bb);

   ret = fnt8(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(95);
         if ( bb[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(99);
      }
   }

   return 0;

}

void initarr1d(float _Complex *x, double _Complex *y) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x[i] = createcomplexf((float)(i+1),(float)(i+1));
   }

   for ( i = 0; i < 5; i++ ) {
      y[i] = createcomplex((double)(i+1),(double)(i+1));
   }

}

void initarr2d(float _Complex xx[][10], double _Complex yy[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         xx[i][j] = createcomplexf((float)(i+j+1),(float)(i+j+1));
      }
   }

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         yy[i][j] = createcomplex((double)(i+j+1),(double)(i+j+1));
      }
   }

}

