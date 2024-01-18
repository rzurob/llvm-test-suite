
/*
	C code for testcase "fxison06.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

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
#ifdef CMPLX
      if ( a[i] != (float)(i+2)+I*(float)(i+2) ) exit(21);
#else
      if ( a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(23);
#endif
#ifdef CMPLX
      if ( b[i] != (double)(i+2)+I*(double)(i+2) ) exit(25);
#else
      if ( b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(27);
#endif
   }

/* Test 2 */

   initarr1d(a,b);

   ret = fnt2(a,&b[0]);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(29);
#else
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(31);
#endif
#ifdef CMPLX
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(33);
#else
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(35);
#endif
   }

/* Test 2a */

   initarr1d(a,b);

   ret = fnt2a(a,&b[0]);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(37);
#else
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(39);
#endif
#ifdef CMPLX
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(41);
#else
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(43);
#endif
   }

/* Test 3 */

   initarr1d(a,b);

   ret = fnt3(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (float)(i+2)+I*(float)(i+2) ) exit(45);
#else
      if ( a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(47);
#endif
#ifdef CMPLX
      if ( b[i] != (double)(i+2)+I*(double)(i+2) ) exit(49);
#else
      if ( b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(51);
#endif
   }

/* Test 4 */

   initarr1d(a,b);

   ret = fnt4(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (float)(i+2)+I*(float)(i+2) ) exit(53);
#else
      if ( a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(55);
#endif
#ifdef CMPLX
      if ( b[i] != (double)(i+2)+I*(double)(i+2) ) exit(57);
#else
      if ( b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(59);
#endif
   }

/* Test 5 */

   initarr2d(aa,bb);

   ret = fnt5(aa[0],&bb[0][0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(61);
#else
         if ( aa[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(63);
#endif
#ifdef CMPLX
         if ( bb[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(65);
#else
         if ( bb[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(67);
#endif
      }
   }

/* Test 6 */

   initarr2d(aa,bb);

   ret = fnt6(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(69);
#else
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(71);
#endif
#ifdef CMPLX
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(73);
#else
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(75);
#endif
      }
   }

/* Test 6a */

   initarr2d(aa,bb);

   ret = fnt6a(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(77);
#else
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(79);
#endif
#ifdef CMPLX
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(81);
#else
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(83);
#endif
      }
   }

/* Test 7 */

   initarr2d(aa,bb);

   ret = fnt7(&aa[0][0],bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(85);
#else
         if ( aa[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(87);
#endif
#ifdef CMPLX
         if ( bb[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(89);
#else
         if ( bb[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(91);
#endif
      }
   }

/* Test 8 */

   initarr2d(aa,bb);

   ret = fnt8(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(93);
#else
         if ( aa[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(95);
#endif
#ifdef CMPLX
         if ( bb[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(97);
#else
         if ( bb[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(99);
#endif
      }
   }

   return 0;

}

void initarr1d(float _Complex *x, double _Complex *y) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      x[i] = (float)(i+1)+I*(float)(i+1);
#else
      x[i] = createcomplexf((float)(i+1),(float)(i+1));
#endif
   }

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      y[i] = (double)(i+1)+I*(double)(i+1);
#else
      y[i] = createcomplex((double)(i+1),(double)(i+1));
#endif
   }

}

void initarr2d(float _Complex xx[][10], double _Complex yy[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         xx[i][j] = (float)(i+j+1)+I*(float)(i+j+1);
#else
         xx[i][j] = createcomplexf((float)(i+j+1),(float)(i+j+1));
#endif
      }
   }

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         yy[i][j] = (double)(i+j+1)+I*(double)(i+j+1);
#else
         yy[i][j] = createcomplex((double)(i+j+1),(double)(i+j+1));
#endif
      }
   }

}

