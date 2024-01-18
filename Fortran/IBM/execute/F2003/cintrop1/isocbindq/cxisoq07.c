
/*
	C code for testcase "fxisoq06.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

void initarr1d(long double _Complex *);
void initarr2d(long double _Complex [][10]);

int main() {

   void sub1(long double _Complex *);
   void sub2(long double _Complex *);
   void sub2a(const long double _Complex *);
   void sub3(long double _Complex *);
   void sub4(long double _Complex *);
   void sub5(long double _Complex []);
   void sub6(long double _Complex []);
   void sub6a(const long double _Complex []);
   void sub7(long double _Complex []);
   void sub8(long double _Complex []);

   long double _Complex a[5], aa[5][10], ret;
   int i, j;

/* Test 1 */

   initarr1d(a);

   sub1(&a[0]);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (long double)(i+2)+I*(long double)(i+2) ) exit(21);
#else
      if ( a[i] != createcomplexl((long double)(i+2),(long double)(i+2)) ) exit(23);
#endif
   }

/* Test 2 */

   initarr1d(a);

   sub2(a);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(25);
#else
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(27);
#endif
   }

/* Test 2a */

   initarr1d(a);

   sub2a(a);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(29);
#else
      if ( a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(31);
#endif
   }

/* Test 3 */

   initarr1d(a);

   sub3(&a[0]);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (long double)(i+2)+I*(long double)(i+2) ) exit(33);
#else
      if ( a[i] != createcomplexl((long double)(i+2),(long double)(i+2)) ) exit(35);
#endif
   }

/* Test 4 */

   initarr1d(a);

   sub4(&a[0]);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (long double)(i+2)+I*(long double)(i+2) ) exit(37);
#else
      if ( a[i] != createcomplexl((long double)(i+2),(long double)(i+2)) ) exit(39);
#endif
   }

/* Test 5 */

   initarr2d(aa);

   sub5(aa[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(41);
#else
         if ( aa[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(43);
#endif
      }
   }

/* Test 6 */

   initarr2d(aa);

   sub6(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(45);
#else
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(47);
#endif
      }
   }

/* Test 6a */

   initarr2d(aa);

   sub6a(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(49);
#else
         if ( aa[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(51);
#endif
      }
   }

/* Test 7 */

   initarr2d(aa);

   sub7(&aa[0][0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(53);
#else
         if ( aa[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(55);
#endif
      }
   }

/* Test 8 */

   initarr2d(aa);

   sub8(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(57);
#else
         if ( aa[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(59);
#endif
      }
   }

   return 0;

}

void initarr1d(long double _Complex *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      x[i] = (long double)(i+1)+I*(long double)(i+1);
#else
      x[i] = createcomplexl((long double)(i+1),(long double)(i+1));
#endif
   }


}

void initarr2d(long double _Complex xx[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         xx[i][j] = (long double)(i+j+1)+I*(long double)(i+j+1);
#else
         xx[i][j] = createcomplexl((long double)(i+j+1),(long double)(i+j+1));
#endif
      }
   }


}

