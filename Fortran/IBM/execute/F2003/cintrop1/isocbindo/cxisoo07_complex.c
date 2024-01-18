
/*
C code for testcase "fxisoo06.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

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
      if ( a[i] != (long double)(i+2)+I*(long double)(i+2) ) exit(21);
   }

/* Test 2 */

   initarr1d(a);

   sub2(a);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(25);
   }

/* Test 2a */

   initarr1d(a);

   sub2a(a);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(29);
   }

/* Test 3 */

   initarr1d(a);

   sub3(&a[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+2)+I*(long double)(i+2) ) exit(33);
   }

/* Test 4 */

   initarr1d(a);

   sub4(&a[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+2)+I*(long double)(i+2) ) exit(37);
   }

/* Test 5 */

   initarr2d(aa);

   sub5(aa[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(41);
      }
   }

/* Test 6 */

   initarr2d(aa);

   sub6(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(45);
      }
   }

/* Test 6a */

   initarr2d(aa);

   sub6a(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(49);
      }
   }

/* Test 7 */

   initarr2d(aa);

   sub7(&aa[0][0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(53);
      }
   }

/* Test 8 */

   initarr2d(aa);

   sub8(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(57);
      }
   }

   return 0;

}

void initarr1d(long double _Complex *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x[i] = (long double)(i+1)+I*(long double)(i+1);
   }


}

void initarr2d(long double _Complex xx[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         xx[i][j] = (long double)(i+j+1)+I*(long double)(i+j+1);
      }
   }


}

