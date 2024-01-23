
/*
	C code for testcase "fxisor06.f"
*/

#include <stdio.h>
#include <stdlib.h>


void initarr1d(_Bool *);
void initarr2d(_Bool [][10]);

int main() {

   void sub1(_Bool *);
   void sub2(_Bool *);
   void sub2a(const _Bool *);
   void sub3(_Bool *);
   void sub4(_Bool *);
   void sub5(_Bool []);
   void sub6(_Bool []);
   void sub6a(const _Bool []);
   void sub7(_Bool []);
   void sub8(_Bool []);

   _Bool a[5], aa[5][10], ret;
   int i, j;

/* Test 1 */

   initarr1d(a);

   sub1(&a[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 0 ) exit(21);
   }

/* Test 2 */

   initarr1d(a);

   sub2(a);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 1 ) exit(23);
   }

/* Test 2a */

   initarr1d(a);

   sub2a(a);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 1 ) exit(25);
   }

/* Test 3 */

   initarr1d(a);

   sub3(&a[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 0 ) exit(27);
   }

/* Test 4 */

   initarr1d(a);

   sub4(&a[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 0 ) exit(29);
   }

/* Test 5 */

   initarr2d(aa);

   sub5(aa[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 0 ) exit(31);
      }
   }

/* Test 6 */

   initarr2d(aa);

   sub6(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 1 ) exit(33);
      }
   }

/* Test 6a */

   initarr2d(aa);

   sub6a(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 1 ) exit(35);
      }
   }

/* Test 7 */

   initarr2d(aa);

   sub7(&aa[0][0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 0 ) exit(37);
      }
   }

/* Test 8 */

   initarr2d(aa);

   sub8(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 0 ) exit(39);
      }
   }

   return 0;

}

void initarr1d(_Bool *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x[i] = 1;
   }


}

void initarr2d(_Bool xx[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         xx[i][j] = i+j+1;
      }
   }


}

