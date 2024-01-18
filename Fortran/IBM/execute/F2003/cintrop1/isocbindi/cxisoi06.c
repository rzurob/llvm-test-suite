
/*
	C code for testcase "fxisoi06.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

void initarr1d(int_fast16_t *);
void initarr2d(int_fast16_t [][10]);

int main() {

   int_fast16_t fnt1(int_fast16_t *);
   int_fast16_t fnt2(int_fast16_t *);
   int_fast16_t fnt2a(const int_fast16_t *);
   int_fast16_t fnt3(int_fast16_t *);
   int_fast16_t fnt4(int_fast16_t *);
   int_fast16_t fnt5(int_fast16_t []);
   int_fast16_t fnt6(int_fast16_t []);
   int_fast16_t fnt6a(const int_fast16_t []);
   int_fast16_t fnt7(int_fast16_t []);
   int_fast16_t fnt8(int_fast16_t []);

   int_fast16_t a[5], aa[5][10], ret;
   int i, j;

/* Test 1 */

   initarr1d(a);

   ret = fnt1(&a[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+2 ) exit(21);
   }

/* Test 2 */

   initarr1d(a);

   ret = fnt2(a);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(23);
   }

/* Test 2a */

   initarr1d(a);

   ret = fnt2a(a);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(25);
   }

/* Test 3 */

   initarr1d(a);

   ret = fnt3(&a[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+2 ) exit(27);
   }

/* Test 4 */

   initarr1d(a);

   ret = fnt4(&a[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+2 ) exit(29);
   }

/* Test 5 */

   initarr2d(aa);

   ret = fnt5(aa[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+2 ) exit(31);
      }
   }

/* Test 6 */

   initarr2d(aa);

   ret = fnt6(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(33);
      }
   }

/* Test 6a */

   initarr2d(aa);

   ret = fnt6a(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(35);
      }
   }

/* Test 7 */

   initarr2d(aa);

   ret = fnt7(&aa[0][0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+2 ) exit(37);
      }
   }

/* Test 8 */

   initarr2d(aa);

   ret = fnt8(*aa);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+2 ) exit(39);
      }
   }

   return 0;

}

void initarr1d(int_fast16_t *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x[i] = i+1;
   }


}

void initarr2d(int_fast16_t xx[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         xx[i][j] = i+j+1;
      }
   }


}

