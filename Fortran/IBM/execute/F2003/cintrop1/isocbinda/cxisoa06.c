
/*
	C code for testcase "fxisoa06.f"
*/

#include <stdio.h>
#include <stdlib.h>


void initarr1d(int *, short *);
void initarr2d(int [][10], short [][10]);

int main() {

   int fnt1(int *, short *);
   int fnt2(int *, short *);
   int fnt2a(const int *, const short *);
   int fnt3(int *, short *);
   int fnt4(int *, short *);
   int fnt5(int [], short []);
   int fnt6(int [], short []);
   int fnt6a(const int [], const short []);
   int fnt7(int [], short []);
   int fnt8(int [], short []);

   int a[5], aa[5][10], ret;
   short b[5], bb[5][10];
   int i, j;

/* Test 1 */

   initarr1d(a,b);

   ret = fnt1(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+2 ) exit(21);
      if ( b[i] != i+2 ) exit(23);
   }

/* Test 2 */

   initarr1d(a,b);

   ret = fnt2(a,&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(25);
      if ( b[i] != i+1 ) exit(27);
   }

/* Test 2a */

   initarr1d(a,b);

   ret = fnt2a(a,&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+1 ) exit(29);
      if ( b[i] != i+1 ) exit(31);
   }

/* Test 3 */

   initarr1d(a,b);

   ret = fnt3(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+2 ) exit(33);
      if ( b[i] != i+2 ) exit(35);
   }

/* Test 4 */

   initarr1d(a,b);

   ret = fnt4(&a[0],&b[0]);

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != i+2 ) exit(37);
      if ( b[i] != i+2 ) exit(39);
   }

/* Test 5 */

   initarr2d(aa,bb);

   ret = fnt5(aa[0],&bb[0][0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+2 ) exit(41);
         if ( bb[i][j] != i+j+2 ) exit(43);
      }
   }

/* Test 6 */

   initarr2d(aa,bb);

   ret = fnt6(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(45);
         if ( bb[i][j] != i+j+1 ) exit(47);
      }
   }

/* Test 6a */

   initarr2d(aa,bb);

   ret = fnt6a(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+1 ) exit(49);
         if ( bb[i][j] != i+j+1 ) exit(51);
      }
   }

/* Test 7 */

   initarr2d(aa,bb);

   ret = fnt7(&aa[0][0],bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+2 ) exit(53);
         if ( bb[i][j] != i+j+2 ) exit(55);
      }
   }

/* Test 8 */

   initarr2d(aa,bb);

   ret = fnt8(*aa,bb[0]);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != i+j+2 ) exit(57);
         if ( bb[i][j] != i+j+2 ) exit(59);
      }
   }

   return 0;

}

void initarr1d(int *x, short *y) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x[i] = i+1;
   }

   for ( i = 0; i < 5; i++ ) {
      y[i] = i+1;
   }

}

void initarr2d(int xx[][10], short yy[][10]) {
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
