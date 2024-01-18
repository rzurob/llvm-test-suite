
/*
	C code for testcase "fxisok06.f"
*/

#include <stdio.h>
#include <stdlib.h>

void initarr1d(char *, signed char *);
void initarr2d(char [][6], signed char [][6]);

int main() {

   void sub1(char *, signed char *);
   void sub2(char *, signed char *);
   void sub2a(const char *, const signed char *);
   void sub3(char *, signed char *);
   void sub4(char *, signed char *);
   void sub5(char [], signed char []);
   void sub6(char [], signed char []);
   void sub6a(const char [], const signed char []);
   void sub7(char [], signed char []);
   void sub8(char [], signed char []);

   char a[4], aa[4][6], ret;
   signed char b[4], bb[4][6];
   int i, j;

/* Test 1 */

   initarr1d(a,b);

   sub1(&a[0],&b[0]);

   for ( i = 0; i < 4; i++ ) {
      if ( a[i] != 'A'+i+4 ) exit(21);
      if ( b[i] != 'A'+i+4 ) exit(23);
   }

/* Test 2 */

   initarr1d(a,b);

   sub2(a,&b[0]);

   for ( i = 0; i < 4; i++ ) {
      if ( a[i] != 'A'+i ) exit(25);
      if ( b[i] != 'A'+i ) exit(27);
   }

/* Test 2a */

   initarr1d(a,b);

   sub2a(a,&b[0]);

   for ( i = 0; i < 4; i++ ) {
      if ( a[i] != 'A'+i ) exit(29);
      if ( b[i] != 'A'+i ) exit(31);
   }

/* Test 3 */

   initarr1d(a,b);

   sub3(&a[0],&b[0]);

   for ( i = 0; i < 4; i++ ) {
      if ( a[i] != 'A'+i+4 ) exit(33);
      if ( b[i] != 'A'+i+4 ) exit(35);
   }

/* Test 4 */

   initarr1d(a,b);

   sub4(&a[0],&b[0]);

   for ( i = 0; i < 4; i++ ) {
      if ( a[i] != 'A'+i+4 ) exit(37);
      if ( b[i] != 'A'+i+4 ) exit(39);
   }

/* Test 5 */

   initarr2d(aa,bb);

   sub5(aa[0],&bb[0][0]);

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( aa[i][j] != 'A'+i*6+j+1 ) exit(41);
         if ( bb[i][j] != 'A'+i*6+j+1 ) exit(43);
      }
   }

/* Test 6 */

   initarr2d(aa,bb);

   sub6(*aa,bb[0]);

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( aa[i][j] != 'A'+i*6+j ) exit(45);
         if ( bb[i][j] != 'A'+i*6+j ) exit(47);
      }
   }

/* Test 6a */

   initarr2d(aa,bb);

   sub6a(*aa,bb[0]);

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( aa[i][j] != 'A'+i*6+j ) exit(49);
         if ( bb[i][j] != 'A'+i*6+j ) exit(51);
      }
   }

/* Test 7 */

   initarr2d(aa,bb);

   sub7(&aa[0][0],bb[0]);

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( aa[i][j] != 'A'+i*6+j+1 ) exit(53);
         if ( bb[i][j] != 'A'+i*6+j+1 ) exit(55);
      }
   }

/* Test 8 */

   initarr2d(aa,bb);

   sub8(*aa,bb[0]);

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( aa[i][j] != 'A'+i*6+j+1 ) exit(57);
         if ( bb[i][j] != 'A'+i*6+j+1 ) exit(59);
      }
   }

   return 0;

}

void initarr1d(char *x, signed char *y) {
   int i;

   for ( i = 0; i < 4; i++ ) {
      x[i] = 'A'+i;
   }

   for ( i = 0; i < 4; i++ ) {
      y[i] = 'A'+i;
   }

}

void initarr2d(char xx[][6], signed char yy[][6]) {
   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         xx[i][j] = 'A'+i*6+j;
      }
   }

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         yy[i][j] = 'A'+i*6+j;
      }
   }

}

