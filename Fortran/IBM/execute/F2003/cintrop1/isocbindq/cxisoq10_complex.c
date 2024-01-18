
/*
        C code for testcase "fxisoq10.f" and "fxisoq11.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

void sub1(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(21);
      a[i] = (long double)(i+2)+I*(long double)(i+2);
   }

}

void sub2(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(25);
   }

}

void sub2a(const long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(29);
   }

}

void sub3(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(33);
      a[i] = (long double)(i+2)+I*(long double)(i+2);
   }

}

void sub4(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = (long double)(i+2)+I*(long double)(i+2);
   }

}

void sub5(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(37);
         aa[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }

}

void sub6(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(41);
      }
   }

}

void sub6a(const long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(45);
      }
   }

}

void sub7(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(49);
         aa[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }

}

void sub8(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }

}
