
/*
        C code for testcase "fxisoq08.f" and "fxisoq09.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

long double _Complex fnt1(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(21);
      a[i] = (long double)(i+2)+I*(long double)(i+2);
   }

   return 0;
}

long double _Complex fnt2(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(25);
   }

   return 0;
}

long double _Complex fnt2a(const long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(29);
   }

   return 0;
}

long double _Complex fnt3(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(33);
      a[i] = (long double)(i+2)+I*(long double)(i+2);
   }

   return 0;
}

long double _Complex fnt4(long double _Complex *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = (long double)(i+2)+I*(long double)(i+2);
   }

   return 0;
}

long double _Complex fnt5(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(37);
         aa[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }

   return 0;
}

long double _Complex fnt6(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(41);
      }
   }

   return 0;
}

long double _Complex fnt6a(const long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(45);
      }
   }

   return 0;
}

long double _Complex fnt7(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(49);
         aa[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }

   return 0;
}

long double _Complex fnt8(long double _Complex aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }

   return 0;
}
