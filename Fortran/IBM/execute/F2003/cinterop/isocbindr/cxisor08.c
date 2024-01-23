
/*
        C code for testcase "fxisor08.f" and "fxisor09.f"
*/

#include <stdio.h>
#include <stdlib.h>


_Bool fnt1(_Bool *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 1 ) exit(21);
      a[i] = 0;
   }

   return 0;
}

_Bool fnt2(_Bool *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 1 ) exit(23);
   }

   return 0;
}

_Bool fnt2a(const _Bool *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 1 ) exit(25);
   }

   return 0;
}

_Bool fnt3(_Bool *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != 1 ) exit(27);
      a[i] = 0;
   }

   return 0;
}

_Bool fnt4(_Bool *a) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = 0;
   }

   return 0;
}

_Bool fnt5(_Bool aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 1 ) exit(29);
         aa[i][j] = 0;
      }
   }

   return 0;
}

_Bool fnt6(_Bool aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 1 ) exit(31);
      }
   }

   return 0;
}

_Bool fnt6a(const _Bool aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 1 ) exit(33);
      }
   }

   return 0;
}

_Bool fnt7(_Bool aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != 1 ) exit(35);
         aa[i][j] = 0;
      }
   }

   return 0;
}

_Bool fnt8(_Bool aa[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = 0;
      }
   }

   return 0;
}
