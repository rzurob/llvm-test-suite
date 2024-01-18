
/*
        C code for testcase "fxisok08.f" and "fxisok09.f"
*/

#include <stdio.h>
#include <stdlib.h>

char fnt1(char *a, signed char *b) {
   int i;

   for ( i = 0; i < 4; i++ ) {
      if ( a[i] != 'A'+i ) exit(21);
      a[i] = 'A'+i+4;
      if ( b[i] != 'A'+i ) exit(23);
      b[i] = 'A'+i+4;
   }

   return 0;
}

char fnt2(char *a, signed char *b) {
   int i;

   for ( i = 0; i < 4; i++ ) {
      if ( a[i] != 'A'+i ) exit(25);
      if ( b[i] != 'A'+i ) exit(27);
   }

   return 0;
}

char fnt2a(const char *a, const signed char *b) {
   int i;

   for ( i = 0; i < 4; i++ ) {
      if ( a[i] != 'A'+i ) exit(29);
      if ( b[i] != 'A'+i ) exit(31);
   }

   return 0;
}

char fnt3(char *a, signed char *b) {
   int i;

   for ( i = 0; i < 4; i++ ) {
      if ( a[i] != 'A'+i ) exit(33);
      a[i] = 'A'+i+4;
      if ( b[i] != 'A'+i ) exit(35);
      b[i] = 'A'+i+4;
   }

   return 0;
}

char fnt4(char *a, signed char *b) {
   int i;

   for ( i = 0; i < 4; i++ ) {
      a[i] = 'A'+i+4;
      b[i] = 'A'+i+4;
   }

   return 0;
}

char fnt5(char aa[][6], signed char bb[][6]) {
   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( aa[i][j] != 'A'+i*6+j ) exit(37);
         aa[i][j] = 'A'+i*6+j+1;
         if ( bb[i][j] != 'A'+i*6+j ) exit(39);
         bb[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}

char fnt6(char aa[][6], signed char bb[][6]) {
   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( aa[i][j] != 'A'+i*6+j ) exit(41);
         if ( bb[i][j] != 'A'+i*6+j ) exit(43);
      }
   }

   return 0;
}

char fnt6a(const char aa[][6], const signed char bb[][6]) {
   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( aa[i][j] != 'A'+i*6+j ) exit(45);
         if ( bb[i][j] != 'A'+i*6+j ) exit(47);
      }
   }

   return 0;
}

char fnt7(char aa[][6], signed char bb[][6]) {
   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( aa[i][j] != 'A'+i*6+j ) exit(49);
         aa[i][j] = 'A'+i*6+j+1;
         if ( bb[i][j] != 'A'+i*6+j ) exit(51);
         bb[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}

char fnt8(char aa[][6], signed char bb[][6]) {
   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         aa[i][j] = 'A'+i*6+j+1;
         bb[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}
