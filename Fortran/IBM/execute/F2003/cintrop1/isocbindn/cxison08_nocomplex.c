
/*
        C code for testcase "fxison08.f" and "fxison09.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

float _Complex fnt1(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(23);
      a[i] = createcomplexf((float)(i+2),(float)(i+2));
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(27);
      b[i] = createcomplex((double)(i+2),(double)(i+2));
   }

   return 0;
}

float _Complex fnt2(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(31);
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(35);
   }

   return 0;
}

float _Complex fnt2a(const float _Complex *a, const double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(39);
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(43);
   }

   return 0;
}

float _Complex fnt3(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(47);
      a[i] = createcomplexf((float)(i+2),(float)(i+2));
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(51);
      b[i] = createcomplex((double)(i+2),(double)(i+2));
   }

   return 0;
}

float _Complex fnt4(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      a[i] = createcomplexf((float)(i+2),(float)(i+2));
      b[i] = createcomplex((double)(i+2),(double)(i+2));
   }

   return 0;
}

float _Complex fnt5(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(55);
         aa[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(59);
         bb[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

   return 0;
}

float _Complex fnt6(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(63);
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(67);
      }
   }

   return 0;
}

float _Complex fnt6a(const float _Complex aa[][10], const double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(71);
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(75);
      }
   }

   return 0;
}

float _Complex fnt7(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(79);
         aa[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(83);
         bb[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

   return 0;
}

float _Complex fnt8(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         aa[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         bb[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

   return 0;
}
