
/*
        C code for testcase "fxison08.f" and "fxison09.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

float _Complex fnt1(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(21);
#else
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(23);
#endif
#ifdef CMPLX
      a[i] = (float)(i+2)+I*(float)(i+2);
#else
      a[i] = createcomplexf((float)(i+2),(float)(i+2));
#endif
#ifdef CMPLX
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(25);
#else
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(27);
#endif
#ifdef CMPLX
      b[i] = (double)(i+2)+I*(double)(i+2);
#else
      b[i] = createcomplex((double)(i+2),(double)(i+2));
#endif
   }

   return 0;
}

float _Complex fnt2(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(29);
#else
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(31);
#endif
#ifdef CMPLX
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(33);
#else
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(35);
#endif
   }

   return 0;
}

float _Complex fnt2a(const float _Complex *a, const double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(37);
#else
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(39);
#endif
#ifdef CMPLX
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(41);
#else
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(43);
#endif
   }

   return 0;
}

float _Complex fnt3(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( a[i] != (float)(i+1)+I*(float)(i+1) ) exit(45);
#else
      if ( a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(47);
#endif
#ifdef CMPLX
      a[i] = (float)(i+2)+I*(float)(i+2);
#else
      a[i] = createcomplexf((float)(i+2),(float)(i+2));
#endif
#ifdef CMPLX
      if ( b[i] != (double)(i+1)+I*(double)(i+1) ) exit(49);
#else
      if ( b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(51);
#endif
#ifdef CMPLX
      b[i] = (double)(i+2)+I*(double)(i+2);
#else
      b[i] = createcomplex((double)(i+2),(double)(i+2));
#endif
   }

   return 0;
}

float _Complex fnt4(float _Complex *a, double _Complex *b) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      a[i] = (float)(i+2)+I*(float)(i+2);
#else
      a[i] = createcomplexf((float)(i+2),(float)(i+2));
#endif
#ifdef CMPLX
      b[i] = (double)(i+2)+I*(double)(i+2);
#else
      b[i] = createcomplex((double)(i+2),(double)(i+2));
#endif
   }

   return 0;
}

float _Complex fnt5(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(53);
#else
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(55);
#endif
#ifdef CMPLX
         aa[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
#else
         aa[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
#endif
#ifdef CMPLX
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(57);
#else
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(59);
#endif
#ifdef CMPLX
         bb[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
#else
         bb[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
#endif
      }
   }

   return 0;
}

float _Complex fnt6(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(61);
#else
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(63);
#endif
#ifdef CMPLX
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(65);
#else
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(67);
#endif
      }
   }

   return 0;
}

float _Complex fnt6a(const float _Complex aa[][10], const double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(69);
#else
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(71);
#endif
#ifdef CMPLX
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(73);
#else
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(75);
#endif
      }
   }

   return 0;
}

float _Complex fnt7(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( aa[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(77);
#else
         if ( aa[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(79);
#endif
#ifdef CMPLX
         aa[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
#else
         aa[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
#endif
#ifdef CMPLX
         if ( bb[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(81);
#else
         if ( bb[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(83);
#endif
#ifdef CMPLX
         bb[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
#else
         bb[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
#endif
      }
   }

   return 0;
}

float _Complex fnt8(float _Complex aa[][10], double _Complex bb[][10]) {
   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         aa[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
#else
         aa[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
#endif
#ifdef CMPLX
         bb[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
#else
         bb[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
#endif
      }
   }

   return 0;
}
