
/*
        C code for testcase "fxisopxd01.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

#define DIM1 4
#define DIM2 3

struct dts0 {
   long double _Complex a;
};

struct dts1 {
   long double _Complex b[DIM1];
};

struct dts2 {
   long double _Complex c[DIM2][DIM2];
};

void sub1(struct dts0 **p) {

#ifdef CMPLX
   if ( (*p)->a != 5.0l+I*5.0l ) exit(21);
#else
   if ( (*p)->a != createcomplexl(5.0l,5.0l) ) exit(21);
#endif
   *p = malloc(sizeof(**p));
#ifdef CMPLX
   (*p)->a = 10.0l+I*10.0l;
#else
   (*p)->a = createcomplexl(10.0l,10.0l);
#endif

}

void sub2(struct dts1 **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
#ifdef CMPLX
      if ( (*p)->b[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(23);
#else
      if ( (*p)->b[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(23);
#endif
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
#ifdef CMPLX
      (*p)->b[i] = (long double)(i+2)+I*(long double)(i+2);
#else
      (*p)->b[i] = createcomplexl((long double)(i+2),(long double)(i+2));
#endif
   }

}

void sub3(struct dts2 **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
         if ( (*p)->c[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(25);
#else
         if ( (*p)->c[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(25);
#endif
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
         (*p)->c[i][j] = (long double)(i+j+3)+I*(long double)(i+j+3);
#else
         (*p)->c[i][j] = createcomplexl((long double)(i+j+3),(long double)(i+j+3));
#endif
      }
   }

}
