
/*
        C code for testcase "fxisopxb01.f"
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
   double _Complex a;
};

struct dts1 {
   double _Complex b[DIM1];
};

struct dts2 {
   double _Complex c[DIM2][DIM2];
};

void sub1(struct dts0 **p) {

#ifdef CMPLX
   if ( (*p)->a != 5.0+I*5.0 ) exit(21);
#else
   if ( (*p)->a != createcomplex(5.0,5.0) ) exit(21);
#endif
   *p = malloc(sizeof(**p));
#ifdef CMPLX
   (*p)->a = 10.0+I*10.0;
#else
   (*p)->a = createcomplex(10.0,10.0);
#endif

}

void sub2(struct dts1 **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
#ifdef CMPLX
      if ( (*p)->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(23);
#else
      if ( (*p)->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(23);
#endif
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
#ifdef CMPLX
      (*p)->b[i] = (double)(i+2)+I*(double)(i+2);
#else
      (*p)->b[i] = createcomplex((double)(i+2),(double)(i+2));
#endif
   }

}

void sub3(struct dts2 **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
         if ( (*p)->c[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(25);
#else
         if ( (*p)->c[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(25);
#endif
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
         (*p)->c[i][j] = (double)(i+j+3)+I*(double)(i+j+3);
#else
         (*p)->c[i][j] = createcomplex((double)(i+j+3),(double)(i+j+3));
#endif
      }
   }

}
