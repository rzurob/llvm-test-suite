
/*
        C code for testcase "fxisopxb02.f"
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
#define DIM3 2

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
   int i;

   for ( i = 0; i < DIM3; i++ ) {
#ifdef CMPLX
      if ( (*p+i)->a != 5.0+I*5.0 ) exit(21);
#else
      if ( (*p+i)->a != createcomplex(5.0,5.0) ) exit(21);
#endif
   }

   *p = malloc(sizeof(**p)*DIM3);

   for ( i = 0; i < DIM3; i++ ) {
#ifdef CMPLX
      (*p+i)->a = 10.0+I*10.0;
#else
      (*p+i)->a = createcomplex(10.0,10.0);
#endif
   }
}

void sub2(struct dts1 **p) {
   int i, j;

   for ( i = 0; i < DIM3; i++ ) {
      for ( j = 0; j < DIM1; j++ ) {
#ifdef CMPLX
         if ( (*p)->b[i*DIM1+j] != (double)(j+1)+I*(double)(j+1) ) exit(23);
#else
         if ( (*p)->b[i*DIM1+j] != createcomplex((double)(j+1),(double)(j+1)) ) exit(23);
#endif
      }
   }

   *p = malloc(sizeof(**p)*DIM1*DIM3);

   for ( i = 0; i < DIM3; i++ ) {
      for ( j = 0; j < DIM1; j++ ) {
#ifdef CMPLX
         (*p)->b[i*DIM1+j] = (double)(j+2)+I*(double)(j+2);
#else
         (*p)->b[i*DIM1+j] = createcomplex((double)(j+2),(double)(j+2));
#endif
      }
   }
}

void sub3(struct dts2 **p) {
   int i, j, k, l;

   for ( i = 0; i < DIM3; i++ )
      for ( j = 0; j < DIM3; j++ )
         for ( k = 0; k < DIM2; k++ )
            for ( l = 0; l < DIM2; l++ )
#ifdef CMPLX
               if ( (*p+i*DIM3+j)->c[k][l] != (double)(k+l+2)+I*(double)(k+l+2) ) exit(25);
#else
               if ( (*p+i*DIM3+j)->c[k][l] != createcomplex((double)(k+l+2),(double)(k+l+2)) ) exit(25);
#endif

   *p = malloc(sizeof(**p)*DIM2*DIM2*DIM3*DIM3);

   for ( i = 0; i < DIM3; i++ )
      for ( j = 0; j < DIM3; j++ )
         for ( k = 0; k < DIM2; k++ )
            for ( l = 0; l < DIM2; l++ )
#ifdef CMPLX
               (*p+i*DIM3+j)->c[k][l] = (double)(k+l+3)+I*(double)(k+l+3);
#else
               (*p+i*DIM3+j)->c[k][l] = createcomplex((double)(k+l+3),(double)(k+l+3));
#endif
}
