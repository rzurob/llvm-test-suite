
/*
        C code for testcase "fxisopxb01.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

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

   if ( (*p)->a != createcomplex(5.0,5.0) ) exit(21);
   *p = malloc(sizeof(**p));
   (*p)->a = createcomplex(10.0,10.0);

}

void sub2(struct dts1 **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( (*p)->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      (*p)->b[i] = createcomplex((double)(i+2),(double)(i+2));
   }

}

void sub3(struct dts2 **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( (*p)->c[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         (*p)->c[i][j] = createcomplex((double)(i+j+3),(double)(i+j+3));
      }
   }

}
