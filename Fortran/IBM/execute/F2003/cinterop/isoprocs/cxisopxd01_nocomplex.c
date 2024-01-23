
/*
        C code for testcase "fxisopxd01.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

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

   if ( (*p)->a != createcomplexl(5.0l,5.0l) ) exit(21);
   *p = malloc(sizeof(**p));
   (*p)->a = createcomplexl(10.0l,10.0l);

}

void sub2(struct dts1 **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( (*p)->b[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      (*p)->b[i] = createcomplexl((long double)(i+2),(long double)(i+2));
   }

}

void sub3(struct dts2 **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( (*p)->c[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         (*p)->c[i][j] = createcomplexl((long double)(i+j+3),(long double)(i+j+3));
      }
   }

}
