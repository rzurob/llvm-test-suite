
/*
        C code for testcase "fxisopxa02.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

#define DIM1 4
#define DIM2 3
#define DIM3 2

struct dts0 {
   float _Complex a;
};

struct dts1 {
   float _Complex b[DIM1];
};

struct dts2 {
   float _Complex c[DIM2][DIM2];
};

void sub1(struct dts0 **p) {
   int i;

   for ( i = 0; i < DIM3; i++ ) {
      if ( (*p+i)->a != createcomplexf(5.0f,5.0f) ) exit(21);
   }

   *p = malloc(sizeof(**p)*DIM3);

   for ( i = 0; i < DIM3; i++ ) {
      (*p+i)->a = createcomplexf(10.0f,10.0f);
   }
}

void sub2(struct dts1 **p) {
   int i, j;

   for ( i = 0; i < DIM3; i++ ) {
      for ( j = 0; j < DIM1; j++ ) {
         if ( (*p)->b[i*DIM1+j] != createcomplexf((float)(j+1),(float)(j+1)) ) exit(23);
      }
   }

   *p = malloc(sizeof(**p)*DIM1*DIM3);

   for ( i = 0; i < DIM3; i++ ) {
      for ( j = 0; j < DIM1; j++ ) {
         (*p)->b[i*DIM1+j] = createcomplexf((float)(j+2),(float)(j+2));
      }
   }
}

void sub3(struct dts2 **p) {
   int i, j, k, l;

   for ( i = 0; i < DIM3; i++ )
      for ( j = 0; j < DIM3; j++ )
         for ( k = 0; k < DIM2; k++ )
            for ( l = 0; l < DIM2; l++ )
               if ( (*p+i*DIM3+j)->c[k][l] != createcomplexf((float)(k+l+2),(float)(k+l+2)) ) exit(25);

   *p = malloc(sizeof(**p)*DIM2*DIM2*DIM3*DIM3);

   for ( i = 0; i < DIM3; i++ )
      for ( j = 0; j < DIM3; j++ )
         for ( k = 0; k < DIM2; k++ )
            for ( l = 0; l < DIM2; l++ )
               (*p+i*DIM3+j)->c[k][l] = createcomplexf((float)(k+l+3),(float)(k+l+3));
}
