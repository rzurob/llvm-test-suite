
/*
        C code for testcase "fxisopla02.f"
*/

#include <stdio.h>
#include <stdlib.h>


#define DIM1 4
#define DIM2 3
#define DIM3 2

struct dts0 {
   _Bool a;
};

struct dts1 {
   _Bool b[DIM1];
};

struct dts2 {
   _Bool c[DIM2][DIM2];
};

void sub1(struct dts0 **p) {
   int i;

   for ( i = 0; i < DIM3; i++ ) {
      if ( (*p+i)->a != 1 ) exit(21);
   }

   *p = malloc(sizeof(**p)*DIM3);

   for ( i = 0; i < DIM3; i++ ) {
      (*p+i)->a = 0;
   }
}

void sub2(struct dts1 **p) {
   int i, j;

   for ( i = 0; i < DIM3; i++ ) {
      for ( j = 0; j < DIM1; j++ ) {
         if ( (*p)->b[i*DIM1+j] != 1 ) exit(23);
      }
   }

   *p = malloc(sizeof(**p)*DIM1*DIM3);

   for ( i = 0; i < DIM3; i++ ) {
      for ( j = 0; j < DIM1; j++ ) {
         (*p)->b[i*DIM1+j] = 0;
      }
   }
}

void sub3(struct dts2 **p) {
   int i, j, k, l;

   for ( i = 0; i < DIM3; i++ )
      for ( j = 0; j < DIM3; j++ )
         for ( k = 0; k < DIM2; k++ )
            for ( l = 0; l < DIM2; l++ )
               if ( (*p+i*DIM3+j)->c[k][l] != 1 ) exit(25);

   *p = malloc(sizeof(**p)*DIM2*DIM2*DIM3*DIM3);

   for ( i = 0; i < DIM3; i++ )
      for ( j = 0; j < DIM3; j++ )
         for ( k = 0; k < DIM2; k++ )
            for ( l = 0; l < DIM2; l++ )
               (*p+i*DIM3+j)->c[k][l] = 0;
}
