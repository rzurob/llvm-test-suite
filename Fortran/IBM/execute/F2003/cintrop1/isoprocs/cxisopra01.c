
/*
        C code for testcase "fxisopra01.f"
*/

#include <stdio.h>
#include <stdlib.h>


#define DIM1 4
#define DIM2 3

struct dts0 {
   float a;
};

struct dts1 {
   float b[DIM1];
};

struct dts2 {
   float c[DIM2][DIM2];
};

void sub1(struct dts0 **p) {

   if ( (*p)->a != 5 ) exit(21);
   *p = malloc(sizeof(**p));
   (*p)->a = 10;

}

void sub2(struct dts1 **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( (*p)->b[i] != (float)(i+1) ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      (*p)->b[i] = (float)(i+2);
   }

}

void sub3(struct dts2 **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( (*p)->c[i][j] != (float)(i+j+2) ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         (*p)->c[i][j] = (float)(i+j+3);
      }
   }

}
