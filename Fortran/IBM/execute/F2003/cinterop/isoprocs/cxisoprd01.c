
/*
        C code for testcase "fxisoprd01.f"
*/

#include <stdio.h>
#include <stdlib.h>


#define DIM1 4
#define DIM2 3

struct dts0 {
   long double a;
};

struct dts1 {
   long double b[DIM1];
};

struct dts2 {
   long double c[DIM2][DIM2];
};

void sub1(struct dts0 **p) {

   if ( (*p)->a != 5.0l ) exit(21);
   *p = malloc(sizeof(**p));
   (*p)->a = 10.0l;

}

void sub2(struct dts1 **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( (*p)->b[i] != (long double)(i+1) ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      (*p)->b[i] = (long double)(i+2);
   }

}

void sub3(struct dts2 **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( (*p)->c[i][j] != (long double)(i+j+2) ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         (*p)->c[i][j] = (long double)(i+j+3);
      }
   }

}
