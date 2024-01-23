
/*
        C code for testcase "fxisopit01.f"
*/

#include <stdio.h>
#include <stdlib.h>

#define DIM1 4
#define DIM2 3

struct dts0 {
   signed char a;
};

struct dts1 {
   signed char b[DIM1];
};

struct dts2 {
   signed char c[DIM2][DIM2];
};

void sub1(struct dts0 **p) {

   if ( (*p)->a != 'A' ) exit(21);
   *p = malloc(sizeof(**p));
   (*p)->a = 'C';

}

void sub2(struct dts1 **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( (*p)->b[i] != 'A'+i ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      (*p)->b[i] = 'A'+i+4;
   }

}

void sub3(struct dts2 **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( (*p)->c[i][j] != 'A'+i+j ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         (*p)->c[i][j] = 'J'+i+j;
      }
   }

}
