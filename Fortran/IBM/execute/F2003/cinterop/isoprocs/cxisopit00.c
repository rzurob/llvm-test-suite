
/*
        C code for testcase "fxisopit00.f"
*/

#include <stdio.h>
#include <stdlib.h>

#define DIM1 4
#define DIM2 3

void sub1(signed char **p) {

   if ( **p != 'A' ) exit(21);

   *p = malloc(sizeof(**p));
   **p = 'C';

}

void sub2(signed char **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( *(*p+i) != 'A'+i ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      *(*p+i) = 'A'+i+4;
   }

}

void sub3(signed char **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( *((*p+i*DIM2)+j) != 'A'+i+j ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         *((*p+i*DIM2)+j) = 'J'+i+j;
      }
   }

}

