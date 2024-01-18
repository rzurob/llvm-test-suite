
/*
        C code for testcase "fxisopic00.f"
*/

#include <stdio.h>
#include <stdlib.h>

#define DIM1 4
#define DIM2 3

void sub1(long **p) {

   if ( **p != 5 ) exit(21);

   *p = malloc(sizeof(**p));
   **p = 10;

}

void sub2(long **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( *(*p+i) != i+1 ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      *(*p+i) = i+2;
   }

}

void sub3(long **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( *((*p+i*DIM2)+j) != i+j+2 ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         *((*p+i*DIM2)+j) = i+j+3;
      }
   }

}

