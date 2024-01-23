
/*
        C code for testcase "fxisopxb00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

#define DIM1 4
#define DIM2 3

void sub1(double _Complex **p) {

   if ( **p != createcomplex(5.0,5.0) ) exit(21);

   *p = malloc(sizeof(**p));
   **p = createcomplex(10.0,10.0);

}

void sub2(double _Complex **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( *(*p+i) != createcomplex((double)(i+1),(double)(i+1)) ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      *(*p+i) = createcomplex((double)(i+2),(double)(i+2));
   }

}

void sub3(double _Complex **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( *((*p+i*DIM2)+j) != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         *((*p+i*DIM2)+j) = createcomplex((double)(i+j+3),(double)(i+j+3));
      }
   }

}

