
/*
        C code for testcase "fxisopxa00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

#define DIM1 4
#define DIM2 3

void sub1(float _Complex **p) {

   if ( **p != createcomplexf(5.0f,5.0f) ) exit(21);

   *p = malloc(sizeof(**p));
   **p = createcomplexf(10.0f,10.0f);

}

void sub2(float _Complex **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( *(*p+i) != createcomplexf((float)(i+1),(float)(i+1)) ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      *(*p+i) = createcomplexf((float)(i+2),(float)(i+2));
   }

}

void sub3(float _Complex **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( *((*p+i*DIM2)+j) != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         *((*p+i*DIM2)+j) = createcomplexf((float)(i+j+3),(float)(i+j+3));
      }
   }

}

