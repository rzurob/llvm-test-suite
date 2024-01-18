
/*
        C code for testcase "fxisopxd00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

#define DIM1 4
#define DIM2 3

void sub1(long double _Complex **p) {

   if ( **p != createcomplexl(5.0l,5.0l) ) exit(21);

   *p = malloc(sizeof(**p));
   **p = createcomplexl(10.0l,10.0l);

}

void sub2(long double _Complex **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( *(*p+i) != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      *(*p+i) = createcomplexl((long double)(i+2),(long double)(i+2));
   }

}

void sub3(long double _Complex **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( *((*p+i*DIM2)+j) != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         *((*p+i*DIM2)+j) = createcomplexl((long double)(i+j+3),(long double)(i+j+3));
      }
   }

}

