
/*
        C code for testcase "fxisopxc00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

#define DIM1 4
#define DIM2 3

void sub1(long double _Complex **p) {

   if ( **p != 5.0l+I*5.0l ) exit(21);

   *p = malloc(sizeof(**p));
   **p = 10.0l+I*10.0l;

}

void sub2(long double _Complex **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( *(*p+i) != (long double)(i+1)+I*(long double)(i+1) ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      *(*p+i) = (long double)(i+2)+I*(long double)(i+2);
   }

}

void sub3(long double _Complex **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( *((*p+i*DIM2)+j) != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         *((*p+i*DIM2)+j) = (long double)(i+j+3)+I*(long double)(i+j+3);
      }
   }

}

