
/*
        C code for testcase "fxisopxb00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

#define DIM1 4
#define DIM2 3

void sub1(double _Complex **p) {

   if ( **p != 5.0+I*5.0 ) exit(21);

   *p = malloc(sizeof(**p));
   **p = 10.0+I*10.0;

}

void sub2(double _Complex **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
      if ( *(*p+i) != (double)(i+1)+I*(double)(i+1) ) exit(23);
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
      *(*p+i) = (double)(i+2)+I*(double)(i+2);
   }

}

void sub3(double _Complex **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         if ( *((*p+i*DIM2)+j) != (double)(i+j+2)+I*(double)(i+j+2) ) exit(25);
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
         *((*p+i*DIM2)+j) = (double)(i+j+3)+I*(double)(i+j+3);
      }
   }

}

