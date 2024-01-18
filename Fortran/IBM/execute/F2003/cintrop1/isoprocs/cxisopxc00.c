
/*
        C code for testcase "fxisopxc00.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

#define DIM1 4
#define DIM2 3

void sub1(long double _Complex **p) {

#ifdef CMPLX
   if ( **p != 5.0l+I*5.0l ) exit(21);
#else
   if ( **p != createcomplexl(5.0l,5.0l) ) exit(21);
#endif

   *p = malloc(sizeof(**p));
#ifdef CMPLX
   **p = 10.0l+I*10.0l;
#else
   **p = createcomplexl(10.0l,10.0l);
#endif

}

void sub2(long double _Complex **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
#ifdef CMPLX
      if ( *(*p+i) != (long double)(i+1)+I*(long double)(i+1) ) exit(23);
#else
      if ( *(*p+i) != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(23);
#endif
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
#ifdef CMPLX
      *(*p+i) = (long double)(i+2)+I*(long double)(i+2);
#else
      *(*p+i) = createcomplexl((long double)(i+2),(long double)(i+2));
#endif
   }

}

void sub3(long double _Complex **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
         if ( *((*p+i*DIM2)+j) != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(25);
#else
         if ( *((*p+i*DIM2)+j) != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(25);
#endif
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
         *((*p+i*DIM2)+j) = (long double)(i+j+3)+I*(long double)(i+j+3);
#else
         *((*p+i*DIM2)+j) = createcomplexl((long double)(i+j+3),(long double)(i+j+3));
#endif
      }
   }

}

