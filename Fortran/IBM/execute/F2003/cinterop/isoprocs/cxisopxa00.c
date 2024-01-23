
/*
        C code for testcase "fxisopxa00.f"
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

void sub1(float _Complex **p) {

#ifdef CMPLX
   if ( **p != 5.0f+I*5.0f ) exit(21);
#else
   if ( **p != createcomplexf(5.0f,5.0f) ) exit(21);
#endif

   *p = malloc(sizeof(**p));
#ifdef CMPLX
   **p = 10.0f+I*10.0f;
#else
   **p = createcomplexf(10.0f,10.0f);
#endif

}

void sub2(float _Complex **p) {
   int i;

   for ( i = 0; i < DIM1; i++ ) {
#ifdef CMPLX
      if ( *(*p+i) != (float)(i+1)+I*(float)(i+1) ) exit(23);
#else
      if ( *(*p+i) != createcomplexf((float)(i+1),(float)(i+1)) ) exit(23);
#endif
   }

   *p = malloc(sizeof(**p)*DIM1);

   for ( i = 0; i < DIM1; i++ ) {
#ifdef CMPLX
      *(*p+i) = (float)(i+2)+I*(float)(i+2);
#else
      *(*p+i) = createcomplexf((float)(i+2),(float)(i+2));
#endif
   }

}

void sub3(float _Complex **p) {
   int i, j;

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
         if ( *((*p+i*DIM2)+j) != (float)(i+j+2)+I*(float)(i+j+2) ) exit(25);
#else
         if ( *((*p+i*DIM2)+j) != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(25);
#endif
      }
   }

   *p = malloc(sizeof(**p)*DIM2*DIM2);

   for ( i = 0; i < DIM2; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
         *((*p+i*DIM2)+j) = (float)(i+j+3)+I*(float)(i+j+3);
#else
         *((*p+i*DIM2)+j) = createcomplexf((float)(i+j+3),(float)(i+j+3));
#endif
      }
   }

}

