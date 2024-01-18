
#include <stdio.h>
#include <stdlib.h>

#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

void csub(void(**f)(float _Complex *, float _Complex *)) {

   extern void swap(float _Complex *, float _Complex *);

   *f = &swap;

}

void  swap(float _Complex *p1, float _Complex *p2) {

   float _Complex tmp;

#ifdef CMPLX
   if ( *p1 != 5.0f+I*5.0f ) exit(41);
#else
   if ( *p1 != createcomplexf(5.0f,5.0f) ) exit(42);
#endif

#ifdef CMPLX
   if ( *p2 != 6.0f+I*6.0f ) exit(43);
#else
   if ( *p2 != createcomplexf(6.0f,6.0f) ) exit(44);
#endif

   tmp = *p2;
   *p2 = *p1;
   *p1 = tmp;

}

