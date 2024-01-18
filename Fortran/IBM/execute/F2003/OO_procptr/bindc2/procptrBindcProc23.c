
#include <stdio.h>
#include <stdlib.h>

#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

void * cfunc(void *p) {

   float _Complex *a;

   a = malloc(sizeof(float _Complex));

   if(!(a=malloc(sizeof(float _Complex)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = * (float _Complex *) p;

#ifdef CMPLX
   if ( *a != 5.0f+I*5.0f ) exit(41);
#else
   if ( *a != createcomplexf(5.0f,5.0f) ) exit(42);
#endif

#ifdef CMPLX
   *a = 10.0f+I*10.0f;
#else
   *a = createcomplexf(10.0f,10.0f);
#endif

   p = a;

   return a; 

}
