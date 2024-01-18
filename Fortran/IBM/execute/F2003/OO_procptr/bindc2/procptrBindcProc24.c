
#include <stdio.h>
#include <stdlib.h>

#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

void * cfunc(void *p) {

   double _Complex *a;

   a = malloc(sizeof(double _Complex));

   if(!(a=malloc(sizeof(double _Complex)))){
      printf("Out of memory.\n");
      exit(40);
   }

   *a = * (double _Complex *) p;

#ifdef CMPLX
   if ( *a != 5.0+I*5.0 ) exit(41);
#else
   if ( *a != createcomplexf(5.0,5.0) ) exit(42);
#endif

#ifdef CMPLX
   *a = 10.0+I*10.0;
#else
   *a = createcomplexf(10.0,10.0);
#endif

   p = a;

   return a; 

}
