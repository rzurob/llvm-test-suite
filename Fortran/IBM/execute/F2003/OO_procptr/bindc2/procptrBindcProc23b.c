
#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h" 
#endif

void csub(float _Complex **p) {

#ifdef CMPLX
   if ( **p != 5.0f+I*5.0f ) exit(41);
#else
   if ( **p != createcomplexf(5.0f,5.0f) ) exit(42);
#endif

   *p = malloc(sizeof(**p));
#ifdef CMPLX
   **p = 10.0f+I*10.0f;
#else
   **p = createcomplexf(10.0f,10.0f);
#endif

}

float _Complex * cfunc(float _Complex **p) {

#ifdef CMPLX
   if ( **p != 5.0f+I*5.0f ) exit(43);
#else
   if ( **p != createcomplexf(5.0f,5.0f) ) exit(44);
#endif

   *p = malloc(sizeof(**p));

#ifdef CMPLX
   **p = 10.0f+I*10.0f;
#else
   **p = createcomplexf(10.0f,10.0f);
#endif

   return *p;
}
