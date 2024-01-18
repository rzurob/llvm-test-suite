
#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h" 
#endif

void csub(double _Complex **p) {

#ifdef CMPLX
   if ( **p != 5.0+I*5.0 ) exit(41);
#else
   if ( **p != createcomplex(5.0,5.0) ) exit(42);
#endif

   *p = malloc(sizeof(**p));
#ifdef CMPLX
   **p = 10.0+I*10.0;
#else
   **p = createcomplex(10.0,10.0);
#endif

}

double _Complex * cfunc(double _Complex **p) {

#ifdef CMPLX
   if ( **p != 5.0+I*5.0 ) exit(43);
#else
   if ( **p != createcomplex(5.0,5.0) ) exit(44);
#endif

   *p = malloc(sizeof(**p));

#ifdef CMPLX
   **p = 10.0+I*10.0;
#else
   **p = createcomplex(10.0,10.0);
#endif

   return *p;
}
