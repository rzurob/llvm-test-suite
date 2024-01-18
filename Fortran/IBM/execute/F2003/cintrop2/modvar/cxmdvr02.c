#include <stdio.h>
#include "cmplx.h"
#include <math.h>

struct dt {
  
  float _Complex a;
 
  double _Complex b;
};

int testf(float _Complex x)
{   
   if (fabs(crealf(x)) > 0.0001f) return 1; 
   if (fabs(cimagf(x) - 1.0f) > 0.0001f) return 1;
   return 0;
}

int testd(double _Complex x)
{  
   if ( fabs(creal(x)) > 0.000000000001 ) return 1;
   if ( fabs(cimag(x) - 1.0) > 0.000000000001) return 1;
   return 0;
}


void csub(struct dt * x)
{
  
  if ( testf(x->a) ) exit(60);
  
  if ( testd(x->b) ) exit(61);
  
  
  x->a = createcomplexf(1.0f, 3.0f);
  
  x->b = createcomplex(1.0, 3.0);
  return;
 
}
 