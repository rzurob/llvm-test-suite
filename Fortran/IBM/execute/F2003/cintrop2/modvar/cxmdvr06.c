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

void fsub(struct dt *);

int main()
{
  struct dt x;
  
  
  x.a = createcomplexf(0.0f, 1.0f);
  x.b = createcomplex(0.0 ,1.0);
  
  
  fsub(&x);

  
  if ( testf(createcomplexf(crealf(x.a ) - 1.0f, cimagf(x.a) -2.0f)) ) 
        exit(61);
  
  if ( testd(createcomplex(creal(x.b) - 1.0, cimag(x.b) - 2.0) )) 
        exit(63);
  
  return 0;
 
}
 