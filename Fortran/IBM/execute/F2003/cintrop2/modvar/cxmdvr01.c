#include <stdio.h>
#include <math.h>

struct dt {
  char ch;
  float a;
  char lo;
  double b;
};

int testf(float x)
{   
   if (fabs(x) > 0.0001f) return 1; 
     else return 0 ;
}

int testd(double x)
{  
   if ( fabs(x) > 0.000000000001 ) return 1; 
    else return 0;
}

void csub(struct dt * x)
{
  if ( x->ch != 'a') exit(60);
  if ( testf(x->a) ) exit(61);
  if ( x->lo == 0) exit(62);
  if ( testd(x->b) ) exit(63);
  
  x->ch = 'b';
  x->a = 2.0f;
  x->lo = 0;
  x->b = 2.0;
  return;
 
}
 
