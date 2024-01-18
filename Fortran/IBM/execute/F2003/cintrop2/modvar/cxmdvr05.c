#include <stdio.h>
#include <math.h>
#include <stdlib.h>

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

void fsub(struct dt *);

int main()
{
  struct dt x;
  
  x.ch = 'a';
  x.a = 0.0f;
  x.b = 0.0;
  x.lo = 1;
  
  fsub(&x);

  if ( x.ch != 'b') exit(60);
  if ( testf(x.a - 2.0f) ) exit(61);
  if ( x.lo != 0) exit(62);
  if ( testd(x.b -2.0) ) exit(63);
  
  return 0;
 
}
 
