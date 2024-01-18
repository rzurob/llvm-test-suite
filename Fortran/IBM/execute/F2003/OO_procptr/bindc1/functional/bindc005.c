#include <complex.h>


float _Complex fadd ( long double _Complex * c1 );
float _Complex fmul ( double _Complex * c1 );

float _Complex (* faddptr)  ( long double _Complex * );
float _Complex (* fmulptr ) ( double _Complex * );

void cset ( double _Complex * c1, double r1, double r2 )
{
   *c1 = r1 + r2 * I;
}
float _Complex cmul ( double _Complex * c1 )
{
   printf (" Inside cmul\n");
   fmulptr = fmul;

   return fmulptr(c1);
}

float _Complex cadd ( long double _Complex * c1 )
{
   printf (" Inside cadd\n");
   faddptr = fadd;

   return faddptr(c1);
}
