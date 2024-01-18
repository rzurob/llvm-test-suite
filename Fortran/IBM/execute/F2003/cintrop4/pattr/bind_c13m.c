#include <stdio.h>
#include <math.h>
#include "cmplx.h"
#include <stdlib.h>

void extsub_real(long double *k);

void extsub_comp(long double _Complex *b);
   
int testl(long double x, long double y)
{
   if (fabs((x - y)/(x + y)) > 0.000000000000001l ) return 1;
   return 0;
}

int main() {

long double r16;

long double _Complex co32;

r16 = 1.0l;

co32 = createcomplexl(0.0l, 0.0l);

extsub_real(&r16);

if (testl(r16, 2.0l)) exit (10);

extsub_comp(&co32);

if (testl(creal(co32), 1.0l) || testl(cimag(co32), 1.0l) )
    exit(20);

return 0;

}




