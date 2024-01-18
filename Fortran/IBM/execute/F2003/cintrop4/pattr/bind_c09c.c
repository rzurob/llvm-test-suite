#include <stdio.h>
#include <math.h>
#include "cmplx.h"

 void extsub_int(signed char *i, signed short *j, signed int *k, signed long long *l, signed int *b);
     

  void extsub_real(float *i, double *j, signed int *b);

  void extsub_char(char *i, int *a);
   
  void extsub_comp(float _Complex *a, double _Complex *b, int *i); 

int testf(float x, float y)
{
   if ( (2*fabs(x - y)) > (x + y) * 0.0001f) return 1;
     else return 0 ;
}

int testd(double x, double y)
{
   if ( (2*fabs(x - y)) > (x + y) * 0.000000000001 ) return 1;
    else return 0;
}


int main() {

signed char ai1 = 5;
signed short ai2 = 15;
signed int ai4 = 11;
signed long long ai8 = 17;

float r4 = 4.8f;
double r8 = 48.0;

char ch = 'f';

float _Complex co8;
double _Complex co16;
co8 = createcomplexf(3.0f, 6.0f);
co16 = createcomplex(3.0, 6.0);

int a = 3;

extsub_int(&ai1, &ai2, &ai4, &ai8, &a);

if ( ai1 != 8) exit(10);
if ( ai2 != 18 ) exit (11);
if ( ai4 != 14 ) exit (12);
if ( ai8 != 20 ) exit (13);

a = 3;

extsub_real(&r4, &r8, &a);

if ( testf(r4, 1.80) ) exit(20);
if ( testd(r8, 45.0) ) exit(21);
a = 3;
extsub_char(&ch, &a);

if ( ch != 'd') exit(30);

a = 3;

extsub_comp(&co8, &co16, &a);

if ( testf( crealf(co8), 0.0f) || testf(cimagf(co8), 0.0f) )
  exit(40);
if ( testd( creal(co16), 0.0) || testd(cimag(co16), 0.0) )
  exit(41); 

return 0;

}




