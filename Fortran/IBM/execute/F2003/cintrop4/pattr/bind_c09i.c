#include <stdio.h>
#include <math.h>
#include "cmplx.h"

 signed char exfun_int1(signed char *i);

 signed short exfun_int2(signed short *i);

 signed int exfun_int4(signed int *i);

 signed long long exfun_int8(signed long long *i);

 float exfun_real4(float *i, int *j);

 double exfun_real8(double *j, int *i);

 float _Complex exfun_comp1(float _Complex *i, int *j); 
  
 double _Complex exfun_comp2(double _Complex *i, int *j);

int testf(float x, float y)
{
   if ( (2 * fabs(x - y))/(x + y) > 0.0001f) return 1;
     else return 0 ;
}

int testd(double x, double y)
{
   if ( (2*fabs(x - y))/(x + y) > 0.000000000001 ) return 1;
    else return 0;
}


int main() {

signed char a1 = 5, a2 = 10, a3;
signed short b1 = 15, b2 = 30, b3;
signed int c1 = 11, c2 = 22, c3;
signed long long d1 = 17, d2 = 34, d3;

float e1 = 4.80f;
float e2 = 9.60f;
float e3;

double f1 = 140.8, f2 = 281.6, f3;

float _Complex l1, l2, l3;
double _Complex m1, m2, m3;

l1 = createcomplexf(0.0f, 0.0f);
l2 = createcomplexf(3.0f, 6.0f);

m1 = createcomplex(0.0, 0.0);
m2 = createcomplex(3.0, 6.0);

int i;

a3 = exfun_int1(&a1);
if ( a3 != a2 ) exit(10);

b3 = exfun_int2(&b1);
if ( b3 != b2 ) exit(11);

c3 = exfun_int4(&c1);
if ( c3 != c2 ) exit(12);

d3 = exfun_int8(&d1);
if ( d3 != d2 ) exit(13);

i = 1;

e3 = exfun_real4(&e1, &i);
if ( testf(e3, e2) ) exit(20);

i = 1;

f3 = exfun_real8(&f1, &i);
if ( testd(f3, f2) ) exit(21);

i = 3;

l3 = exfun_comp1(&l1, &i);
if ( testf(crealf(l3), crealf(l2)) ||
     testf(cimagf(l3), cimagf(l2)) ) 
  exit(50);

i = 3;
m3 = exfun_comp2(&m1, &i);

if ( testd(creal(m3), creal(m2)) ||
     testd(cimag(m3), cimag(m2)) )
    exit(60);

return 0;

}
 

