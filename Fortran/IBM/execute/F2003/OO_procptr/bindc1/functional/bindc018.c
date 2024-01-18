#include <complex.h>

typedef int C_INT;
typedef short int C_SHORT_INT;
typedef long int C_LONG;

typedef float C_FLOAT;
typedef double C_DOUBLE;
typedef long double C_LONG_DOUBLE;

typedef _Bool C_BOOL;
typedef float _Complex C_FLOAT_COMPLEX;


C_INT pint (C_INT i1, C_SHORT_INT i2, C_LONG *i3 )
{
   printf("Inside pint: %d, %d, %d\n", i1, i2, *i3 );
   return i1+i2+*i3;
}

C_FLOAT preal (C_FLOAT i1, C_DOUBLE i2, C_LONG_DOUBLE *i3 )
{
   printf("Inside preal: %f, %f, %Lf\n", i1, i2, *i3 );
   return i1+i2+*i3;
}

void pLnC (C_BOOL i1, C_FLOAT_COMPLEX *i2 )
{
   printf("Inside pLnC: %d, %f, %f\n", i1, creal(*i2),  cimag(*i2) );
}
