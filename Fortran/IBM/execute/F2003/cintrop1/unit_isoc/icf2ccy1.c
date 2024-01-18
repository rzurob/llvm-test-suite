double creal(double _Complex arg)
{
  union {
    double _Complex dcomplex;
    struct {
      double real;
      double imag;
    } dstruct;
  } z;
  z.dcomplex = arg;
  return z.dstruct.real;
}

double cimag(double _Complex arg)
{
  union {
    double _Complex dcomplex;
    struct {
      double real;
      double imag;
    } dstruct;
  } z;
  z.dcomplex = arg;
  return z.dstruct.imag;
}
#define ARGNAME arg
#define ARGTYPE double _Complex
#define COMPARISON creal(ARGNAME) == 42.0 && cimag(ARGNAME) == 84.0
#include "icf2cc.h"
