float crealf(float _Complex arg)
{
  union {
    float _Complex fcomplex;
    struct {
      float real;
      float imag;
    } fstruct;
  } z;
  z.fcomplex = arg;
  return z.fstruct.real;
}

float cimagf(float _Complex arg)
{
  union {
    float _Complex fcomplex;
    struct {
      float real;
      float imag;
    } fstruct;
  } z;
  z.fcomplex = arg;
  return z.fstruct.imag;
}
#define ARGNAME arg
#define ARGTYPE float _Complex
#define COMPARISON crealf(ARGNAME) == 42.0f && cimagf(ARGNAME) == 84.0f
#include "icf2cc.h"
