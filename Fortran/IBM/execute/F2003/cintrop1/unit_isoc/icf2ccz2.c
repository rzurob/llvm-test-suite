long double creall(long double _Complex arg)
{
  union {
    long double _Complex lcomplex;
    struct {
      long double real;
      long double imag;
    } lstruct;
  } z;
  z.lcomplex = arg;
  return z.lstruct.real;
}

long double cimagl(long double _Complex arg)
{
  union {
    long double _Complex lcomplex;
    struct {
      long double real;
      long double imag;
    } lstruct;
  } z;
  z.lcomplex = arg;
  return z.lstruct.imag;
}
#define ARGNAME arg
#define ARGTYPE long double _Complex*
#define COMPARISON creall(*ARGNAME) == 42.0l && cimagl(*ARGNAME) == 84.0l
#include "icf2cc.h"
