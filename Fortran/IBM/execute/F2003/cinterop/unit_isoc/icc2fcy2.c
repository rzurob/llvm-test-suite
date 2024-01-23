double _Complex createcomplex(double arg1, double arg2)
{
  union {
    double _Complex dcomplex;
    struct {
      double real;
      double imag;
    } dstruct;
  } z;
  z.dstruct.real = arg1;
  z.dstruct.imag = arg2;
  return z.dcomplex;
}
#define ARGTYPE double _Complex
#define DEREF *
#define TDEF_STMT typedef ARGTYPE DEREF TDEF_NAME
#define TARGET_DECL 
#define MALLOC_CALL arg = malloc(sizeof(*arg))
#define FREE_CALL free(arg)
#define VALUE createcomplex(42.0, 84.0)
#include "icc2fc.h"
