long double _Complex createcomplexl(long double arg1, long double arg2)
{
  union {
    long double _Complex lcomplex;
    struct {
      long double real;
      long double imag;
    } lstruct;
  } z;
  z.lstruct.real = arg1;
  z.lstruct.imag = arg2;
  return z.lcomplex;
}
#define ARGTYPE long double _Complex
#define DEREF 
#define TDEF_STMT typedef ARGTYPE DEREF TDEF_NAME
#define TARGET_DECL 
#define MALLOC_CALL 
#define FREE_CALL 
#define VALUE createcomplexl(42.0l, 84.0l)
#include "icc2fc.h"
