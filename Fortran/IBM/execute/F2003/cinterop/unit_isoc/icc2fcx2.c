float _Complex createcomplexf(float arg1, float arg2)
{
  union {
    float _Complex fcomplex;
    struct {
      float real;
      float imag;
    } fstruct;
  } z;
  z.fstruct.real = arg1;
  z.fstruct.imag = arg2;
  return z.fcomplex;
}
#define ARGTYPE float _Complex
#define DEREF *
#define TDEF_STMT typedef ARGTYPE DEREF TDEF_NAME
#define TARGET_DECL 
#define MALLOC_CALL arg = malloc(sizeof(arg))
#define FREE_CALL free(arg)
#define VALUE createcomplexf(42.0f, 84.0f)
#include "icc2fc.h"
