#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct dt1 DT1;

struct dt1 {
  int_least8_t ci8;
  int_least16_t ci16;
};

void sub_testf(size_t (*pt2Func1)(DT1 *), DT1 *, _Bool (*pt2Func2)(int_least32_t *, int_least64_t *), int_least32_t *);
size_t c_get_size_of (DT1 *);
_Bool c_get_presence_of (int_least32_t *, int_least64_t *);

int main(int argc, char ** argv)
{   
    DT1 c_dt1;
    int_least32_t cleast32;

    sub_testf(&c_get_size_of, NULL, NULL, &cleast32);
    sub_testf(&c_get_size_of, &c_dt1, &c_get_presence_of, &cleast32);

    return 0;
}

size_t c_get_size_of (DT1 *c_dt1)
{
  if (c_dt1 != NULL)
     return sizeof(*c_dt1);
  else
     return 0;
}

_Bool c_get_presence_of (int_least32_t *i1, int_least64_t *i2) 
{
  if (i2 != NULL)   
     return true;
  else
     return false;
}

