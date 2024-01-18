#include <inttypes.h>
#include <malloc.h>

int64_t getsumplus1 ( size_t *i1, int8_t *i2, int16_t *i3, int32_t *i4, int64_t *i5 )
{
   int64_t i = *i1+*i2+*i3+*i4+*i5;
   
   (*i1)++;
   (*i2)++;
   (*i3)++;
   (*i4)++;
   (*i5)++;
   
   return i;
}

int64_t  getsumminus1 ( size_t *i1, int8_t *i2, int16_t *i3, int32_t *i4, int64_t *i5 )
{
   int64_t i = *i1+*i2+*i3+*i4+*i5;
   
   (*i1)--;
   (*i2)--;
   (*i3)--;
   (*i4)--;
   (*i5)--;
   
   return i;
}

