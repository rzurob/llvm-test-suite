#include <inttypes.h>

intmax_t add( intmax_t i1, intptr_t i2, int_least8_t i3, int_fast16_t i4, int_fast64_t i5 );
intmax_t multiply( intmax_t *i1, intptr_t *i2, int_least8_t *i3, int_fast16_t *i4, int_fast64_t *i5 );

intmax_t getsum ( intmax_t i1, intptr_t i2, int_least8_t i3, int_fast16_t i4, int_fast64_t i5 )
{
   return add ( i1, i2, i3, i4, i5 );
}

intmax_t getproduct ( intmax_t i1, intptr_t i2, int_least8_t i3, int_fast16_t i4, int_fast64_t i5 )
{
   return multiply( &i1, &i2, &i3, &i4, &i5 );
}

