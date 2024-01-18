/* C code for testcase "fxbind_c03aak.f" */
/* Requires C compiler -qlongdbl option. */
#pragma options ldbl128
#include <stdio.h>
#include <math.h>
#include <assert.h>

long double arith_real16_ref(long double *x,long double *y) {
  assert ( *x == 5.0l ) ;
  assert ( *y == 10.0l );

  *x = *x + 5.0l;
  *y = *y + 10.0l;
  return *x > *y ? *x : *y;
}

long double arith_real16_val(long double x,long double y) {
  assert ( x == 5.0l ) ;
  assert ( y == 10.0l ) ;

  x = x + 5.0l;
  y = y + 10.0l;
  return x > y ? x : y;
}
