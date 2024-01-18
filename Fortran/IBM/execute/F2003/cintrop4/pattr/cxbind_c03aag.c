/* C code for testcase "fxbind_c03aag.f" */

#include <stdio.h>
#include <math.h>
#include <assert.h>

signed char arith_int1_ref(signed char *x,signed char *y) {
  assert ( *x == 5 ) ;
  assert (*y == 8 ) ;
  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed short arith_int2_ref(signed short *x,signed short *y) {
  assert ( *x == 5 ) ;
  assert (*y == 8 ) ;
  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed int arith_int4_ref(signed int *x,signed int *y) {
  assert (*x == 5 ) ;
  assert ( *y == 8 ) ;
  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed long long arith_int8_ref(signed long long *x,signed long long *y) {
  assert (*x == 5 ) ;
  assert ( *y == 8 ) ;
  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed long long arith_int8_octal(signed long long *x,signed long long *y) {
  assert (*x == 5 ) ;
  assert ( *y == 511 ) ;
  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}
