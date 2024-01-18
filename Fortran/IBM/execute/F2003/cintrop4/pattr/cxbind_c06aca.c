/* C code for testcase "fxbind_c06aca.f" */

#include <stdio.h>
#include <math.h>
#include <assert.h>

signed long long arith_int1_ref(signed long long *x,signed long long *y) {
  assert ( *x == 5 ) ;
  assert (*y == 10 ) ;

  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed long long arith_int1_val(signed long long x,signed long long y) {
  assert ( x== 5 ) ;
  assert ( y == 10 ) ;

  x = x + 5;
  y = y + 10;
  return x > y ? x : y;
}

signed short arith_int2_ref(signed short *x,signed short *y) {
  assert (*x == 5 ) ;
  assert ( *y == 10 ) ;

  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed short arith_int2_val(signed short x,signed short y) {
  assert ( x== 5 ) ;
  assert ( y == 10 ) ;

  x = x + 5;
  y = y + 10;
  return x > y ? x : y;
}

signed int arith_int4_ref(signed int *x,signed int *y) {
  assert (*x == 5 ) ;
  assert ( *y == 10 ) ;

  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed int arith_int4_val(signed int x,signed int y) {
  assert ( x== 5 ) ;
  assert ( y == 10 ) ;

  x = x + 5;
  y = y + 10;
  return x > y ? x : y;
}

signed long long arith_int8_ref(signed long long *x,signed long long *y) {
  assert (*x == 5 ) ;
  assert ( *y == 10 ) ;

  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed long long arith_int8_val(signed long long x,signed long long y) {
  assert ( x== 5 ) ;
  assert ( y == 10 ) ;

  x = x + 5;
  y = y + 10;
  return x > y ? x : y;
}
