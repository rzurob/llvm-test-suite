/* C code for testcase "fxbind_c03mma.f" */

#include <stdio.h>
#include <math.h>

signed char arith_int1_ref(signed char *x,signed char *y) {
  if ( *x != 5 ) exit(70);
  if ( *y != 10 ) exit(71);

  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed char arith_int1_val(signed char x,signed char y) {
  if ( x != 5 ) exit(72);
  if ( y != 10 ) exit(73);

  x = x + 5;
  y = y + 10;
  return x > y ? x : y;
}

signed short arith_int2_ref(signed short *x,signed short *y) {
  if ( *x != 5 ) exit(74);
  if ( *y != 10 ) exit(75);

  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed short arith_int2_val(signed short x,signed short y) {
  if ( x != 5 ) exit(76);
  if ( y != 10 ) exit(77);

  x = x + 5;
  y = y + 10;
  return x > y ? x : y;
}

signed int arith_int4_ref(signed int *x,signed int *y) {
  if ( *x != 5 ) exit(78);
  if ( *y != 10 ) exit(79);

  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed int arith_int4_val(signed int x,signed int y) {
  if ( x != 5 ) exit(80);
  if ( y != 10 ) exit(81);

  x = x + 5;
  y = y + 10;
  return x > y ? x : y;
}

signed long long arith_int8_ref(signed long long *x,signed long long *y) {
  if ( *x != 5 ) exit(82);
  if ( *y != 10 ) exit(83);

  *x = *x + 5;
  *y = *y + 10;
  return *x > *y ? *x : *y;
}

signed long long arith_int8_val(signed long long x,signed long long y) {
  if ( x != 5 ) exit(84);
  if ( y != 10 ) exit(85);

  x = x + 5;
  y = y + 10;
  return x > y ? x : y;
}
