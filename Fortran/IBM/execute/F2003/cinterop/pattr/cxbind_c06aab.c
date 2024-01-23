/* C code for testcase "fxbind_c06aab.f" */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/* floating-point constant represented in ANSI C standard */
/* in the absence of a 'f' or 'l' qualifier, floating-point */
/* constants are assumed to be of datatype double. */

double  arith_real4_ref(double *x,double *y) {
  if ( *x != 5.0 ) exit(78);
  if ( *y != 10.0 ) exit(79);

  *x = *x + 5.0;
  *y = *y + 10.0;
  return *x > *y ? *x : *y;
}

double arith_real4_val(double x,double y) {
  if ( x != 5.0 ) exit(80);
  if ( y != 10.0 ) exit(81);

  x = x + 5.0;
  y = y + 10.0;
  return x > y ? x : y;
}

double arith_real8_ref(double *x,double *y) {
  if ( *x != 5.0 ) exit(82);
  if ( *y != 10.0 ) exit(83);

  *x = *x + 5.0;
  *y = *y + 10.0;
  return *x > *y ? *x : *y;
}

double arith_real8_val(double x,double y) {
  if ( x != 5.0 ) exit(84);
  if ( y != 10.0 ) exit(85);

  x = x + 5.0;
  y = y + 10.0;
  return x > y ? x : y;
}

