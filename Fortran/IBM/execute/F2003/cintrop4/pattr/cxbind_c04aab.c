/* C code for testcase "fxbind_c03aab.f" */

#include <stdio.h>
#include <math.h>
#include <assert.h>

/* floating-point constant represented in ANSI C standard */
/* in the absence of a 'f' or 'l' qualifier, floating-point */
/* constants are assumed to be of datatype double. */

float  arith_real4_ref(float *x,float *y) {
  assert ( *x == 5.0f ) ;
  assert ( *y == 10.0f ) ;

  *x = *x + 5.0f;
  *y = *y + 10.0f;
  return *x > *y ? *x : *y;
}

float arith_real4_val(float x,float y) {
  assert ( x == 5.0f ) ;
  assert ( y == 10.0f ) ;

  x = x + 5.0f;
  y = y + 10.0f;
  return x > y ? x : y;
}

double arith_real8_ref(double *x,double *y) {
  assert ( *x == 5.0 ) ;
  assert ( *y == 10.0 ) ;

  *x = *x + 5.0;
  *y = *y + 10.0;
  return *x > *y ? *x : *y;
}

double arith_real8_val(double x,double y) {
  assert ( x == 5.0 ) ;
  assert ( y == 10.0 ) ;

  x = x + 5.0;
  y = y + 10.0;
  return x > y ? x : y;
}

