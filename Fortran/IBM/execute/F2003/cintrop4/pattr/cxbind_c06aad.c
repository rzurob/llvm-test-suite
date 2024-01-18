/* C code for testcase "fxbind_c06aad.f" */

#include <stdio.h>
#include <complex.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

double _Complex fun_complex4_ref(double _Complex *x, double _Complex *y) {

  assert (*x == 5.0+I*5.0 ) ;
  assert (*y == 10.0+I*10.0 ) ;
  *x = *x + 5.0+I*5.0;
  *y = *y + 10.0+I*10.0;
  return *y; 
}

double _Complex fun_complex4_val(double _Complex x, double _Complex y) {

  assert ( x == 5.0+I*5.0 ) ;
  assert ( y == 10.0+I*10.0 ) ;
  x = x + 5.0+I*5.0;
  y = y + 10.0+I*10.0;
  return y; 
}

