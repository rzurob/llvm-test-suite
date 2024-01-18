/* C code for testcase "fxbind_c03aad.f" */

#include <stdio.h>
#include <complex.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

float _Complex fun_complex4_ref(float _Complex *x, double _Complex *y) {

  assert (*x == 5.0f+I*5.0f ) ;
  assert (*y == 10.0+I*10.0 ) ;
  *x = *x + 5.0f+I*5.0f;
  *y = *y + 10.0+I*10.0;
  return *y; 
}

float _Complex fun_complex4_val(float _Complex x, double _Complex y) {

  assert ( x == 5.0f+I*5.0f ) ;
  assert ( y == 10.0+I*10.0 ) ;
  x = x + 5.0f+I*5.0f;
  y = y + 10.0+I*10.0;
  return y; 
}

double _Complex fun_complex8_ref(double _Complex *x) {
  assert (*x == 5.0+I*5.0 ) ;
  *x = *x + 5.0+I*5.0;
  return *x; 
}

double _Complex fun_complex8_val(double _Complex x) {

  if ( x == 5.0+I*5.0 ) ;
  x = x + 5.0+I*5.0;
  return x;
}

