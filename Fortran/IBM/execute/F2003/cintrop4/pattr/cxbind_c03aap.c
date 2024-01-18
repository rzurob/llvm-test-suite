/* C code for testcase "fxbind_c03aap.f" */

#pragma options ldbl128 
#include <stdio.h>
#include <complex.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

long double _Complex fun_complex16_ref(long double _Complex *x) {
  assert (*x == 5.0l+I*5.0l ) ;
  *x = *x + 5.0l+I*5.0l;
  return *x; 

}

long double _Complex fun_complex16_val(long double _Complex x) {
  assert ( x == 5.0l+I*5.0l ) ;
  x = x + 5.0l+I*5.0l;
  return x;
}
