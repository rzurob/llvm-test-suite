/* C code for testcase "fxbind_c12xya.f" */

#include <stdio.h>
#include <assert.h>

float test_fun(float * x)
{
  /*  Purpose:dummy function called by Fortran. */
  float y;
  y = *x;
  return ( 3 * y *y  + 1.0);
}
