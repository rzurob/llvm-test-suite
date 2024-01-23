/* C code for testcase "fxbind_c12xza.f" */

#include <stdio.h>
#include <assert.h>

float C_Func(float * x)
{
  /*  Purpose:dummy function called by Fortran. */
  float y;
  y = *x;
  return ( 3 * y *y  + 1.0);
}
