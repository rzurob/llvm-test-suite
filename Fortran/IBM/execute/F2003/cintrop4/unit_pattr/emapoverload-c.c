/**
*** Implement Fortran overloading-interfaces in C
*** call from Fortran
**/

#include "pf.h"  /* for pf() -- Print and Flush */


float
f1a_B()
{
  pf ("%s entered (no args)\n", __FUNCTION__);
  return 1.0;
}

float
f1b_B (float a)
{
  pf ("%s entered (args %f)\n", __FUNCTION__, a);
  return a + 1.0;
}

float
f1c_B (float a, float b)
{
  pf ("%s entered (args %f, %f)\n", __FUNCTION__, a, b);
  return a + b;
}
