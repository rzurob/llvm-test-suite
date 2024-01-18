/* C Code for testcase fxbind_c04wwd.f */
/* Compile use option:  -qlanglvl=stdc99  */

#include <stdio.h>
#include <stdlib.h>
#include "cmplx.h"

float sum(float  _Complex * , int *);
float   check(float  _Complex * s1, int * n)
          
{
  float  _Complex value;
  float  (*p)(float  _Complex *,int *); /* function pointer */
  p = sum ;/*assign address of function sum  to p */  
  
  value = (*p)(s1,n);
     
  return value;
} 

float sum( float  _Complex * x, int * n ) {
  float  sum_v=0;

  int i;
  for(i=0;i<*n;i++) sum_v += crealf( x[i]);
  return sum_v;
}
