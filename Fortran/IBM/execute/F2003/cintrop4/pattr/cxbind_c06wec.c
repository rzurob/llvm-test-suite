/* C Code for testcase fxbind_c06wec.f */

#include <stdio.h>
#include <stdlib.h>

double sum(double * , int *);
double  check(double * s1, int * n)
          
{
  double value;
  double (*p)(double *,int *); /* function pointer */
  p = sum ;/*assign address of function sum  to p */  
  
  value = (*p)(s1,n);
     
  return value;
} 

double sum( double * x, int * n ) {
  double s = 0.0;
  int i;
  for(i=0;i<*n;i++) s += x[i];
  return s;
}
