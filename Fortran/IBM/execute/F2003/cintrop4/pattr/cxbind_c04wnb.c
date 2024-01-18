/* C Code for testcase fxbind_c04wnb.f */

#include <stdio.h>
#include <stdlib.h>

int sum(int * , int *);
int  second_c_function(int * s1, int * n)
          
{
  int value;
  int (*p)(int *,int *); /* function pointer */
  p = sum ;/*assign address of function sum  to p */  
  
  value = (*p)(s1,n);
     
  return value;
} 
 

int sum(int * x, int *n)
{
  int i,s=0;
  for (i = 0; i< *n ; ++i)
    s += x[i];
  return s;
}
  
