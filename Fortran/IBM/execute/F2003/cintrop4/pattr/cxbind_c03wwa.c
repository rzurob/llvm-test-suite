/* C Code for testcase fxbind_c03wwa.f */
/* Use option -qlanglvl=stdc99 to compile */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

_Bool  check(char  s1[5], char  s2[5])
  
{  
  int i;
  int count = 5;   
  for(i = 0; i < count; i++)
    assert (s1[i]==s2[i]);
  return 0; 
} 
 
