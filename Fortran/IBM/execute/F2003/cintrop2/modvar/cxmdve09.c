/* C code for testcase "fxmdve09.f" */
#include <inttypes.h>
#include <stdio.h> 

extern int16_t a[4];
int csub()
{
  if (a[4] !=10)
    return 1;
  if (a[0] !=-20)
    return 1;
  a[4]=a[3]*100;
  a[2]=-100 * a[2];
  return 0;

}
