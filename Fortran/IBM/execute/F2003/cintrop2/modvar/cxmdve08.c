/* C code for testcase "fxmdve08.f" */
#include <inttypes.h>
#include <stdio.h> 

extern int8_t a[4];
int csub()
{
  if (a[4] !=10)
    return 1;
  if (a[0] !=-20)
    return 1;
  a[4]=a[3]*3;
  a[2]=-30 * a[2];
  return 0;

}
