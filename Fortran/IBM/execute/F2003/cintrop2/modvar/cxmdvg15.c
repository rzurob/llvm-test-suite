/* C code for testcase "fxmdvg15.f" */
#include <inttypes.h>
#include <stdio.h> 

extern int_least64_t xaa,Xaa;
int_least64_t compare_val1,compare_val2;

int csub()
{
  printf("%d", Xaa);
  /* Initialization */
  compare_val1 = 1;

  compare_val2 = 2;


  if (xaa !=compare_val1)
    return 1; 
  xaa +=10;

  if (Xaa !=compare_val2)
    return 1; 
  Xaa = Xaa * 100;


  return 0;
}
