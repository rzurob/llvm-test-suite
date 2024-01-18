#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void sub_1(CFI_cdesc_t * a)
{
  int i, j;
  if (a->rank == 2)
  {
    char *address;
    CFI_index_t subscripts[2];

    printf("\n value on C Side : ");

    for (i = a->dim[1].lower_bound;
         i < a->dim[1].lower_bound + a->dim[1].extent;
         ++i)
    {
      for (j = a->dim[0].lower_bound;
           j < a->dim[0].lower_bound + a->dim[0].extent;
           ++j)
      {
        subscripts[0] = j;
        subscripts[1] = i;
        address = (char *) CFI_address(a, subscripts );
        printf("%c",*((char *)address));
      }
    }
    printf("\n");
  }
  return;
}
