#include <stdlib.h>
#include <stdio.h>
#include "ISO_Fortran_binding.h"

void print2dfarr(CFI_cdesc_t * d, int test)
{
  CFI_index_t extents[2];
  CFI_index_t strides[2];
  int i, j;
  char * prow;

  if (d->rank != 2)
  {
    fprintf(stderr, "Expected rank 2 array!\n");
    exit(1);
  }
  
  if (d->type != CFI_type_double)
  {
    fprintf(stderr, "Expected array of type double!\n");
    exit(2);
  }

  printf("Test %d:\n", test);

  prow = (char *)d->base_addr;
  extents[0] = d->dim[0].extent;
  extents[1] = d->dim[1].extent;
  strides[0] = d->dim[0].sm;
  strides[1] = d->dim[1].sm;

  if (prow == NULL)
  {
    printf("Null Array\n");
    return;
  }

  // Print the values of the array one row at a time.
  // Note: This is not cache friendly, since Fortran arrays are column major.
  for(i = 0; i < extents[0]; i++)
  {
    char * tp = prow;
    for(j = 0; j < extents[1]; j++)
    {
      printf("%.1f ", *(double *)tp);
      tp += strides[1];
    }
    prow += strides[0];
    printf("\n");
  }
}
