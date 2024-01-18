#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

// Prototype for the function that is defined on the Fortran side.
void foo(CFI_cdesc_t *farr);

int main()
{
  CFI_CDESC_T(2) arr2d;
  CFI_cdesc_t * desc_arr2d = (CFI_cdesc_t *) &arr2d;
  CFI_index_t extents[2];
  CFI_index_t strides[2];
  CFI_rank_t rank = 2;
  int rc, i, j;
  char * prow, * pp;
  
  rc = CFI_establish(desc_arr2d,
                     NULL,
                     CFI_attribute_allocatable,
                     CFI_type_double,
                     sizeof(double),
                     rank,
                     NULL);
  assert(CFI_SUCCESS == rc);

  // call Fortran
  foo(desc_arr2d);
  assert(desc_arr2d->base_addr);

  prow = (char *)desc_arr2d->base_addr;
  extents[0] = desc_arr2d->dim[0].extent;
  extents[1] = desc_arr2d->dim[1].extent;
  strides[0] = desc_arr2d->dim[0].sm;
  strides[1] = desc_arr2d->dim[1].sm;

  // print the values of the array one row at a time
  for(i = 0; i < extents[0]; i++)
  {
    pp = prow;
    for(j = 0; j < extents[1]; j++)
    {
      printf("%.3f ", *(double *)pp);
      pp += strides[1];
    }
    prow += strides[0];
    printf("\n");
  }

  rc = CFI_deallocate(desc_arr2d);
  assert(CFI_SUCCESS == rc);
  return 0;
}
