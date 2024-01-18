#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void check_f_to_c_in(CFI_cdesc_t* a, int *n)
{
  assert(a->base_addr != NULL);
  assert(CFI_is_contiguous(a)==1);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 1);
  assert(a->dim[0].extent == -1);
  assert(a->dim[0].lower_bound == 0);
  return;
}
void check_f_to_c_out(CFI_cdesc_t* a, int *n)
{
  assert(a->base_addr != NULL);
  assert(CFI_is_contiguous(a)==1);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 1);
  assert(a->dim[0].extent == -1);
  assert(a->dim[0].lower_bound == 0);
  return;
}

void check_f_to_c_inout(CFI_cdesc_t* a, int *n)
{
  assert(a->base_addr != NULL);
  assert(CFI_is_contiguous(a)==1);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 1);
  assert(a->dim[0].extent == -1);
  assert(a->dim[0].lower_bound == 0);
  return;
}



