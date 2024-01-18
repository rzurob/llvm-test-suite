#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void check_f_to_c_in(CFI_cdesc_t* a, int *c_len, int *test_no)
{
  assert(a->base_addr != NULL);
  assert(a->elem_len  == *c_len); 
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 0);
  return;
}
void check_f_to_c_out(CFI_cdesc_t* a, int *c_len, int *test_no)
{
  assert(a->base_addr != NULL);
  assert(a->elem_len  == *c_len);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 0);
  return;
}

void check_f_to_c_inout(CFI_cdesc_t* a, int *c_len, int *test_no)
{
  assert(a->base_addr != NULL);
  assert(a->elem_len  == *c_len);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 0);
  return;
}




