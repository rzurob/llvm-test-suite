#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void check_f_to_f(CFI_cdesc_t* a, int *c_len, int *test_no);
void check_f_to_c(int *c_len, int *test_no, CFI_cdesc_t* a)
{
  assert(a== NULL); 
  return;
}

void check_f_to_c_to_f(int *c_len, int *test_no, CFI_cdesc_t* a)
{
    assert(a==NULL);
    check_f_to_f(c_len, test_no, a);
    return;
}

