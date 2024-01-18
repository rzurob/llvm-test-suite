#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void test_ptr(CFI_cdesc_t * a, int n)
{
  if (a->base_addr != NULL)
    exit(n);

  return;
}

void test_alloc(CFI_cdesc_t * a, int n)
{
  if (a->base_addr != NULL)
    exit(n);

  return;
}
