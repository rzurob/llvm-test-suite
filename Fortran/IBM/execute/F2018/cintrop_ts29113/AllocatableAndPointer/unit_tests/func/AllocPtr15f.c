#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

// Prototype for the function that is defined on the Fortran side.
short test_allocated(CFI_cdesc_t * a);

void test2(CFI_cdesc_t * a, CFI_cdesc_t * b)
{
  int rc;

  if (!test_allocated(a))
    exit(6);
  if ((*(int*)a->base_addr) != 77)
    exit(7);
  if (test_allocated(b))
    exit(8);

  rc = CFI_allocate(b, NULL, NULL, 0);
  assert(CFI_SUCCESS == rc);
  (*(int*)(b->base_addr)) = -88;

  return;
}

