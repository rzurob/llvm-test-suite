#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define dummy 0

void c_associate(CFI_cdesc_t * a)
{
  int rc;

  printf("In c_associate\n");
  if (a->base_addr == NULL)
  {
      printf("a is not associated... \n");
      rc = CFI_allocate(a, dummy, dummy, dummy);
      assert(rc == CFI_SUCCESS);
      printf("...associate a\n");
  }
  else
  {
      printf("a is associated... \n");
      int *a_address;
      a_address = (int*) CFI_address(a, NULL);
      printf("a has the value: %d\n", *a_address);
  }

  return;
}

void c_allocate(CFI_cdesc_t * a)
{
  int rc;
  printf("In c_allocate\n");
  if (a->base_addr == NULL)
  {
      printf("a is not allocated... \n");
      rc = CFI_allocate(a, dummy, dummy, dummy);
      assert(rc == CFI_SUCCESS);
      printf("...allocate a\n");
  }
  else
  {
      printf("a is allocated... \n");
      int *a_address;
      a_address = (int*) CFI_address(a, NULL);
      printf("a has the value: %d\n", *a_address);
  }

  return;
}
