#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define dummy 0
#define true 1
#define false 0
#define ARR_RANK 15

void c_allocate(CFI_cdesc_t * a)
{
  int rc;
  CFI_index_t lower_bounds[] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
  CFI_index_t upper_bounds[] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3};

  if (a->base_addr != NULL)
  {
      printf("a is allocated... \n");
      printf("...expecting an unallocated var \n");
      printf("...exiting... \n");
      exit(1);
  }

  rc = CFI_allocate(a, lower_bounds, upper_bounds, dummy);
  assert(rc == CFI_SUCCESS);

  int *actual_a = a->base_addr;
  *actual_a = 3; 
  *(actual_a+1) = -2;
  *(actual_a+2) = 4;

  return;
}

void c_verify(CFI_cdesc_t * a)
{
  int rc;
  CFI_index_t lower[ARR_RANK], extents[ARR_RANK], strides[ARR_RANK];
  CFI_index_t subscripts[ARR_RANK];



  if (a->base_addr == NULL)
  {
      fprintf(stderr, "Expecting the arg. to be allocated... \n");
      printf("...exiting.\n");
      exit(2);
  }

  if (a->rank != ARR_RANK)
  {
      fprintf(stderr, "Expected rank 15 array...\n");
      printf("...exiting.\n");
      exit(3);
  }

  if (a->type != CFI_type_int)
  {
      fprintf(stderr, "Expected array of type int...\n");
      printf("...exiting.\n");
      exit(4);
  }

  if (CFI_is_contiguous(a) != true)
  {
      fprintf(stderr, "Expected the array to be contiguous...\n");
      printf("...exiting.\n");
      exit(5);
  }

  strides[0] = a->dim[0].sm;
  extents[0] = a->dim[0].extent;
  lower[0] = a->dim[0].lower_bound;
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0, extents[0], lower[0], strides[0]);
#endif 
  assert(lower[0] == 1);
  assert(extents[0] == 1);
  assert(strides[0] == 4);

  for (int i=1; i<14; i++) {
      strides[i] = a->dim[i].sm;
      extents[i] = a->dim[i].extent;
      lower[i] = a->dim[i].lower_bound;
#if _DEBUG
      printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", i, extents[i], lower[i], strides[i]);
#endif 
      assert(lower[i] == 1);
      assert(extents[i] == 1);
      assert(strides[i] == extents[i-1]*strides[i-1]);
  }

  strides[14] = a->dim[14].sm;
  extents[14] = a->dim[14].extent;
  lower[14] = a->dim[14].lower_bound;
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 14, extents[14], lower[14], strides[14]);
#endif 
  assert(lower[14] == 1);
  assert(extents[14] == 3);
  assert(strides[14] == extents[13]*strides[13]);

  int *address;
  for (int i=lower[14]; i<(lower[14]+extents[14]); i++) {
      subscripts[0] = 1;
      subscripts[1] = 1;
      subscripts[2] = 1;
      subscripts[3] = 1;
      subscripts[4] = 1;
      subscripts[5] = 1;
      subscripts[6] = 1;
      subscripts[7] = 1;
      subscripts[8] = 1;
      subscripts[9] = 1;
      subscripts[10] = 1;
      subscripts[11] = 1;
      subscripts[12] = 1;
      subscripts[13] = 1;
      subscripts[14] = i;
      address = (int *) CFI_address(a, subscripts);
      printf("the value stores in element %d is: %d\n", i, *address);
  }


  return;
}