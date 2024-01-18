#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define true 1
#define dummy 0

void c_modify(CFI_cdesc_t * a)
{
  int rc;

  if (a->base_addr != NULL)
  {
      fprintf(stderr, "INTENT(OUT): variable should have been deallocated before execution of the C function begins...\n");
      printf("...exiting.\n");
      exit(1);
  }

  if (a->type != CFI_type_float)
  {
      fprintf(stderr, "Expected array of type float...\n");
      printf("...exiting.\n");
      exit(2);
  }

  if (CFI_is_contiguous(a) != true)
  {
      fprintf(stderr, "Expected the array to be contiguous...\n");
      printf("...exiting.\n");
      exit(3);
  }

  if (a->rank == 0)
  {
#if _DEBUG
  printf("a is a scalar \n");
  fflush(stdout);
#endif 
      rc = CFI_allocate( a, dummy, dummy, dummy);
      assert(rc == CFI_SUCCESS);
      assert(a->base_addr);

      float *actual_a = a->base_addr;
      *actual_a = 3.1;
  }
  else if (a->rank == 1)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  fflush(stdout);
#endif 

      CFI_index_t lb[1] = {-5};
      CFI_index_t ub[1] = {5};
      rc = CFI_allocate( a, lb, ub, dummy);
      assert(CFI_SUCCESS == rc);
      assert(a->base_addr);

      float *actual_a = a->base_addr;
      for (int i=0; i<11; i++) 
      {
          *(actual_a+i) = (i-5)*1.0;
      } 
  }
  else if (a->rank == 2)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1,  a->dim[1].extent, a->dim[1].lower_bound, a->dim[1].sm);
  fflush(stdout);
#endif 

      CFI_index_t lb[2] = {-1, 0};
      CFI_index_t ub[2] = {0, 2};
      rc = CFI_allocate( a, lb, ub, dummy);
      assert(CFI_SUCCESS == rc);
      assert(a->base_addr);

      float matrix[3][2] = { {3., -2.} ,{2., 4.}, {1., 1.} };

      float *actual_a = a->base_addr;
      *actual_a = matrix[0][0];
      *(actual_a+1) = matrix[0][1];
      *(actual_a+2) = matrix[1][0];
      *(actual_a+3) = matrix[1][1];
      *(actual_a+4) = matrix[2][0];
      *(actual_a+5) = matrix[2][1];
  }
  else if (a->rank == 15)
  {
      for (int i=0; i<15; i++) 
      {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", i, a->dim[i].extent, a->dim[i].lower_bound, a->dim[i].sm);
  fflush(stdout);
#endif 
      }

      CFI_index_t lb[15] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
      CFI_index_t ub[15] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3};
      rc = CFI_allocate( a, lb, ub, dummy);
      assert(rc == CFI_SUCCESS);
      assert(a->base_addr);

      float *actual_a = a->base_addr;
      *actual_a = 3;
      *(actual_a+1) = -2;
      *(actual_a+2) = 4;
  }
  else
  {
      fprintf(stderr, "Expecting array of rank: 0, 1, 2, 3 or 15...\n");
      printf("...exiting.\n");
      exit(4);
  }

  return;
}
