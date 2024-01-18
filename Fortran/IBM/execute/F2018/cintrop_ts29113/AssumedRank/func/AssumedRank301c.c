#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define true 1
#define dummy 0

void check_int(CFI_cdesc_t * a) 
{
  int rc;

  if (a->base_addr == NULL)
  {
      fprintf(stderr, "Expecting an allocated object...\n");
      printf("...exiting.\n");
      exit(1);
  }

  if (a->type != CFI_type_int) 
  {
      fprintf(stderr, "Expected array of type CFI_type_int_fast8_t...\n");
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
      int *actual_a = a->base_addr;
      *actual_a = -99;
  }
  else if (a->rank == 1)
  {
#if _DEBUG
  printf("a is an array of rank %d\n", a->rank);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  fflush(stdout);
#endif 

      CFI_index_t lb[1] = {-5};
      CFI_index_t ub[1] = {5};
      rc = CFI_deallocate(a);
      assert(CFI_SUCCESS == rc);
      rc = CFI_allocate(a, lb, ub, dummy);
      assert(CFI_SUCCESS == rc);
      assert(a->base_addr);

#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  fflush(stdout);
#endif 
      int *actual_a = a->base_addr;
      for (int i=0; i<11; i++) 
      {
          *(actual_a+i) = (i-5);
      } 
  }
  else
  {
      fprintf(stderr, "Expecting array of rank: 0 or 1...\n");
      printf("...exiting.\n");
      exit(4);
  }

  return;
}

void check_short(CFI_cdesc_t * a)
{
  int rc;

  if (a->base_addr == NULL)
  {
      fprintf(stderr, "SHORT: Expecting an allocated object...\n");
      printf("...exiting.\n");
      exit(5);
  }

  if (a->type != CFI_type_short)
  {
      fprintf(stderr, "Expected array of type CFI_type_short...\n");
      printf("...exiting.\n");
      exit(6);
  }

  if (CFI_is_contiguous(a) != true)
  {
      fprintf(stderr, "Expected the array to be contiguous...\n");
      printf("...exiting.\n");
      exit(7);
  }

  if (a->rank == 3)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1,  a->dim[1].extent, a->dim[1].lower_bound, a->dim[1].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 2,  a->dim[2].extent, a->dim[2].lower_bound, a->dim[2].sm);
  fflush(stdout);
#endif 

      CFI_index_t lb[3] = {0, 0, -1};
      CFI_index_t ub[3] = {9, 2, 1};
      rc = CFI_deallocate(a);
      assert(CFI_SUCCESS == rc);
      rc = CFI_allocate( a, lb, ub, dummy);
      assert(CFI_SUCCESS == rc);
      assert(a->base_addr);
  }
  else
  {
      fprintf(stderr, "Expecting array of rank: 0 or 2...\n");
      printf("...exiting.\n");
      exit(8);
  }

  return;
}

void check_float(CFI_cdesc_t * a)
{
  int rc;

  if (a->base_addr == NULL)
  {
      fprintf(stderr, "Expecting an allocated object...\n");
      printf("...exiting.\n");
      exit(9);
  }

  if (a->type != CFI_type_float)
  {
      fprintf(stderr, "Expected array of type CFI_type_float...\n");
      printf("...exiting.\n");
      exit(10);
  }

  if (CFI_is_contiguous(a) != true)
  {
      fprintf(stderr, "Expected the array to be contiguous...\n");
      printf("...exiting.\n");
      exit(11);
  }

  if (a->rank == 0)
  {
#if _DEBUG
  printf("a is a scalar \n");
  fflush(stdout);
#endif 

       rc = CFI_deallocate(a);
      assert(CFI_SUCCESS == rc);

      rc = CFI_allocate( a, dummy, dummy, dummy);
      assert(rc == CFI_SUCCESS);
      assert(a->base_addr); 

      float *actual_a = a->base_addr;
      *actual_a = 3.1;
  }
  else if (a->rank == 2)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1,  a->dim[1].extent, a->dim[1].lower_bound, a->dim[1].sm);
  fflush(stdout);
#endif 

      rc = CFI_deallocate(a);
      assert(CFI_SUCCESS == rc);

      CFI_index_t lb[2] = {10, 11};
      CFI_index_t ub[2] = {11, 12};
      rc = CFI_allocate( a, lb, ub, dummy);
      assert(CFI_SUCCESS == rc);
      assert(a->base_addr);
  }
  else
  {
      fprintf(stderr, "Expecting array of rank: 0 or 2...\n");
      printf("...exiting.\n");
      exit(12);
  }

  return;
}

void check_complex(CFI_cdesc_t * a)
{
  int rc;

  if (a->base_addr == NULL)
  {
      fprintf(stderr, "Expecting an allocated object...\n");
      printf("...exiting.\n");
      exit(13);
  }

  if (a->type != CFI_type_float_Complex)
  {
      fprintf(stderr, "Expected array of type CFI_type_float_Complex...\n");
      printf("...exiting.\n");
      exit(14);
  }

  if (CFI_is_contiguous(a) != true)
  {
      fprintf(stderr, "Expected the array to be contiguous...\n");
      printf("...exiting.\n");
      exit(15);
  }

  if (a->rank == 0)
  {
#if _DEBUG
  printf("a is a scalar \n");
  fflush(stdout);
#endif 

      rc = CFI_deallocate(a);
      assert(CFI_SUCCESS == rc);

      rc = CFI_allocate( a, dummy, dummy, dummy);
      assert(rc == CFI_SUCCESS);
      assert(a->base_addr);

      float *actual_a = a->base_addr;
      *actual_a = 1.01;
      *(actual_a+1) = 2.2;
  }
  else if (a->rank == 1)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  fflush(stdout);
#endif 

      rc = CFI_deallocate(a);
      assert(CFI_SUCCESS == rc);

      CFI_index_t lb[1] = {1};
      CFI_index_t ub[1] = {3};
      rc = CFI_allocate( a, lb, ub, dummy);
      assert(rc == CFI_SUCCESS);
      assert(a->base_addr);
  }
  else
  {
      fprintf(stderr, "Expecting array of rank: 0 or 1...\n");
      printf("...exiting.\n");
      exit(16);
  }

  return;
}
