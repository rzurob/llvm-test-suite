#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define true 1

void c_check(CFI_cdesc_t * a)
{
  int rc;

  if (a->base_addr != NULL)
  {
      fprintf(stderr, "INTENT(OUT): variable should have been deallocated before execution of the C function begins...\n");
      printf("...exiting.\n");
      exit(1);
  }


#ifdef __64BIT__
      if (a->type != CFI_type_long_long)
      {
          fprintf(stderr, "Expecting object of type long...\n");
          printf("...exiting.\n");
          exit(2);
      }
#else
      if (a->type != CFI_type_int)
      {
          fprintf(stderr, "Expecting object of type long...\n");
          printf("...exiting.\n");
          exit(2);
      }
#endif

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
#endif 
  }
  else if (a->rank == 1)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
#endif 
  }
  else if (a->rank == 2)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1,  a->dim[1].extent, a->dim[1].lower_bound, a->dim[1].sm);
#endif 
  }
  else if (a->rank == 3)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1,  a->dim[1].extent, a->dim[1].lower_bound, a->dim[1].sm);
#endif 
  }
  else if (a->rank == 15)
  {
      for (int i=0; i<15; i++) 
      {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", i, a->dim[i].extent, a->dim[i].lower_bound, a->dim[i].sm);
#endif 
      }

  }
  else
  {
      fprintf(stderr, "Expecting array of rank: 0, 1, 2, 3 or 15...\n");
      printf("...exiting.\n");
      exit(4);
  }

  return;
}
