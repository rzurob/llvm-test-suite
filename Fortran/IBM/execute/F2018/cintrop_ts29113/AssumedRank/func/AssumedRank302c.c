#include <stdio.h>
#include <assert.h>
#include <math.h>
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

      int *a_address;
      a_address = (int*) CFI_address(a, NULL);
#if _DEBUG
      printf("a has the value: %d\n", *a_address);
#endif 
      *a_address = -99;
  }
  else if (a->rank == 1)
  {
#if _DEBUG
  printf("a is an array of rank %d\n", a->rank);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  fflush(stdout);
#endif 
 
      // Check bounds and extent 
      assert(a->dim[0].extent == 10);
      assert(a->dim[0].lower_bound == 1);

      // Check values  
      int *address;
      CFI_index_t subscripts[1];
      for (int i=a->dim[0].lower_bound; i<(a->dim[0].lower_bound+a->dim[0].extent); i++)
      {
         subscripts[0] = i;
         address = (int *) CFI_address(a, subscripts);
         if (*address != pow(i, 2))
           exit(4);
#if _DEBUG
         printf("the value stored in element %d is: %d\n", i, *address);
#endif 
      }

      // Chnage values  
      int *actual_a = a->base_addr;
      for (int i=0; i<10; i++) 
      {
          *(actual_a+i) = (i-5);
      } 
  }
  else
  {
      fprintf(stderr, "Expecting array of rank: 0 or 1...\n");
      printf("...exiting.\n");
      exit(5);
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
      exit(6);
  }

  if (a->type != CFI_type_short)
  {
      fprintf(stderr, "Expected array of type CFI_type_short...\n");
      printf("...exiting.\n");
      exit(7);
  }

  if (CFI_is_contiguous(a) != true)
  {
      fprintf(stderr, "Expected the array to be contiguous...\n");
      printf("...exiting.\n");
      exit(8);
  }

  if (a->rank == 3)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1,  a->dim[1].extent, a->dim[1].lower_bound, a->dim[1].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 2,  a->dim[2].extent, a->dim[2].lower_bound, a->dim[2].sm);
  fflush(stdout);
#endif 

  }
  else
  {
      fprintf(stderr, "Expecting array of rank: 0 or 2...\n");
      printf("...exiting.\n");
      exit(9);
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
      exit(10);
  }

  if (a->type != CFI_type_float)
  {
      fprintf(stderr, "Expected array of type CFI_type_float...\n");
      printf("...exiting.\n");
      exit(11);
  }

  if (CFI_is_contiguous(a) != true)
  {
      fprintf(stderr, "Expected the array to be contiguous...\n");
      printf("...exiting.\n");
      exit(12);
  }

  if (a->rank == 0)
  {
#if _DEBUG
  printf("a is a scalar \n");
  fflush(stdout);
#endif 

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

  }
  else
  {
      fprintf(stderr, "Expecting array of rank: 0 or 2...\n");
      printf("...exiting.\n");
      exit(13);
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
      exit(14);
  }

  if (a->type != CFI_type_float_Complex)
  {
      fprintf(stderr, "Expected array of type CFI_type_float_Complex...\n");
      printf("...exiting.\n");
      exit(15);
  }

  if (CFI_is_contiguous(a) != true)
  {
      fprintf(stderr, "Expected the array to be contiguous...\n");
      printf("...exiting.\n");
      exit(16);
  }

  if (a->rank == 0)
  {
#if _DEBUG
  printf("a is a scalar \n");
  fflush(stdout);
#endif 

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

  }
  else
  {
      fprintf(stderr, "Expecting array of rank: 0 or 1...\n");
      printf("...exiting.\n");
      exit(17);
  }

  return;
}