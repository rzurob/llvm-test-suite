#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define true 1

void c_check(CFI_cdesc_t * a)
{
  int rc;

  if (a->base_addr == NULL)
  {
      fprintf(stderr, "Expecting the arg. to be allocated... \n");
      printf("...exiting.\n");
      exit(2);
  }

  if (a->type != CFI_type_short)
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

  if (a->rank == 0)
  {
#if _DEBUG
  printf("a is a scalar \n");
#endif 
      short *address;
      address = (short *) CFI_address(a, NULL);
      printf("the value stored in the object is: %d\n", *address);
  }
  else if (a->rank == 1)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
#endif 
      assert(a->dim[0].lower_bound == 1);
      assert(a->dim[0].extent == 10);
      assert(a->dim[0].sm == 2);

      short *address;
      CFI_index_t subscripts[1];
      for (int i=a->dim[0].lower_bound; i<(a->dim[0].lower_bound+a->dim[0].extent); i++) 
      {
          subscripts[0] = i;
          address = (short *) CFI_address(a, subscripts);
          printf("the value stored in element %d is: %d\n", i, *address);
      }

  }
  else if (a->rank == 2)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1,  a->dim[1].extent, a->dim[1].lower_bound, a->dim[1].sm);
#endif 
      assert(a->dim[0].lower_bound == 1);
      assert(a->dim[0].extent == 2);
      assert(a->dim[0].sm == 2);
      assert(a->dim[1].lower_bound == 1);
      assert(a->dim[1].extent == 3);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);

      short *address;
      CFI_index_t subscripts[2];
      for (int j=a->dim[1].lower_bound; j<(a->dim[1].lower_bound+a->dim[1].extent); j++) 
      {
          subscripts[1] = j;
          for (int k=a->dim[0].lower_bound; k<(a->dim[0].lower_bound+a->dim[0].extent); k++) 
          {
              subscripts[0] = k;
              address = (short *) CFI_address(a, subscripts);
              printf("the value stored in element (%d,%d) is: %d\n", k, j, *address);
          }
      }
  }
  else if (a->rank == 3)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1,  a->dim[1].extent, a->dim[1].lower_bound, a->dim[1].sm);
#endif 
      assert(a->dim[0].lower_bound == -4);
      assert(a->dim[0].extent == 10);
      assert(a->dim[0].sm == 2);
      assert(a->dim[1].lower_bound == -2);
      assert(a->dim[1].extent == 6);
      assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
      assert(a->dim[2].lower_bound == 0);
      assert(a->dim[2].extent == 1);
      assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);

      short *address;
      CFI_index_t subscripts[3];
      for (int k=a->dim[2].lower_bound; k<(a->dim[2].lower_bound+a->dim[2].extent); k++) 
      {
          subscripts[2] = k;
          for (int j=a->dim[1].lower_bound; j<(a->dim[1].lower_bound+a->dim[1].extent); j++) 
          {
              subscripts[1] = j;
              for (int i=a->dim[0].lower_bound; i<(a->dim[0].lower_bound+a->dim[0].extent); i++) 
              {
                  subscripts[0] = i;
                  address = (short *) CFI_address(a, subscripts);
                  printf("the value stored in element (%d,%d,%d) is: %d\n", i, j, k, *address);
                  if (*address != i+j+k)
                      exit(99); 
              }
          }
      }

  }
  else if (a->rank == 15)
  {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
#endif 
      assert(a->dim[0].lower_bound == 1);
      assert(a->dim[0].extent == 1);
      assert(a->dim[0].sm == 2);

      for (int i=1; i<14; i++) 
      {
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", i, a->dim[i].extent, a->dim[i].lower_bound, a->dim[i].sm);
#endif 
          assert(a->dim[i].lower_bound == 1);
          assert(a->dim[i].extent == 1);
          assert(a->dim[i].sm == a->dim[i-1].extent*a->dim[i-1].sm);
      }

#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[14].extent, a->dim[14].lower_bound, a->dim[14].sm);
#endif 
      assert(a->dim[14].lower_bound == 1);
      assert(a->dim[14].extent == 3);
      assert(a->dim[14].sm == a->dim[13].extent*a->dim[13].sm);

      short *address;
      CFI_index_t subscripts[15];
      for (int i=a->dim[14].lower_bound; i<(a->dim[14].lower_bound+a->dim[14].extent); i++) 
      {
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
          address = (short *) CFI_address(a, subscripts);
          printf("the value stored in element %d is: %d\n", i, *address);
      }
  }
  else
  {
      fprintf(stderr, "Expecting array of rank: 0, 1, 2, 3 or 15...\n");
      printf("...exiting.\n");
      exit(3);
  }

  return;
}
