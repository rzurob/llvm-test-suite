#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void sub_1(CFI_cdesc_t * a)
{
  if (a->base_addr == NULL)
  {
      fprintf(stderr, "Expecting an allocated object...\n");
      exit(1);
  }

  if (a->type != CFI_type_char)
  {   
      printf("a->type is %d", a->type);
      fprintf(stderr, "CFI_type_char...\n");
      exit(2);
  }


#if _DEBUG 
  printf(" element length of a is :  %d  \n" , a->elem_len);
  printf(" a->rank is : %d \n", a->rank);
  for(int i=0; i< a->rank; i++)
    printf(" a->dim[%d].extent is : %d \n", i, a->dim[i].extent);
#endif  

  assert(a->rank == 3);

  const char * src = "ABCDEFGHIJKL";
#if _DEBUG    
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1,  a->dim[1].extent, a->dim[1].lower_bound, a->dim[1].sm);
#endif
  assert(a->dim[0].lower_bound == 0);
  assert(a->dim[0].extent == 2);
  assert(a->dim[0].sm == 1);
  assert(a->dim[1].lower_bound == 0);
  assert(a->dim[1].extent == 2);
  assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);
  assert(a->dim[2].lower_bound == 0);
  assert(a->dim[2].extent == 3);
  assert(a->dim[2].sm == a->dim[1].extent*a->dim[1].sm);

  char *address;
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
        address = (char *) CFI_address(a, subscripts);
        int index = (a->dim[1].extent*k*a->dim[0].extent  + a->dim[0].extent * j + i)*(a->elem_len);
        assert( memcmp(address, src+index, 1) == 0);
      }
    }
  }

  return;

}

