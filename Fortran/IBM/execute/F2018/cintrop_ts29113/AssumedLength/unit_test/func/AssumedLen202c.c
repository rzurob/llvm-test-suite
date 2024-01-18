#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void sub_2(CFI_cdesc_t * a)
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

#if _DEBUG
  char *p = (char *) a->base_addr;
  printf(" content of a is : ");

  int num_elem = 1;
  for (int i = 0 ; i < (a->rank) ; i++)
  {
    num_elem = num_elem * a->dim[i].extent;
  };

  for (int j = 0 ; j < num_elem ; j++)
  {
    for (int l=0 ; l < a->elem_len ; l++)
    {
      printf(" %c ", p[l + (a->elem_len)*j]);
    };
  };
#endif

  assert(a->rank == 2);

  const char * src = "AAABBBCCCDDDEEEFFF";
#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0,  a->dim[0].extent, a->dim[0].lower_bound, a->dim[0].sm);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1,  a->dim[1].extent, a->dim[1].lower_bound, a->dim[1].sm);
#endif 
  assert(a->dim[0].lower_bound == 0);
  assert(a->dim[0].extent == 2);
  assert(a->dim[0].sm == 3);
  assert(a->dim[1].lower_bound == 0);
  assert(a->dim[1].extent == 3);
  assert(a->dim[1].sm == a->dim[0].extent*a->dim[0].sm);

  char *address;
  CFI_index_t subscripts[2];
  for (int j=a->dim[1].lower_bound; j<(a->dim[1].lower_bound+a->dim[1].extent); j++)
  {
    subscripts[1] = j;
    for (int k=a->dim[0].lower_bound; k<(a->dim[0].lower_bound+a->dim[0].extent); k++)
    {
      subscripts[0] = k;
      address = (char *) CFI_address(a, subscripts);
      int index = (a->dim[0].extent * j + k)*(a->elem_len);
      assert( memcmp(address, src+index, 3) == 0);
    }
  }   
  
  return;

}

