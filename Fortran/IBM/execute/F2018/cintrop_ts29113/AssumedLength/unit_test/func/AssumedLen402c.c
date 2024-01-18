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


#if _DEBUG
  char *p = (char *) a->base_addr;
  printf(" content of a is : ");

  int num_elem = 1;
  for (int i = 0 ; i < (a->rank) ; i++)
  {
    num_elem = num_elem * a->dim[i].extent;
  };

  printf ("\n num_elem = %d \n", num_elem);
  printf("elem_len = %d", a->elem_len);

  for (int j = 0 ; j < num_elem ; j++)
  {
    printf("%d : ", j);
    for (int l=0 ; l < a->elem_len ; l++)
    {
      printf(" %c ", p[l + (a->elem_len)*j]);
    };
     printf("\n");
 
  };
 
  printf ("\n");
#endif

  assert(a->elem_len == 2);  
 
  if (a->rank == 2)
  {
    const char * src = "AACCEEGG";
    assert(a->dim[0].lower_bound == 0);
    assert(a->dim[0].extent == 2);
    assert(a->dim[0].sm == 2);
    assert(a->dim[1].lower_bound == 0);
    assert(a->dim[1].extent == 2);
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
        assert( memcmp(address, src+index, a->elem_len) == 0);
      }
    }   
  }

  return;

}

