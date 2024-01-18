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
      fprintf(stderr, "Expected array of type CFI_type_int_fast8_t...\n");
      exit(2);
  }


  const char * src = "IBM2014";
  char *p = (char *) a->base_addr;

#if _DEBUG
  printf(" element length of a is :  %d  \n" , a->elem_len);
  for (int i=0; i < a->elem_len; ++i)
    printf(" %c ", p[i]); 
  
  printf("\n"); 
#endif
  
  assert( a->rank == 0);
  assert( memcmp(p, src, a->elem_len) == 0);

  return;

}
