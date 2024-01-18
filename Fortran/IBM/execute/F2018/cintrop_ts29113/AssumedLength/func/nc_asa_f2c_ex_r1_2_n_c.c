#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void check_f_to_f(CFI_cdesc_t* a, int *c_len, int *n, int *extent, int *test_no);
void check_f_to_c(CFI_cdesc_t* a, int *c_len, int *n, int *extent, int *test_no)
{
  assert(a->base_addr != NULL);
  assert(CFI_is_contiguous(a)==1);
  assert(a->elem_len  == *c_len); 
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 1);
  assert(a->dim[0].extent == -1);
  assert(a->dim[0].lower_bound == 0);
  assert(a->dim[0].sm == *c_len);
  
  const char * src;
  switch(*test_no) 
  {
    case 1:
        src = "F2C__";
        break;
    case 2:
        src = "F2F__";
        break;
    case 3:
        src = "F2F2C";
        break;
    default :
        src = "_____";
        break;
  }


  #if _DEBUG
     printf(" content of a is : ");
  #endif
  char *p = (char *) a->base_addr;
  for(int i=0; i< *extent; i++){
  #if _DEBUG
      for(int j=0; j<a->dim[0].sm; j++){
           printf("%c", p[j]);
      }
      printf("\n");
  #endif
      assert( memcmp(p, src, a->elem_len) == 0);
      p =p+a->dim[0].sm;
  }
  return;
}
void check_f_to_c_to_f(CFI_cdesc_t* a, int *c_len, int *n, int *extent, int *test_no)
{
   check_f_to_f(a,c_len,n, extent, test_no);
   return;
}
