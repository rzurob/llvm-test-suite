#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"
void check_f_to_f(CFI_cdesc_t* a, int *c_len, int *test_no);
void check_f_to_c(CFI_cdesc_t* a, int *c_len, int *test_no)
{
  if(a == NULL){
      printf("Optional argument not provided");
      return;
  }
  assert(a->base_addr != NULL);
  assert(a->elem_len  == *c_len); 
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 0);
  const char * src;
  switch(*test_no) 
  {
    case 1:
        src = " ";
        break;
    case 2:
        src = "ABC 123 \"test\"";
        break;
    case 3:
        src = "123 abc 'test'";
        break;
    case 4:
        src = "Test \'apostrophes\'";
        break;
    case 5:
        src = "Test \"double quotation\"";
        break;
    case 6:
        src = "Test newline\n";
        break;
    case 7:
        src = "a";
        break;
    case 8:
        src = "b";
        break;
    case 9:
        src = "a\bcde\fg";
        break;
    case 10:
        src = "ABCDEFGHIJKL";
        break;
    case 11:
        src = "EFGH";
        break;
    case 12:
        src = "ABC123";
        break;
    case 13:
        src = "ABC12";
        break;
    default :
        src = "_____";
        break;
  }

  char *p = (char *) a->base_addr;

  #if _DEBUG
  if(a->elem_len >0)
  {
     printf("Content of a is : ");
     char *p = (char *) a->base_addr;
     for(int i=0;i<a->elem_len;i++)
     {
         printf("%c", p[i]);
     }
     printf("\n");
  }
  else
     printf("Content of a is  empty string \n");
  #endif
  assert( memcmp(p, src, a->elem_len) == 0);
  return;
}
void check_f_to_c_to_f(CFI_cdesc_t* a, int *c_len, int *test_no)
{
  if(a == NULL){
      printf("Optional argument not provided");
      return;
  }
  check_f_to_f(a, c_len, test_no);
  return;
}





