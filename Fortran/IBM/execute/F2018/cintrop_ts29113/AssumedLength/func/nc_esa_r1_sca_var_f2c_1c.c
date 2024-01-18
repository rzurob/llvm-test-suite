#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"
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
  assert(a->rank == 1);
  assert(a->dim[0].extent == 1);
  assert(a->dim[0].lower_bound == 0);
  assert(a->dim[0].sm == *c_len);
  assert(CFI_is_contiguous(a)==1);

  const char * src;
  switch(*test_no) 
  {
    case 1:
        src = "ONE";
        break;
    case 2:
        src = "TWO";
        break;
    case 3:
        src = "THREE";
        break;
    case 4:
        src = "FOUR";
        break;
    case 5:
        src = "FIVE";
        break;
    case 6:
        src = "SIX";
        break;
    case 7:
        src = "SEVEN";
        break;
    case 8:
        src = "EIGHT";
        break;
    case 9:
        src = "NINE";
        break;
    case 10:
        src = "TEN";
        break;
    case 11:
        src = "ELEVEN";
        break;
    case 12:
        src = "TWELVE";
        break;
    case 13:
        src = "THIRTEEN  ";
        break;
    case 14:
        src = "FOURTEEN";
        break;
    case 15:
        src = "EIGHTEEN";
        break;
    case 16:
        src = "SIXTEEN   ";
        break;
    case 17:
        src = "SEVENTEEN";
        break;
    case 18:
        src = "EIGHTEEN";
        break;
    case 19:
        src = "NINETEEN";
        break;
    case 20:
        src = "TWENTY";
        break;
    case 21:
        src = "TWENTY_ONE ";
        break;
    case 22:
        src = "TWENTY_TWO  ";
        break;
    case 23:
        src = "TWENTY_THREE";
        break;
    case 24:
        src = "TWENTY_FOUR";
        break;
    case 25:
        src = "TWENTY_FIVE";
        break;
    case 26:
        src = "TWELVE";
        break;
    case 27:
        src = "TWELVE__";
        break;
    case 28:
        src = "TWELVETWELVE";
        break;
    case 29:
        src = "TWENTY ";
        break;
    case 30:
        src = "TWENTYTWENTY";
        break;
    case 31:
        src = "TWENTY_TWO  TWENTY_ONE ";
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
  assert(memcmp(p, src, a->elem_len) == 0);
  
 
  return;
}
