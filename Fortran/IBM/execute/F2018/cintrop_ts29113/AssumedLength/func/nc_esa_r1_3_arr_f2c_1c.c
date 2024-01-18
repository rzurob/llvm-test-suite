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
  assert(a->dim[0].extent == *c_len);
  assert(a->dim[0].lower_bound == 0);
  assert(a->dim[0].sm == *c_len);
  assert(CFI_is_contiguous(a)==1);

  const char * src;
  switch(*test_no) 
  {
    case 1:
        src = "ONES";
        break;
    case 2:
        src = "TWOS";
        break;
    case 3:
        src = "THREES";
        break;
    case 4:
        src = "FOURS";
        break;
    case 5:
        src = "FIVES  ";
        break;
    case 6:
        src = "SIXES";
        break;
    case 7:
        src = "SEVENS";
        break;
    case 8:
        src = "EIGHTS";
        break;
    case 9:
        src = "NINES";
        break;
    case 10:
        src = "TENS  ";
        break;
    case 11:
        src = "ELEVENS";
        break;
    case 12:
        src = "NINES";
        break;
    case 13:
        src = "NINES";
        break;
    case 14:
        src = "ELEVENS";
        break;
    case 15:
        src = "FIFTEEN";
        break;
    case 16:
        src = "SIXTEEN";
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
        src = "TWENTY_ONE";
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
        src = "TWENTY_SIX";
        break;
    case 27:
        src = "TWENTY_SEVENS";
        break;
    case 28:
        src = "TWENTY_EIGHT";
        break;
    case 29:
        src = "TWENTY_NINE";
        break;
    case 30:
        src = "THIRTY";
        break;
    case 31:
        src = "THIRTY_ONE";
        break;
    case 32:
        src = "THIRTY_TWO";
        break;

    default :
        src = "_____";
        break;
  }

  char *p = (char *) a->base_addr;
  #if _DEBUG
  printf("content of a is :\n");
  for(int i=0; i< a->dim[0].extent; i++){
    printf("Dim 0 Element# %d : ", i);
    for(int j=0; j<a->dim[0].sm; j++){
      printf("%c", p[j]);
    }
      printf("\n");
      p = p+a->elem_len;
  }
  #endif
  p = (char *) a->base_addr;
  for(int i=0; i< a->dim[0].extent; i++){
     assert(memcmp(p, src, a->elem_len) == 0);
     p = p+a->elem_len;
  } 
  return;
}
