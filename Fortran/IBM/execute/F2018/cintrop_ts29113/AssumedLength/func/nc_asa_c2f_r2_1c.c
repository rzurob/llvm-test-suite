#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"
// Prototype for the function that is defined on the Fortran side.

void check_c_to_f(CFI_cdesc_t* a, int * c_len,int * extent, int *test_no);
void check_c_to_f_to_f(CFI_cdesc_t* a,int * c_len, int * extent, int *test_no);
void check_c_to_f_to_c(CFI_cdesc_t* a, int * c_len,int * extent, int *test_no);

int main()
{
    CFI_cdesc_t* a;
    int c_len, test_no, rc, extent; 
    char *p = "C2C2C2C2";
    CFI_index_t extents[2] = {2,2};
    CFI_CDESC_T(2) at1;
    a = (CFI_cdesc_t *) &at1;


    p = "C2C2C2C2";
    rc = CFI_establish(a,p,CFI_attribute_other,CFI_type_signed_char,2,2,extents);
    assert(rc == CFI_SUCCESS);
    c_len = 2;
    test_no = 1;
    extent = 2;
    check_c_to_f(a, &c_len, &extent, &test_no);


    p = "CFCFCFCF";
    rc = CFI_establish(a,p,CFI_attribute_other,CFI_type_signed_char,2,2,extents);
    assert(rc == CFI_SUCCESS);
    c_len = 2;
    test_no = 2;
    extent = 2;
    check_c_to_f_to_f(a, &c_len,&extent, &test_no);   


    p = "FCFCFCFC";
    rc = CFI_establish(a,p,CFI_attribute_other,CFI_type_signed_char,2,2,extents);
    assert(rc == CFI_SUCCESS);
    c_len = 2;
    test_no = 3;
    extent = 2;
    check_c_to_f_to_c(a, &c_len,&extent, &test_no); 

    return 0;
}

void check_f_to_c(CFI_cdesc_t* a, int *c_len, int *extent, int *test_no)
{
  assert(a->base_addr != NULL);
  assert(a->elem_len  == *c_len);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 2);
  assert(a->dim[0].extent == *c_len);
  assert(a->dim[0].lower_bound == 0);
  assert(a->dim[0].sm == *c_len);
  assert(a->dim[1].extent == -1);
  assert(a->dim[1].lower_bound == 0);
  assert(a->dim[1].sm == (*c_len)*(a->dim[0].extent));
  assert(CFI_is_contiguous(a)==1);

  const char * src;
  switch(*test_no)
  {
    case 1:
        src = "C2C2C2C2";
        break;
    case 2:
        src = "CFCFCFCF";
        break;
    case 3:
        src = "FCFCFCFC";
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

