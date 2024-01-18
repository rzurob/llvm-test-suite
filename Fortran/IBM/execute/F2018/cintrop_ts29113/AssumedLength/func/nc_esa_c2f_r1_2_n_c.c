#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"
// Prototype for the function that is defined on the Fortran side.

void check_c_to_f(CFI_cdesc_t* a,int *n, int * c_len, int *test_no);
void check_c_to_f_to_f(CFI_cdesc_t* a,int *n, int * c_len, int *test_no);
void check_c_to_f_to_c(CFI_cdesc_t* a, int *n, int * c_len, int *test_no);

int main()
{
    CFI_cdesc_t* a;
    int c_len, test_no, rc, n; 
    char *p = "C2F__";
    CFI_index_t extends[1] = {5};
    CFI_CDESC_T(1) at1;
    a = (CFI_cdesc_t *) &at1;


    p = "C2F__C2F__C2F__C2F__C2F__";
    rc = CFI_establish(a,p,CFI_attribute_other,CFI_type_signed_char,5,1,extends);
    assert(rc == CFI_SUCCESS);
    c_len = 5;
    test_no = 1;
    n = 4;
    check_c_to_f(a, &n, &c_len, &test_no);


    p = "C2F2FC2F2FC2F2FC2F2FC2F2F";
    rc = CFI_establish(a,p,CFI_attribute_other,CFI_type_signed_char,5,1,extends);
    assert(rc == CFI_SUCCESS);
    c_len = 5;
    test_no = 2;
    n = 4;
    check_c_to_f_to_f(a,&n, &c_len, &test_no);   


    p = "C2F2CC2F2CC2F2CC2F2CC2F2C";
    rc = CFI_establish(a,p,CFI_attribute_other,CFI_type_signed_char,5,1,extends);
    assert(rc == CFI_SUCCESS);
    c_len = 5;
    test_no = 3;
    n = 4;
    check_c_to_f_to_c(a,&n, &c_len, &test_no); 

    return 0;
}
void check_f_to_c(CFI_cdesc_t* a, int *n, int *c_len, int *test_no)
{
  assert(a->base_addr != NULL);
  assert(a->elem_len  == *c_len);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 1);
  assert(a->dim[0].extent == *n);
  assert(a->dim[0].lower_bound == 0);
  assert(a->dim[0].sm == *c_len);
  assert(CFI_is_contiguous(a)==1);

  const char * src;
  switch(*test_no)
  {
    case 3:
        src = "C2F2C";
        break;
    default :
        src = "_____";
        break;
  }
  
  #if _DEBUG
     printf(" content of a is : ");
  #endif
  char *p = (char *) a->base_addr;
  for(int i=0; i< a->dim[0].extent; i++){
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

