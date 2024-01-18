#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"
// Prototype for the function that is defined on the Fortran side.

void check_c_to_f(CFI_cdesc_t* a, int *c_len, int *test_no);
int main()
{
    int rc, c_len, test_no;
    CFI_cdesc_t* a; 
    CFI_CDESC_T(0) at;
    a = (CFI_cdesc_t *) &at;
    char *p = "C2F__"; 
    rc = CFI_establish(a,
                     p,
		     CFI_attribute_other,
		     CFI_type_signed_char,
		     5,
		     0,
		     0);
    if (rc != CFI_SUCCESS)
    {
          fprintf(stderr, "CFI_establish returns : %s\n", __xlf_CFI_strerror(rc));
    }
    c_len = 5;
    test_no = 1;
    check_c_to_f(a, &c_len, &test_no);
    
    p = "C2F2F";
    rc = CFI_establish(a,
                     p,
                     CFI_attribute_other,
                     CFI_type_signed_char,
                     5,
                     0,
                     0);

     if (rc != CFI_SUCCESS)
     {
          fprintf(stderr, "CFI_establish returns : %s\n", __xlf_CFI_strerror(rc));
     }
    c_len = 5;
    test_no = 2;
    check_c_to_f_to_f(a, &c_len, &test_no);

    p = "C2F2C";
    rc = CFI_establish(a,
                     p,
                     CFI_attribute_other,
                     CFI_type_signed_char,
                     5,
                     0,
                     0);

     if (rc != CFI_SUCCESS)
     {
          fprintf(stderr, "CFI_establish returns : %s\n", __xlf_CFI_strerror(rc));
     }
    c_len = 5;
    test_no = 3;
    check_c_to_f_to_c(a, &c_len, &test_no);
    return 0;
}

void check_f_to_c(CFI_cdesc_t* a, int *c_len, int *test_no)
{
  assert(a->base_addr != NULL);
  assert(a->elem_len  == *c_len);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 1);
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

