#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"
// Prototype for the function that is defined on the Fortran side.

void check_c_to_f(CFI_cdesc_t* a, int * c_len, int *test_no);
void check_c_to_f_to_f(CFI_cdesc_t* a,int * c_len, int *test_no);
void check_c_to_f_to_c(CFI_cdesc_t* a, int * c_len, int *test_no);

int main()
{
    CFI_cdesc_t* a;
    int c_len, test_no, rc, extent;
    char *p = "C2F__";
    CFI_index_t extents[1] = {5};
    CFI_CDESC_T(1) at1;
    a = (CFI_cdesc_t *) &at1;
    p = "C2F__C2F__C2F__C2F__C2F__";
    rc = CFI_establish(a,p,CFI_attribute_other,CFI_type_signed_char,1,1,extents);
    assert(rc == CFI_SUCCESS);
    c_len = 5;
    test_no = 1;
    extent = 5;
    _xlfdmprtd_cfi(a, 0);
    check_c_to_f(a, &c_len, &test_no);
    return 0;
}
