#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"
// Prototype for the function that is defined on the Fortran side.

void check_c_to_f_in(CFI_cdesc_t* a);
void check_c_to_f_out(CFI_cdesc_t* a);
void check_c_to_f_inout(CFI_cdesc_t* a);
int main()
{
    int rc, c_len, test_no;
    CFI_cdesc_t* a; 
    CFI_CDESC_T(0) at;
    a = (CFI_cdesc_t *) &at;

    char p[5] = {'C','2','F','_' , '_'}; 
    char q[5] = {'C','2','F','_' , '_'};
    char r[5] = {'C','2','F','_' , '_'}; 
    rc = CFI_establish(a,p, CFI_attribute_other,CFI_type_signed_char,5,0,0);
    assert(rc == CFI_SUCCESS);
    check_c_to_f_in(a);
    

    rc = CFI_establish(a,q,CFI_attribute_other,CFI_type_signed_char,5,0,0);
    assert(rc == CFI_SUCCESS);
    check_c_to_f_out(a);


    rc = CFI_establish(a,r,CFI_attribute_other,CFI_type_signed_char,5,0,0);
    assert(rc == CFI_SUCCESS);
    check_c_to_f_inout(a);
    return 0;
}
