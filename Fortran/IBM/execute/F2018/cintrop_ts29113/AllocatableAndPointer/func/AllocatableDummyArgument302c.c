#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

typedef CFI_CDESC_T(0) CFI_cdesc_scalar_t;

// Prototypes for the functions that are defined on the Fortran side.
void sub_alloc(CFI_cdesc_t *arg, int *init);
void sub_alloc_clean(CFI_cdesc_t *arg, int *init);
void sub_dealloc(CFI_cdesc_t *arg);
void sub_dealloc_clean(CFI_cdesc_t *arg);
int compute(int *i1, CFI_cdesc_t *arg); 

void check_value(int i1 , int i2)
{
  assert( i1 == i2);
}

int main(int argc, char ** argv)
{   

   CFI_cdesc_scalar_t i1;         
   CFI_cdesc_t * desc_i1 = (CFI_cdesc_t *) &i1; 

   int rc, value;

    rc = CFI_establish(desc_i1,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_int,
                      sizeof(int),
                      0,
                      NULL);

   assert(CFI_SUCCESS == rc); 
   assert(CFI_SUCCESS == rc);
   assert(i1.attribute == CFI_attribute_allocatable);
   assert(i1.type == CFI_type_int);

// Test 1: use the Fortran intrinsic "allocated" to check allocation status before allocation/deallocation

   // calling the Fortran subroutine with a non-allocated var and allocate it on the Fortran side
   value = -99;
   sub_alloc(desc_i1, &value);
   int *actual_i1 = i1.base_addr;

   // Check on the C side if i1 is allocated 
   assert(desc_i1->base_addr);
   // and has the right value 
   check_value(*actual_i1, value);
   printf("The value is: %d, it should be: %d\n", *actual_i1, value);

   // Use it on the Fortran side. Dummy argument is optional 
    rc = compute(&value, NULL);
    assert( rc == 0);
    
    rc = compute(&value, desc_i1);
    assert( rc == 2*value);
    
    value = 401;
    rc = compute(&value, desc_i1);
    assert( rc == -99);
    
   // calling the Fortran subroutine with an allocated var and deallocate it on the Fortran side
   sub_dealloc(desc_i1);

// Test 2: Use the error recovery for allocate / deallocate 

   // calling the Fortran subroutine with a non-allocated var and allocate it on the Fortran side
   value = 55;
   sub_alloc_clean((CFI_cdesc_t *) &i1, &value);
   int *actual_i1_2 = i1.base_addr;

   // Check on the C side if i1 is allocated 
   assert(desc_i1->base_addr);
   // and has the right value 
   check_value(*actual_i1_2, value);
   printf("The value is: %d, it should be: %d\n", *actual_i1_2, value);

   // Use it on the Fortran side. Dummy argument is optional 
    rc = compute(&value, NULL);
    assert( rc == 0);
    
    rc = compute(&value, desc_i1);
    assert( rc == 2*value);
    
    value = -99;
    rc = compute(&value, desc_i1);
    assert( rc == 55);
    
    // calling the Fortran subroutine with an allocated var and deallocate it on the Fortran side
    sub_dealloc_clean( (CFI_cdesc_t *) &i1);

   return 0;
}
