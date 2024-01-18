#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define true 1
#define false 0 
#define dummy 0 

// Prototype for the functions that are defined on the Fortran side.
void sub_int(CFI_cdesc_t *arg, _Bool *flag, int *init);
void get_val(CFI_cdesc_t *arg, int *val);

void check_allocation(_Bool l1 , _Bool l2)
{
  assert( l1 == l2);
}

void check_value(int i1 , int i2)
{
  assert( i1 == i2);
}

int main(int argc, char ** argv)
{   

   CFI_CDESC_T(0) i1;         
   CFI_cdesc_t * desc_i1 = (CFI_cdesc_t *) &i1;
   int rc, value;
   _Bool test;

   rc = CFI_establish( (CFI_cdesc_t *) &i1,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_int,
                      sizeof(int),
                      0,
                      NULL);

   assert(CFI_SUCCESS == rc); 
   assert(i1.attribute == CFI_attribute_allocatable);
   assert(i1.type == CFI_type_int);

   // calling the Fortran subroutine with non-allocated var 
   sub_int( (CFI_cdesc_t *) &i1, &test, NULL);
   check_allocation(test, false);    
 
   // allocate the object on the C side
   rc = CFI_allocate( (CFI_cdesc_t *) &i1, dummy, dummy, dummy);
   assert(CFI_SUCCESS == rc); 
   assert(desc_i1->base_addr);
   int *actual_i1 = i1.base_addr;

   // Initialize the object on the Fortran side
   value = -99;
   get_val( (CFI_cdesc_t *) &i1, &value ); 
   check_value(*actual_i1, value);    

   // calling the Fortran subroutine with allocated var 
   sub_int( (CFI_cdesc_t *) &i1, &test, &value);
   check_allocation(test, true);    
   check_value(*actual_i1, value);    

   // try a different value
   value = 10507;
   get_val( (CFI_cdesc_t *) &i1, &value ); 
   check_value(*actual_i1, value);    

   // calling the Fortran subroutine with allocated var 
   sub_int( (CFI_cdesc_t *) &i1, &test, &value);
   check_allocation(test, true);    
   check_value(*actual_i1, value);    

   // deallocate the object on the C side
   rc = CFI_deallocate( (CFI_cdesc_t *) &i1);
   assert(CFI_SUCCESS == rc); 

   // calling the Fortran subroutine with non-allocated var 
   sub_int( (CFI_cdesc_t *) &i1, &test, &value);
   check_allocation(test, false);    

   return 0;
}
