#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

// Prototype for the functions that are defined on the Fortran side.
void sub_alloc(float *val, CFI_cdesc_t *arg);  // first dummy is optional 
void sub_alloc_clean(float *val, CFI_cdesc_t *arg);
float compute(CFI_cdesc_t *arg1, CFI_cdesc_t *arg2); 

void check_value(float r1 , float r2)
{
  assert( fabs(r1-r2) <= FLT_EPSILON );
}

int main(int argc, char ** argv)
{   

   CFI_CDESC_T(0) r1, r2;         
   CFI_cdesc_t * desc_r1 = (CFI_cdesc_t *) &r1;
   CFI_cdesc_t * desc_r2 = (CFI_cdesc_t *) &r2;
   float value;
   int rc;

   rc = CFI_establish(desc_r1,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_float,
                      NULL,
                      0,
                      NULL);

   assert(rc == CFI_SUCCESS); 
   assert(r1.attribute == CFI_attribute_allocatable);
   assert(r1.type == CFI_type_float);

   // calling the Fortran subroutine with non-allocated var and allocate it on the Fortran side
   sub_alloc(NULL, desc_r1);

   // Check on the C side if r1 is allocated 
   assert(desc_r1->base_addr);
   // and has the right value  /* use CFI_address */
   float *address;
   address = (float*) CFI_address(desc_r1, NULL);
   printf("The value is: %f, it should be: -88.000000\n", *address);
   check_value(*address, -88.0);

   // deallocate on the C side 
   rc = CFI_deallocate(desc_r1);
   assert(rc == CFI_SUCCESS); 

   // calling the Fortran subroutine with a non-allocated var and allocate it on the Fortran side
   value = -99.0;
   sub_alloc(&value, desc_r1);

   // Check on the C side if r1 is allocated 
   assert(desc_r1->base_addr);
   // and has the right value 
   printf("The value is: %f, it should be: %f\n", *(float *) r1.base_addr, value);
   check_value(*(float *) r1.base_addr, value);

   rc = CFI_establish(desc_r2,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_float,
                      NULL,
                      0,
                      NULL);

   assert(rc == CFI_SUCCESS); 
   assert(r2.attribute == CFI_attribute_allocatable);
   assert(r2.type == CFI_type_float);

   // calling the Fortran subroutine with a non-allocated var and allocate it on the Fortran side
   value = 55.0;
   sub_alloc_clean(&value, (CFI_cdesc_t *) &r2);
   float *actual_r2 = r2.base_addr;

   // Check on the C side if r1 is allocated 
   assert(desc_r2->base_addr);
   // and has the right value 
   printf("The value is: %f, it should be: %f\n", *actual_r2, value);
   check_value(*actual_r2, value);

   // Use it on the Fortran side. Dummy argument is optional 
   float test = compute(desc_r2, desc_r1);
   check_value(test, -99.0*55.0);

   // deallocate both r1 and r2 on the C side 
   rc = CFI_deallocate(desc_r1);
   assert(rc == CFI_SUCCESS); 

   rc = CFI_deallocate(desc_r2);
   assert(rc == CFI_SUCCESS); 

   return 0;
}
