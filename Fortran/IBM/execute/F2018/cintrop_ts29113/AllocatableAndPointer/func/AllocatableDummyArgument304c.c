#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <float.h>
#include <math.h>
#include "ISO_Fortran_binding.h"

#define dummy 0

typedef CFI_CDESC_T(0) CFI_cdesc_scalar_t;

// Prototypes for the functions that are defined on the Fortran side.
void sub_dealloc(CFI_cdesc_t *arg);
void sub_dealloc_clean(CFI_cdesc_t *arg);
void set_value(CFI_cdesc_t *a1, CFI_cdesc_t *a2, CFI_cdesc_t *a3, CFI_cdesc_t *a4, CFI_cdesc_t *a5, double *v1, double *v2, double *v3, double *v4, double *v5);
void compute(CFI_cdesc_t *arg1, CFI_cdesc_t *arg2);

void check_value(double d1 , double d2)
{
  assert( fabs(d1-d2) <= DBL_EPSILON );
}

int main(int argc, char ** argv)
{   

   CFI_cdesc_scalar_t d1, d2;         
   CFI_cdesc_t * desc_d1 = (CFI_cdesc_t *) &d1; 
   CFI_cdesc_t * desc_d2 = (CFI_cdesc_t *) &d2; 

   int rc;
   double tmp1, tmp2;

   rc = CFI_establish(desc_d1,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_double,
                      dummy,
                      0,
                      dummy);

   assert(CFI_SUCCESS == rc); 
   assert(d1.attribute == CFI_attribute_allocatable);
   assert(d1.type == CFI_type_double);

   rc = CFI_establish(desc_d2,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_double,
                      dummy,
                      0,
                      dummy);

   assert(rc == CFI_SUCCESS);
   assert(d2.attribute == CFI_attribute_allocatable);
   assert(d2.type == CFI_type_double);

   // allocate d1 and d2 on the C side
   rc = CFI_allocate( (CFI_cdesc_t *) &d1, dummy, dummy, dummy);
   assert(rc == CFI_SUCCESS);
   rc = CFI_allocate( (CFI_cdesc_t *) &d2, dummy, dummy, dummy);
   assert(rc == CFI_SUCCESS);
   double *actual_d1 = d1.base_addr;
   double *actual_d2 = d2.base_addr;

   // Check on the C side if d1 and d2 are allocated 
   assert(desc_d1->base_addr);
   assert(desc_d2->base_addr);

   // Set the values on the Fortran side 
   tmp1 = -60.0f;
   tmp2 = -50.0f;
   set_value(desc_d1, NULL, desc_d2, NULL, NULL, &tmp1, NULL, &tmp2, NULL, NULL);

   // Check the values on the C-side 
   check_value(*actual_d1, tmp1);
   printf("The value is: %f, it should be: %f\n", *actual_d1, tmp1);
   check_value(*actual_d2, tmp2);
   printf("The value is: %f, it should be: %f\n", *actual_d2, tmp2);
   fflush(stdout);

   // Do some computations on the Fortran side. 
   compute(desc_d2, desc_d1);
    
   // Work is done. Time to deallocate on the Fortran side 
   sub_dealloc(desc_d1);
   sub_dealloc_clean(desc_d2);

   return 0;
}
