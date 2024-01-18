#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

typedef CFI_CDESC_T(0) CFI_cdesc_scalar_t;

// Prototypes for the functions that are defined on the Fortran side.
void fassoc(CFI_cdesc_t *arg, int init);
void fnullify(CFI_cdesc_t *arg);

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
                      CFI_attribute_pointer,
                      CFI_type_int,
                      sizeof(int),
                      0,
                      NULL);

   assert(CFI_SUCCESS == rc); 
   assert(CFI_SUCCESS == rc);
   assert(i1.attribute == CFI_attribute_pointer);
   assert(i1.type == CFI_type_int);

   // calling the Fortran subroutine with a non-associated var and associate it using pointer assignement on the Fortran side
   value = -99;
   fassoc(desc_i1, value);

   // calling the Fortran subroutine with an assoociated var and nullify it on the Fortran side
   fnullify(desc_i1);

   return 0;
}
