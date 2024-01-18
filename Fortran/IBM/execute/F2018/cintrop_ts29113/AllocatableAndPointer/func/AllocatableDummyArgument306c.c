#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define ARR_RANK 5

typedef CFI_CDESC_T(ARR_RANK) CFI_cdesc_5D_t;

// Prototypes for the functions that are defined on the Fortran side.
void sub_alloc(CFI_cdesc_t *arg);
void sub_alloc_clean(CFI_cdesc_t *arg);
void sub_dealloc(CFI_cdesc_t *arg);
void sub_dealloc_clean(CFI_cdesc_t *arg);
double compute(CFI_cdesc_t *arg); 

int main(int argc, char ** argv)
{   

   CFI_cdesc_5D_t Arr5D;         
   CFI_cdesc_t * desc_Arr5D = (CFI_cdesc_t *) &Arr5D; 
   CFI_index_t lower[ARR_RANK], upper[ARR_RANK];
   CFI_index_t extents[ARR_RANK], strides[ARR_RANK];
   CFI_rank_t rank = ARR_RANK;


   int rc, n; 
   double d; 

   rc = CFI_establish(desc_Arr5D,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_double,
                      sizeof(double),
                      rank,
                      NULL);

   assert(CFI_SUCCESS == rc); 
   assert(Arr5D.attribute == CFI_attribute_allocatable);
   assert(Arr5D.type == CFI_type_double);
   assert(Arr5D.rank == rank);

// Test 1: use the Fortran intrinsic "allocated" to check allocation status before allocation/deallocation

   // calling the Fortran subroutine with a non-allocated var and allocate it on the Fortran side
   sub_alloc(desc_Arr5D);
   fflush(stdout);
   double *actual_Arr5D = Arr5D.base_addr;
   double *address; 
   CFI_index_t subscripts[ARR_RANK];

   // Check on the C side if Arr5D is allocated 
   assert(desc_Arr5D->base_addr);
   // verify the extent and the lower bounds 

   for (int i=0; i<ARR_RANK; i++) {
     extents[i] = desc_Arr5D->dim[i].extent;
     strides[i] = desc_Arr5D->dim[i].sm;
     lower[i] = desc_Arr5D->dim[i].lower_bound;
     //printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", i, extents[i], lower[i], strides[i]);
   }

   assert(extents[0] == 6);
   assert(extents[1] == 5);
   assert(extents[2] == 4);
   assert(extents[3] == 3);
   assert(extents[4] == 2);

   assert(strides[0] == 8);
   assert(strides[1] == extents[0]*strides[0]);
   assert(strides[2] == extents[1]*strides[1]);
   assert(strides[3] == extents[2]*strides[2]);
   assert(strides[4] == extents[3]*strides[3]);

   assert(lower[0] == 2);
   assert(lower[1] == 3);
   assert(lower[2] == 4);
   assert(lower[3] == 1);
   assert(lower[4] == -1);

   // verify the values
   // loop on the last dimension first to match Fortran column-major layout 
   for (int m=lower[4]; m<(lower[4]+extents[4]); m++) {
       subscripts[4] = m;
       for (int l=lower[3]; l<(lower[3]+extents[3]); l++) {
           subscripts[3] = l;
           for (int k=lower[2]; k<(lower[2]+extents[2]); k++) {
               subscripts[2] = k;
               for (int j=lower[1]; j<(lower[1]+extents[1]); j++) {
                   subscripts[1] = j;
                   for (int i=lower[0]; i<(lower[0]+extents[0]); i++) {
                       subscripts[0] = i;
                       address = (double*) CFI_address(desc_Arr5D, subscripts);
                       printf("the value is: %f\n", *address);
               }
            }
         }
       }
    }

    // Use it on the Fortran side. Dummy argument is optional 
    d = compute(NULL);
    assert(d == 0);
    
    d = compute(desc_Arr5D);
    assert(d == 259560.000);
    
    // calling the Fortran subroutine with an allocated var and deallocate it on the Fortran side
    sub_dealloc(desc_Arr5D);

// Test 2: Use the error recovery for allocate / deallocate 

    // calling the Fortran subroutine with a non-allocated var and allocate it on the Fortran side
    sub_alloc_clean((CFI_cdesc_t *) &Arr5D);

    // Check on the C side if Arr5D is allocated 
    assert(desc_Arr5D->base_addr);
    // and has the right value 

    // Use it on the Fortran side. Dummy argument is optional 
    d = compute(NULL);
    assert(d == 0);
    
    d = compute(desc_Arr5D);
    assert(d == 259560.000);
    
    // calling the Fortran subroutine with an allocated var and deallocate it on the Fortran side
    sub_dealloc_clean( (CFI_cdesc_t *) &Arr5D);

   return 0;
}
