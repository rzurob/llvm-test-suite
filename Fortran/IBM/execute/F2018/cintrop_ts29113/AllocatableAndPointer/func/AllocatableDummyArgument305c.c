#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define true 1
#define false 0 
#define dummy 0 

// Prototype for the functions that are defined on the Fortran side.
void fsub(CFI_cdesc_t *arg_arr, _Bool *flag, float *init);
void fill_array(CFI_cdesc_t *arg_arr, float *val);

void check_allocation(_Bool l1 , _Bool l2)
{
  assert( l1 == l2);
}

void print_array(float *my_arr, int n)
{
       printf("n: %d\n", n);
  for (int i=0; i<n; i++)
       printf("my_arr[%d]:%f\n", i, my_arr[i]);

}

int main(int argc, char ** argv)
{   

   CFI_CDESC_T(1) my_arr;         
   CFI_cdesc_t * desc_my_arr = (CFI_cdesc_t *) &my_arr;
   CFI_index_t lower[1], upper[1];
   CFI_index_t extents[1], strides[1];
   CFI_rank_t rank = 1;

   int rc, n;
   float value;
   _Bool test;

   rc = CFI_establish( desc_my_arr,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_float,
                      sizeof(float),
                      rank,
                      NULL);

   assert(CFI_SUCCESS == rc); 
   assert(my_arr.attribute == CFI_attribute_allocatable);
   assert(my_arr.type == CFI_type_float);
   assert(my_arr.rank == rank);

   // calling the Fortran subroutine with non-allocated var 
   fsub( (CFI_cdesc_t *) &my_arr, &test, NULL);
   check_allocation(test, false);    
   fflush(stdout);
 
   /*
   Array with lower bound = 1 and upper bound = 20
   */

   // allocate the object on the C side
   lower[0]=1;
   upper[0]=20;
   rc = CFI_allocate( (CFI_cdesc_t *) &my_arr, lower, upper, dummy);
   assert(CFI_SUCCESS == rc); 
   float *actual_arr = my_arr.base_addr;
   fflush(stdout);

   // Initialize the object on the Fortran side
   value = -1.0;
   fill_array( (CFI_cdesc_t *) &my_arr, &value ); 
   n = (upper[0]-lower[0]) + 1;
   print_array(actual_arr, n);    
   fflush(stdout);

   // calling the Fortran subroutine with allocated var 
   fsub( (CFI_cdesc_t *) &my_arr, &test, &value);
   check_allocation(test, true);    
   fflush(stdout);

   // try a different value
   value = 105.07;
   fill_array( (CFI_cdesc_t *) &my_arr, &value ); 
   print_array(actual_arr, n);    
   fflush(stdout);

   // calling the Fortran subroutine with allocated var 
   fsub( (CFI_cdesc_t *) &my_arr, &test, &value);
   check_allocation(test, true);    
   fflush(stdout);

   // deallocate the object on the C side
   rc = CFI_deallocate( (CFI_cdesc_t *) &my_arr);
   assert(CFI_SUCCESS == rc); 

   // calling the Fortran subroutine with non-allocated var 
   fsub( (CFI_cdesc_t *) &my_arr, &test, &value);
   check_allocation(test, false);    
   fflush(stdout);

   /*
   Test different bounds 
   Array with lower bound = -1 and upper bound = +1
   */

   // allocate the object on the C side
   lower[0]=-1;
   upper[0]=1;
   rc = CFI_allocate( (CFI_cdesc_t *) &my_arr, lower, upper, dummy);
   assert(CFI_SUCCESS == rc); 
   float *actual_arr2 = my_arr.base_addr;
   fflush(stdout);

   // Initialize the object on the Fortran side
   value = -99.0;
   fill_array( (CFI_cdesc_t *) &my_arr, &value ); 
   n = (upper[0]-lower[0]) + 1;
   print_array(actual_arr2, n);    
   fflush(stdout);

   // calling the Fortran subroutine with allocated var 
   fsub( (CFI_cdesc_t *) &my_arr, &test, &value);
   check_allocation(test, true);    
   fflush(stdout);

   // deallocate the object on the C side
   rc = CFI_deallocate( (CFI_cdesc_t *) &my_arr);
   assert(CFI_SUCCESS == rc); 

   return 0;
}
