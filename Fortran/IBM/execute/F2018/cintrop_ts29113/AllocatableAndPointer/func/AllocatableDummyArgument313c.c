#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define dummy 0

// Prototype for the functions that are defined on the Fortran side.
void sub_alloc(CFI_cdesc_t *arg);                    // function to allocate an object with C-desc on the Fortran side
void sub_dealloc(CFI_cdesc_t *arg);                  // function to deallocate an object with C-desc on the Fortran side
void check(CFI_cdesc_t *arg);                        // verify values on the Fortran side
float find_max(CFI_cdesc_t *arg);                    // wrapper for maxval
float find_min(CFI_cdesc_t *arg);                    // wrapper for minval
void locate_max(CFI_cdesc_t *arg);                   // wrapper for maxloc
void locate_min(CFI_cdesc_t *arg);                   // wrapper for minloc

int main(int argc, char ** argv)
{   

   CFI_CDESC_T(2) arr, arr2;         
   CFI_cdesc_t * desc_arr = (CFI_cdesc_t *) &arr;
   CFI_cdesc_t * desc_arr2 = (CFI_cdesc_t *) &arr2;

   CFI_rank_t rank = 2;

   // bounds, strides and extents of the arrays 
   CFI_index_t lb[2], ext[2], str[2];

   // Since Fortran is column-major and C is row-major, the input matrixes
   // defined on the C side are transposed.
   float matrix[3][2] = { {3., -2.} ,{2., 4.}, {1., 1.} };

   int rc;
   char * prow, * pp;
   _Bool test;

   rc = CFI_establish(desc_arr,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_float,
                      dummy,
                      rank,
                      NULL);

   assert(rc == CFI_SUCCESS); 
   assert(arr.attribute == CFI_attribute_allocatable);
   assert(arr.type == CFI_type_float);
   assert(arr.rank == rank);

   // Allocate arr on the Fortran side with the source being arr2
   sub_alloc(desc_arr);
   float *actual_arr = arr.base_addr;

   if( *actual_arr != matrix[0][0] )
     exit(1);
   if( *(actual_arr+1) != matrix[0][1] )
     exit(2);
   if( *(actual_arr+2) != matrix[1][0] )
     exit(3);
   if( *(actual_arr+3) != matrix[1][1] )
     exit(4);
   if( *(actual_arr+4) != matrix[2][0] )
     exit(5);
   if( *(actual_arr+5) != matrix[2][1] )
     exit(6);

   // Verify on the C side if the arrays are allocated 
   assert(desc_arr->base_addr);

   // verify the extent, stride and the lower bounds
   ext[0] = desc_arr->dim[0].extent;
   str[0] = desc_arr->dim[0].sm;
   lb[0] = desc_arr->dim[0].lower_bound;
   ext[1] = desc_arr->dim[1].extent;
   str[1] = desc_arr->dim[1].sm;
   lb[1] = desc_arr->dim[1].lower_bound;

   assert(ext[0] == 2);
   assert(ext[1] == 3);
   assert(str[0] == 4);
   assert(str[1] == ext[0]*str[0]);
   assert(lb[0] == 1);
   assert(lb[1] == 1);

   // verify values on the Fortran side
   check(desc_arr);

   // use Fortran intrinsic array-type functions
   float min, max; 
   max = find_max(desc_arr);
   if( max != 4. )
     exit(7);
   min = find_min(desc_arr);
   if( min != -2. )
     exit(8);
   locate_max(desc_arr);
   locate_min(desc_arr);

   // deallocate arr on the Fortran
   sub_dealloc(desc_arr);

   return 0;
}
