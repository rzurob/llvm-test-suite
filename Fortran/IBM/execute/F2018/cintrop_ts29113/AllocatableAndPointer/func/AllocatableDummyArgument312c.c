#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define dummy 0

// Prototype for the functions that are defined on the Fortran side.
void alloc_new_obj(CFI_cdesc_t *arg);                // allocate a new Fortran object on the Fortran side using arg as source
void associate_new_obj(CFI_cdesc_t *arg);            // create a new Fortran pointer on the Fortran side using arg as source
void check_all(CFI_cdesc_t *arg);                    // verify allocation status, size, rank and bounds

int main(int argc, char ** argv)
{   

   CFI_CDESC_T(2) arr;         
   CFI_cdesc_t * desc_arr = (CFI_cdesc_t *) &arr;

   CFI_rank_t rank = 2;

   // bounds, strides and extents of the arrays 
   CFI_index_t lb[2] = {1, 1};  // array of 2 rows and 3 columns
   CFI_index_t ub[2] = {2, 3};  
   CFI_index_t ext[2], str[2];

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

   // Allocate arr on the C side
   rc = CFI_allocate( (CFI_cdesc_t *) &arr, lb, ub, dummy);
   assert(rc == CFI_SUCCESS); 
   assert(desc_arr->base_addr);

   float *actual_arr = arr.base_addr;
   *actual_arr = matrix[0][0];
   *(actual_arr+1) = matrix[0][1];
   *(actual_arr+2) = matrix[1][0];
   *(actual_arr+3) = matrix[1][1];
   *(actual_arr+4) = matrix[2][0];
   *(actual_arr+5) = matrix[2][1];

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

   // print the values of the array one row at a time
   printf("printing arr in C\n");
   prow = (char *)desc_arr->base_addr;
   for(int i = 0; i < ext[0]; i++)
   {
     pp = prow;
     for(int j = 0; j < ext[1]; j++)
     {
       printf("%f ", *(float *)pp);
       pp += str[1];
     }
     prow += str[0];
     printf("\n");
   }

   // Allocate a new Fortran object on the Fortran side using arr as source
   // Do some array computation on the Fortran side
   alloc_new_obj(desc_arr);

   // update the C descriptor of arr to describe a rank-one array section
   CFI_index_t lower_bounds[] = {1, desc_arr->dim[1].lower_bound};
   CFI_index_t upper_bounds[] = {1, desc_arr->dim[1].lower_bound + desc_arr->dim[1].extent-1}; 
   CFI_index_t strides[] = {0,1};
   CFI_CDESC_T(1) section;
   CFI_rank_t r = 1 ;

   rc = CFI_establish ((CFI_cdesc_t *) &section, 
                        NULL, 
                        CFI_attribute_pointer, 
                        CFI_type_float, 
                        0, 
                        r, 
                        NULL);

   assert(rc == CFI_SUCCESS); 

   // the lower bound is 0 by default 
   rc = CFI_section ( (CFI_cdesc_t *) &section, 
                        desc_arr,
                        lower_bounds, 
                        upper_bounds, 
                        strides );

   assert(rc == CFI_SUCCESS); 

   // Change the lower bound to 1 to match Fortran defaults
   CFI_cdesc_t * ptr = (CFI_cdesc_t *) &section;
   CFI_index_t lb_ptr[1];
   lb_ptr[0] = 1;
   rc = CFI_setpointer ( ptr, ptr, lower_bounds );
   assert(rc == CFI_SUCCESS); 
   
   // Create a new Fortran pointer on the Fortran side using arr as source
   // Do some array computation on the Fortran side
   associate_new_obj((CFI_cdesc_t *) &section);

   // deallocate arr on the C side 
   rc = CFI_deallocate(desc_arr);
   assert(rc == CFI_SUCCESS); 

   // Allocate arr as a 10x10 matrix
   lb[0] = -4;   
   lb[1] = -4;   
   ub[0] = 5;  
   ub[1] = 5;  
   rc = CFI_allocate( (CFI_cdesc_t *) &arr, lb, ub, dummy);
   assert(rc == CFI_SUCCESS); 
   assert(desc_arr->base_addr);
   assert(CFI_is_contiguous(desc_arr));

   // verify the extent, stride and the lower bounds
   ext[0] = desc_arr->dim[0].extent;
   str[0] = desc_arr->dim[0].sm;
   lb[0] = desc_arr->dim[0].lower_bound;
   ext[1] = desc_arr->dim[1].extent;
   str[1] = desc_arr->dim[1].sm;
   lb[1] = desc_arr->dim[1].lower_bound;

#if _DEBUG
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0, ext[0], lb[0], str[0]);
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1, ext[1], lb[1], str[1]);
#endif

   assert(ext[0] == 10);
   assert(ext[1] == 10);
   assert(str[0] == 4);
   assert(str[1] == ext[0]*str[0]);
   assert(lb[0] == -4);
   assert(lb[1] == -4);

   // verify array bound/size and read values from file
   check_all(desc_arr);

   return 0;
}
