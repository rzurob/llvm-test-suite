#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

// Prototype for the functions that are defined on the Fortran side.
void sub_alloc(int *n, int *m, CFI_cdesc_t *arg);  // n, m extent of dim1 and dim2 of array arg. 
void sub_alloc_clean(int *n, int *m, CFI_cdesc_t *arg); //sub_alloc and sub_alloc_clean allocate the arrays
void assign_value(CFI_cdesc_t *arg1);           // fill out the arrays
void my_matmul(CFI_cdesc_t *a, CFI_cdesc_t *b); //Matrix multiplication

void check_value(float arr1 , float arr2)
{
  assert( fabs(arr1-arr2) <= FLT_EPSILON );
}

int main(int argc, char ** argv)
{   

   CFI_CDESC_T(2) arr1, arr2;         
   CFI_cdesc_t * desc_arr1 = (CFI_cdesc_t *) &arr1;
   CFI_cdesc_t * desc_arr2 = (CFI_cdesc_t *) &arr2;
   CFI_index_t lb1[2], ext1[2], str1[2];
   CFI_index_t lb2[2], ext2[2], str2[2];
   CFI_rank_t rank = 2;

   int rc1, rc2, nrow, ncol;
   char * prow, * pp;

   rc1 = CFI_establish(desc_arr1,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_float,
                      sizeof(float),
                      rank,
                      NULL);

   rc2 = CFI_establish(desc_arr2,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_float,
                      sizeof(float),
                      rank,
                      NULL);


   assert(rc1 == CFI_SUCCESS); 
   assert(rc2 == CFI_SUCCESS); 
   assert(arr1.attribute == CFI_attribute_allocatable);
   assert(arr1.type == CFI_type_float);
   assert(arr1.rank == rank);
   assert(arr2.attribute == CFI_attribute_allocatable);
   assert(arr2.type == CFI_type_float);
   assert(arr2.rank == rank);

   // calling the Fortran subroutine with non-allocated array and allocate it on the Fortran side
   // verify on the C side if the arrays are allocated 
   nrow=3, ncol=3;
   sub_alloc(&nrow, &ncol, desc_arr1);
   assert(desc_arr1->base_addr);
   nrow=2;
   sub_alloc_clean(&nrow, &ncol, desc_arr2);
   assert(desc_arr2->base_addr);

   // verify the extent, stride and the lower bounds
   ext1[0] = desc_arr1->dim[0].extent;
   str1[0] = desc_arr1->dim[0].sm;
   lb1[0] = desc_arr1->dim[0].lower_bound;
   ext1[1] = desc_arr1->dim[1].extent;
   str1[1] = desc_arr1->dim[1].sm;
   lb1[1] = desc_arr1->dim[1].lower_bound;

   assert(ext1[0] == 3);
   assert(ext1[1] == 3);
   assert(str1[0] == 4);
   assert(str1[1] == ext1[0]*str1[0]);
   assert(lb1[0] == 1);
   assert(lb1[1] == 1);

   ext2[0] = desc_arr2->dim[0].extent;
   str2[0] = desc_arr2->dim[0].sm;
   lb2[0] = desc_arr2->dim[0].lower_bound;
   ext2[1] = desc_arr2->dim[1].extent;
   str2[1] = desc_arr2->dim[1].sm;
   lb2[1] = desc_arr2->dim[1].lower_bound;

   assert(ext2[0] == 2);
   assert(ext2[1] == 3);
   assert(str2[0] == 4);
   assert(str2[1] == ext2[0]*str2[0]);
   assert(lb2[0] == 1);
   assert(lb2[1] == 1);

   // fill out the arrays
   assign_value(desc_arr1);
   fflush(stdout);

   // print the values of the array one row at a time
   prow = (char *)desc_arr1->base_addr;
   for(int i = 0; i < ext1[0]; i++)
   {
     pp = prow;
     for(int j = 0; j < ext1[1]; j++)
     {
       printf("%f ", *(float *)pp);
       pp += str1[1];
     }
     prow += str1[0];
     printf("\n");
   }

   assign_value(desc_arr2);
   fflush(stdout);

   // print the values of the array one row at a time
   prow = (char *)desc_arr2->base_addr;
   for(int i = 0; i < ext2[0]; i++)
   {
     pp = prow;
     for(int j = 0; j < ext2[1]; j++)
     {
       printf("%f ", *(float *)pp);
       pp += str2[1];
     }
     prow += str2[0];
     printf("\n");
   }

   // Matrix multiplication on the Fortran side. Verification on the Fortran side 
   my_matmul(desc_arr1, desc_arr1);
   my_matmul(desc_arr2, desc_arr1);

   // deallocate both arr1 and arr2 on the C side 
   rc1 = CFI_deallocate(desc_arr1);
   rc2 = CFI_deallocate(desc_arr2);
   assert(rc1 == CFI_SUCCESS); 
   assert(rc2 == CFI_SUCCESS); 

   return 0;
}
