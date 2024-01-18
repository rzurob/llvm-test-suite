#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define dummy 0

// Prototype for the functions that are defined on the Fortran side.
void sub_alloc_1(CFI_cdesc_t *arg);                    // Allocate the object in Fortran
void sub_alloc_2(CFI_cdesc_t *tgt, CFI_cdesc_t *src);  // source and target for the allocation of the second object in Fortran
void sub_dealloc(CFI_cdesc_t *arg);                    // object to be deallocated in Fortran
void my_transpose(CFI_cdesc_t *arg);                   // transpose a 2D pointer array
_Bool ffunc(CFI_cdesc_t *a, CFI_cdesc_t *b);           // Matrix multiplication

int main(int argc, char ** argv)
{   

   CFI_CDESC_T(2) arr1, arr2;         
   CFI_cdesc_t * desc_arr1 = (CFI_cdesc_t *) &arr1;
   CFI_cdesc_t * desc_arr2 = (CFI_cdesc_t *) &arr2;

   CFI_rank_t rank = 2;

   // bounds, strides and extents of the arrays 
   CFI_index_t lb1[2], ub1[2], ext1[2], str1[2];
   CFI_index_t lb2[2] = {1, 1};  // array of 2 rows and 3 columns
   CFI_index_t ub2[2] = {2, 3};  
   CFI_index_t ext2[2], str2[2];

   // Since Fortran is column-major and C is row-major, the input matrixes
   // defined on the C side are transposed.
   float matrix[3][2] = { {3., -2.} ,{2., 4.}, {1., 1.} };

   int rc;
   char * prow, * pp;
   _Bool test;

   rc = CFI_establish(desc_arr1,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_float,
                      dummy,
                      rank,
                      NULL);

   assert(rc == CFI_SUCCESS); 
   assert(arr1.attribute == CFI_attribute_allocatable);
   assert(arr1.type == CFI_type_float);
   assert(arr1.rank == rank);

   rc = CFI_establish(desc_arr2,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_float,
                      dummy,
                      rank,
                      NULL);

   assert(rc == CFI_SUCCESS); 
   assert(arr2.attribute == CFI_attribute_allocatable);
   assert(arr2.type == CFI_type_float);
   assert(arr2.rank == rank);

   // Allocate arr2 on the Fortran side
   sub_alloc_1(desc_arr2);
   assert(desc_arr2->base_addr);
   float *actual_arr2 = arr2.base_addr;

   // fill arr2 on the C side
   *actual_arr2 = matrix[0][0];
   *(actual_arr2+1) = matrix[0][1];
   *(actual_arr2+2) = matrix[1][0];
   *(actual_arr2+3) = matrix[1][1];
   *(actual_arr2+4) = matrix[2][0];
   *(actual_arr2+5) = matrix[2][1];

   // Allocate arr1 on the Fortran side with the source being arr2
   sub_alloc_2(desc_arr1, desc_arr2);
   assert(desc_arr1->base_addr);

   // verify the extent, stride and the lower bounds
   ext1[0] = desc_arr1->dim[0].extent;
   str1[0] = desc_arr1->dim[0].sm;
   lb1[0] = desc_arr1->dim[0].lower_bound;
   ext1[1] = desc_arr1->dim[1].extent;
   str1[1] = desc_arr1->dim[1].sm;
   lb1[1] = desc_arr1->dim[1].lower_bound;

   assert(ext1[0] == 2);
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

   // print the values of the array one row at a time
   printf("printing arr1 in C\n");
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

   // print the values of the array one row at a time
   printf("printing arr2 in C\n");
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

   // update the C descriptor of arr2 to describe a rank-one array section
   CFI_index_t lower_bounds[] = {1, desc_arr2->dim[1].lower_bound};
   CFI_index_t upper_bounds[] = {1, desc_arr2->dim[1].lower_bound + desc_arr2->dim[1].extent-1}; 
   CFI_index_t strides[] = {0,1};
   CFI_CDESC_T(1) section;
   int ind;
   CFI_rank_t r = 1 ;

   ind = CFI_establish ((CFI_cdesc_t *) &section, 
                        NULL, 
                        CFI_attribute_pointer, 
                        CFI_type_float, 
                        0, 
                        r, 
                        NULL);

   assert(ind == CFI_SUCCESS); 

   ind = CFI_section ( (CFI_cdesc_t *) &section, 
                        desc_arr2,
                        lower_bounds, 
                        upper_bounds, 
                        strides );

   assert(ind == CFI_SUCCESS); 

   // Matrix multiplication on the Fortran side. Verification on the Fortran side 
   test = ffunc( (CFI_cdesc_t *) &section, desc_arr1);
   printf("the received value is %d\n", test);
   assert(test); 

   // Create a rank 2 array section with negative strides A(2:1:-1,3:1:-1)
   CFI_index_t neg_lb[] = {desc_arr2->dim[0].extent, desc_arr2->dim[1].extent};
   CFI_index_t neg_up[] = {desc_arr2->dim[0].lower_bound, desc_arr2->dim[1].lower_bound }; 
   CFI_index_t neg_str[] = {-1,-1};
   CFI_CDESC_T(2) neg_sec;
   CFI_cdesc_t * desc_neg_sec = (CFI_cdesc_t *) &neg_sec;
   CFI_rank_t q = 2 ;

   rc = CFI_establish ( desc_neg_sec,
                        NULL, 
                        CFI_attribute_pointer, 
                        CFI_type_float, 
                        0, 
                        q, 
                        NULL);

   assert(rc == CFI_SUCCESS); 

   rc = CFI_section ( desc_neg_sec,  // the new array section has a 
                      desc_arr2,     // lower bound of 0 and upper  
                      neg_lb,        // bound of (neg_up-neg_lb)-1
                      neg_up, 
                      neg_str);

   assert(rc == CFI_SUCCESS); 

   // Verification on the Fortran side and transpose the 2D array
   my_transpose(desc_neg_sec);

   // deallocate both arrays on the Fortran
   sub_dealloc(desc_arr1);
   sub_dealloc(desc_arr2);

   return 0;
}
