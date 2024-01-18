#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

// Prototype for the functions that are defined on the Fortran side.
void sub_assoc(int *n, int *m, CFI_cdesc_t *arg);  // n, m extent of dim1 and dim2 of array arg. 
void assign_value(CFI_cdesc_t *arg1);           // fill out the arrays
void my_matmul(CFI_cdesc_t *a, CFI_cdesc_t *b, CFI_cdesc_t *res); //Matrix multiplication

int main(int argc, char ** argv)
{   

   CFI_CDESC_T(2) arr1, arr2, arr_res;         
   CFI_cdesc_t * desc_arr1 = (CFI_cdesc_t *) &arr1;
   CFI_cdesc_t * desc_arr2 = (CFI_cdesc_t *) &arr2;
   CFI_cdesc_t * desc_arr_res = (CFI_cdesc_t *) &arr_res;
   CFI_index_t lb[2], ext[2], str[2];
   CFI_rank_t rank = 2;

   int rc, nrow, ncol;
   char * prow, * pp;

   rc = CFI_establish(desc_arr1,
                      NULL,
                      CFI_attribute_pointer,
                      CFI_type_float,
                      sizeof(float),
                      rank,
                      NULL);
   assert(rc == CFI_SUCCESS); 
   assert(arr1.attribute == CFI_attribute_pointer);
   assert(arr1.type == CFI_type_float);
   assert(arr1.rank == rank);
   #if _DEBUG
       printf("established desc_arr1\n");
   #endif


   rc = CFI_establish(desc_arr2,
                      NULL,
                      CFI_attribute_pointer,
                      CFI_type_float,
                      sizeof(float),
                      rank,
                      NULL);
   assert(rc == CFI_SUCCESS); 
   assert(arr2.attribute == CFI_attribute_pointer);
   assert(arr2.type == CFI_type_float);
   assert(arr2.rank == rank);
   #if _DEBUG
       printf("established desc_arr2\n");
   #endif

   rc = CFI_establish(desc_arr_res,
                      NULL,
                      CFI_attribute_pointer,
                      CFI_type_float,
                      sizeof(float),
                      rank,
                      NULL);
   assert(rc == CFI_SUCCESS); 
   assert(arr_res.attribute == CFI_attribute_pointer);
   assert(arr_res.type == CFI_type_float);
   assert(arr_res.rank == rank);
   #if _DEBUG
       printf("established desc_arr_res\n");
   #endif

   // calling the Fortran subroutine with non-associated array and allocate it on the Fortran side
   // verify on the C side if the arrays are associated 
   nrow=3, ncol=3;
   sub_assoc(&nrow, &ncol, desc_arr1);
   assert(desc_arr1->base_addr);
   #if _DEBUG
       printf("after sub_assoc: call 1\n");
   #endif

   nrow=2;
   sub_assoc(&nrow, &ncol, desc_arr2);
   assert(desc_arr2->base_addr);
   #if _DEBUG
       printf("after sub_assoc: call 2\n");
   #endif

   // fill out the arrays
   assign_value(desc_arr1);
   #if _DEBUG
       printf("after assign_value: call 1\n");
   #endif
   assign_value(desc_arr2);
   #if _DEBUG
       printf("after assign_value: call 2\n");
   #endif

   // Matrix multiplication on the Fortran side. Verification on the C-side 
   #if _DEBUG
       printf("before my_matmul\n");
   #endif
   my_matmul(desc_arr1, desc_arr1, desc_arr_res);
   #if _DEBUG
       printf("after my_matmul\n");
   #endif

   // verify the extent, stride and the lower bounds of the result of matmul 
   ext[0] = desc_arr_res->dim[0].extent;
   str[0] = desc_arr_res->dim[0].sm;
   lb[0] = desc_arr_res->dim[0].lower_bound;
   ext[1] = desc_arr_res->dim[1].extent;
   str[1] = desc_arr_res->dim[1].sm;
   lb[1] = desc_arr_res->dim[1].lower_bound;

   assert(lb[0] == 1);
   assert(lb[1] == 1);
   assert(ext[0] == 3);
   assert(ext[1] == 3);
   assert(str[0] == 4);
   assert(str[1] == ext[0]*str[0]);
   #if _DEBUG
       printf("verify the results after my_matmul\n");
   #endif


   // print the values of the array one row at a time
   prow = (char *)desc_arr_res->base_addr;
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
   /*
   !*********************************************
   ! expected result: for matmul(x,x)
   !  -5 15  8
   !   5 13 12
   !   1  1 -8
   !*********************************************
   */

   rc = CFI_setpointer(desc_arr_res, NULL, NULL);
   assert(rc == CFI_SUCCESS); 
   my_matmul(desc_arr2, desc_arr1, desc_arr_res);

   // verify the extent, stride and the lower bounds of the result of matmul 
   ext[0] = desc_arr_res->dim[0].extent;
   str[0] = desc_arr_res->dim[0].sm;
   lb[0] = desc_arr_res->dim[0].lower_bound;
   ext[1] = desc_arr_res->dim[1].extent;
   str[1] = desc_arr_res->dim[1].sm;
   lb[1] = desc_arr_res->dim[1].lower_bound;

   assert(lb[0] == 1);
   assert(lb[1] == 1);
   assert(ext[0] == 2);
   assert(ext[1] == 3);
   assert(str[0] == 4);
   assert(str[1] == ext[0]*str[0]);


   // print the values of the array one row at a time
   prow = (char *)desc_arr_res->base_addr;
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
   /*
   !*********************************************
   ! expected result: for matmul(y,x)
   !  5 13 12
   !  6  7 -1
   !*********************************************
   */

   // deallocate all arrays on the C side 
   rc = CFI_setpointer(desc_arr1, NULL, NULL);
   assert(rc == CFI_SUCCESS); 
   rc = CFI_setpointer(desc_arr2, NULL, NULL);
   assert(rc == CFI_SUCCESS); 
   rc = CFI_setpointer(desc_arr_res, NULL, NULL);
   assert(rc == CFI_SUCCESS); 

   return 0;
}
