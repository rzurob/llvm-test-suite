#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define dummy 0

typedef struct shape SHAPE;

struct shape { // define a parallelogram
  char name;
  int area;
  int sides[2];
};

// Prototype for the functions that are defined on the Fortran side.
void check_all(CFI_cdesc_t *arg);                    // verify allocation status, size, rank and bounds
void check_ptr(CFI_cdesc_t *arg);                    // verify allocation status, size, rank and bounds
void alloc_obj(CFI_cdesc_t *arg);                    // allocate a new Fortran object on the Fortran side using arg as source
void fill_all(CFI_cdesc_t *arg);                     // Fill the array 
void alloc_new_obj(CFI_cdesc_t *arg);                // allocate a new Fortran object on the Fortran side using arg as source
void associate_new_obj(CFI_cdesc_t *arg);            // create a new Fortran pointer on the Fortran side using arg as source

int main(int argc, char ** argv)
{   

   CFI_CDESC_T(2) s_arr;
   CFI_CDESC_T(1) p_arr;
   CFI_cdesc_t * s_desc_arr = (CFI_cdesc_t *) &s_arr;
   CFI_cdesc_t * p_desc_arr = (CFI_cdesc_t *) &p_arr;

   // rank, bounds, stride and extent of the array
   CFI_rank_t rank = 2;
   CFI_index_t lb[2] = {1, 1}; 
   CFI_index_t ub[2] = {2, 2};  
   CFI_index_t ext[2] = {2, 2};  
   CFI_index_t str[2];

   // defined on the C side are transposed.
   SHAPE arr_shapes[2];
  
   // init. 
   arr_shapes[0].area = -99;
   arr_shapes[1].area = -99;

   arr_shapes[0].sides[0] = -1;
   arr_shapes[0].sides[1] = -1;
   arr_shapes[1].sides[0] = -1;
   arr_shapes[1].sides[1] = -1;

   int rc;
   _Bool test;

  rc = CFI_establish(s_desc_arr,
                     NULL,
                     CFI_attribute_allocatable,
                     CFI_type_struct,
                     sizeof(SHAPE),
                     rank,
                     NULL);

   assert(rc == CFI_SUCCESS); 
   assert(s_arr.attribute == CFI_attribute_allocatable);
   assert(s_arr.type == CFI_type_struct);
   assert(s_arr.rank == rank);

   CFI_rank_t p_rank = 1;
   CFI_index_t p_ext[1] = {2};  
   rc = CFI_establish(p_desc_arr,
                      arr_shapes,
                      CFI_attribute_pointer,
                      CFI_type_struct,
                      sizeof(SHAPE),
                      p_rank,
                      ext);

   assert(rc == CFI_SUCCESS); 
   assert(p_arr.attribute == CFI_attribute_pointer);
   assert(p_arr.type == CFI_type_struct);
   assert(p_arr.rank == p_rank);

   // Allocate s_arr on the C side
   rc = CFI_allocate(s_desc_arr, lb, ub, dummy);  
   assert(rc == CFI_SUCCESS); 
   assert(s_desc_arr->base_addr);

   rc = CFI_is_contiguous(s_desc_arr);
   if (rc != 1)
   {
     fprintf(stderr, "The array should be contiguous %d\n", rc);
     exit(1);
   }

   // Verification on the Fortran side 
   check_all(s_desc_arr); 

   // verify the extent, stride and the lower bounds
   CFI_index_t lb1[2], ub1[2], ext1[2], str1[2];

   ext1[0] = s_desc_arr->dim[0].extent;
   str1[0] = s_desc_arr->dim[0].sm;
   lb1[0] = s_desc_arr->dim[0].lower_bound;
   ext1[1] = s_desc_arr->dim[1].extent;
   str1[1] = s_desc_arr->dim[1].sm;
   lb1[1] = s_desc_arr->dim[1].lower_bound;

   assert(ext1[0] == 2);
   assert(ext1[1] == 2);
   assert(lb1[0] == 1);
   assert(lb1[1] == 1);
   assert(str1[0] == 16);  // padding ?
   assert(str1[1] == ext1[0]*str1[0]);

   // Fill the values
   fill_all(s_desc_arr); 

   // print the values of the array one row at a time
   printf("printing arr in C\n");

   SHAPE *address;
   CFI_index_t subscripts[2];
   for (int j=lb1[1]; j<(lb1[1]+ext1[1]); j++) {
      subscripts[1] = j;
      for (int i=lb1[0]; i<(lb1[0]+ext1[0]); i++) {
          subscripts[0] = i;
          address = (SHAPE*) CFI_address(s_desc_arr, subscripts);
          printf("%c %d %d %d\n", address->name, address->area, address->sides[0], address->sides[1]);
      }
   }


   // Allocate a new Fortran object on the Fortran side using s_arr as source
   alloc_new_obj(s_desc_arr);

   // update the C descriptor of arr to describe a rank-one array section
   CFI_index_t lower_bounds[] = {s_desc_arr->dim[0].lower_bound, s_desc_arr->dim[1].lower_bound};
   CFI_index_t upper_bounds[] = {s_desc_arr->dim[0].lower_bound, s_desc_arr->dim[1].lower_bound + s_desc_arr->dim[1].extent-1}; 
   CFI_index_t strides[] = {0,1};
   CFI_CDESC_T(1) section;
   CFI_rank_t r = 1 ;

   rc = CFI_establish((CFI_cdesc_t *) &section, 
                       NULL, 
                       CFI_attribute_pointer, 
                       CFI_type_struct, 
                       sizeof(SHAPE),
                       r, 
                       NULL);
   assert(rc == CFI_SUCCESS); 

   // the lower bound is 0 by default 
   rc = CFI_section((CFI_cdesc_t *) &section, 
                     s_desc_arr,
                     lower_bounds, 
                     upper_bounds, 
                     strides );
   assert(rc == CFI_SUCCESS); 

   // Change the lower bound to 1 to match Fortran defaults
   CFI_cdesc_t * ptr = (CFI_cdesc_t *) &section;
   CFI_index_t lb_ptr[1];
   lb_ptr[0] = 1;
   rc = CFI_setpointer(ptr, ptr, lower_bounds );
   assert(rc == CFI_SUCCESS); 
   
   // Create a new Fortran pointer on the Fortran side using section as source
   // Do some array computation on the Fortran side
   associate_new_obj((CFI_cdesc_t *) &section);

   // Deallocate s_arr on the C side 
   rc = CFI_deallocate(s_desc_arr);
   assert(rc == CFI_SUCCESS); 

   // Allocate s_arr on the Fortran side 
   alloc_obj(s_desc_arr);
   // deallocate s_arr on the C side 
   rc = CFI_deallocate(s_desc_arr);
   assert(rc == CFI_SUCCESS); 

   // Verification on the Fortran side 
   check_ptr(p_desc_arr);
   return 0;
}
