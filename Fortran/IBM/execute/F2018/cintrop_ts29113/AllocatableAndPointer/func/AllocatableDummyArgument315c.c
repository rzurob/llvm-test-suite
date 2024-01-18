#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <complex.h>
#include "ISO_Fortran_binding.h"

#define dummy 0

// Prototype for the functions that are defined on the Fortran side.
void check_all(CFI_cdesc_t *arg);                    // verify allocation status, size, rank and bounds
void check_ptr(CFI_cdesc_t *arg);                    // verify allocation status, size, rank and bounds
void check_fptr(CFI_cdesc_t *arg);                    // verify allocation status, size, rank and bounds
void update_all(CFI_cdesc_t *arg);                   // update the values of the allocatable array dummy arg. 
void alloc_new_obj(CFI_cdesc_t *arg);                // allocate a new Fortran object on the Fortran side using arg as source
void associate_new_obj(CFI_cdesc_t *arg);            // create a new Fortran pointer on the Fortran side using arg as source

int main(int argc, char ** argv)
{   

   CFI_CDESC_T(1) all, p_arr;
   CFI_cdesc_t * desc_all = (CFI_cdesc_t *) &all;
   CFI_cdesc_t * desc_p_arr = (CFI_cdesc_t *) &p_arr;

   // rank, bounds, stride and extent of the array
   CFI_rank_t rank = 1;
   //CFI_index_t lb1[1], ub1[1], ext1[1], str1[1];
   CFI_index_t lb[1] = {1}; 
   CFI_index_t ub[1] = {5};  
   CFI_index_t ext[1], str[1];
   CFI_index_t sub[1];

   ext[0]=5;

   // defined on the C side are transposed.
   float _Complex z[5];

   z[0] = -0.0f + -0.0f*_Complex_I;
   z[1] = -1.0f + -1.0f*_Complex_I;
   z[2] = -2.0f + -2.0f*_Complex_I;
   z[3] = -3.0f + -3.0f*_Complex_I;
   z[4] = -4.0f + -4.0f*_Complex_I;

   int rc;

   rc = CFI_establish(desc_all,
                     NULL,
                     CFI_attribute_allocatable,
                     CFI_type_float_Complex,
                     2*sizeof(float),
                     rank, 
                     NULL); 

   assert(rc == CFI_SUCCESS); 
   assert(all.attribute == CFI_attribute_allocatable);
   assert(all.type == CFI_type_float_Complex);
   assert(all.rank == rank);

   rc = CFI_establish(desc_p_arr,
                     z,
                     CFI_attribute_pointer,
                     CFI_type_float_Complex,
                     2*sizeof(float),
                     rank,
                     ext);

   assert(rc == CFI_SUCCESS); 
   assert(p_arr.attribute == CFI_attribute_pointer);
   assert(p_arr.type == CFI_type_float_Complex);
   assert(p_arr.rank == rank);

   check_ptr(desc_p_arr);

   // Change the lower bound to 1 in order to match Fortran defaults
   rc = CFI_setpointer(desc_p_arr, desc_p_arr, lb);
   assert(rc == CFI_SUCCESS); 
   check_fptr(desc_p_arr);

   // Allocate all on the C side
   rc = CFI_allocate(desc_all, lb, ub, dummy);  
   assert(rc == CFI_SUCCESS); 
   assert(desc_all->base_addr);
   check_all(desc_all); 

   // verify the extent, stride and the lower bounds
   assert(desc_all->dim[0].sm == 8);
   assert(desc_all->dim[0].extent == 5);
   assert(desc_all->dim[0].lower_bound == 1);

   // Fill the array in Fortran
   update_all(desc_all);
   #if _DEBUG
       printf("extent of all: %d\n", desc_all->dim[0].extent);
       printf("lower bound of all: %d:\n", desc_all->dim[0].lower_bound);
   #endif
   assert(desc_all->dim[0].sm == 8);
   assert(desc_all->dim[0].extent == 10);
   assert(desc_all->dim[0].lower_bound == 1);

   // print the values of the array 
   float _Complex *address;
   for (int i=desc_all->dim[0].lower_bound; i<(desc_all->dim[0].lower_bound+desc_all->dim[0].extent); i++) {
       sub[0] = i;
       address = (float _Complex*) CFI_address(desc_all, sub);
       printf("the values stored in c of element %d is: (%f, %f)\n", i, crealf(*address), cimagf(*address));
   }

   // print only the real part      
   CFI_CDESC_T(1) rel;
   CFI_cdesc_t *crel = (CFI_cdesc_t *) &rel;
   rc = CFI_establish( crel, 
                       NULL, 
                       CFI_attribute_pointer, 
                       CFI_type_float, 
                       0, 
                       1, 
                       NULL);
   assert(rc == CFI_SUCCESS); 
   assert(rel.attribute == CFI_attribute_pointer);
   assert(rel.type == CFI_type_float);
   assert(rel.rank == 1);
               
   rc = CFI_select_part(crel, desc_all, 0, 0);
   assert(rc == CFI_SUCCESS); 
   // verify the extent, stride and the lower bounds
   assert(crel->dim[0].sm == 8);
   assert(crel->dim[0].extent == 10);
   assert(crel->dim[0].lower_bound == 0);
   #if _DEBUG
       printf("extent: %d\n", crel->dim[0].extent);
       printf("lower bound: %d:\n", crel->dim[0].lower_bound);
   #endif

   float *crel_add;
   for (int i=crel->dim[0].lower_bound; i<(crel->dim[0].lower_bound+crel->dim[0].extent); i++) {
       sub[0] = i;
       crel_add = (float *) CFI_address(crel, sub);
       printf("Real part of element %d is: %f\n", i, *crel_add);
   }

   // print only the imaginary part      
   CFI_CDESC_T(1) imag;
   CFI_cdesc_t *cimag = (CFI_cdesc_t *) &imag;
   rc = CFI_establish( cimag, 
                       NULL, 
                       CFI_attribute_pointer, 
                       CFI_type_float, 
                       0, 
                       1, 
                       NULL);
   assert(rc == CFI_SUCCESS); 
   assert(imag.attribute == CFI_attribute_pointer);
   assert(imag.type == CFI_type_float);
   assert(imag.rank == 1);
               
   rc = CFI_select_part(cimag, desc_all, sizeof(float), 0);
   assert(rc == CFI_SUCCESS); 
   // verify the extent, stride and the lower bounds
   assert(cimag->dim[0].sm == 8);
   assert(cimag->dim[0].extent == 10);
   assert(cimag->dim[0].lower_bound == 0);
   #if _DEBUG
       printf("extent of all: %d\n", cimag->dim[0].extent);
       printf("lower bound of all: %d:\n", cimag->dim[0].lower_bound);
   #endif

   float *cimag_add;
   for (int i=cimag->dim[0].lower_bound; i<(cimag->dim[0].lower_bound+cimag->dim[0].extent); i++) {
       sub[0] = i;
       cimag_add = (float *) CFI_address(cimag, sub);
       printf("Imaginay part of element %d is: %f\n", i, *cimag_add);
   }

   // Allocate a new Fortran object on the Fortran side using all as source
   // Do some array computation on the Fortran side
   alloc_new_obj(desc_all);

   // update the C descriptor of all to describe a rank-one array section
   #if _DEBUG
       printf("extent of all: %d\n", desc_all->dim[0].extent);
       printf("lower bound of all: %d:\n", desc_all->dim[0].lower_bound);
   #endif

   CFI_index_t lower_bounds[] = {desc_all->dim[0].lower_bound + 1};
   CFI_index_t upper_bounds[] = {desc_all->dim[0].lower_bound + desc_all->dim[0].extent - 2}; 
   CFI_index_t strides[] = {2};
   CFI_CDESC_T(1) section;
   CFI_rank_t r = 1 ;

   rc = CFI_establish ((CFI_cdesc_t *) &section, 
                        NULL, 
                        CFI_attribute_pointer, 
                        CFI_type_float_Complex,
                        2*sizeof(float), 
                        r, 
                        NULL);

   assert(rc == CFI_SUCCESS); 

   rc = CFI_section ( (CFI_cdesc_t *) &section, 
                        desc_all,
                        lower_bounds, 
                        upper_bounds, 
                        strides );
   assert(rc == CFI_SUCCESS); 

   
   // Create a new Fortran pointer on the Fortran side using section as source
   associate_new_obj((CFI_cdesc_t *) &section);

   // deallocate arr on the C side 
   rc = CFI_deallocate(desc_all);
   assert(rc == CFI_SUCCESS); 

   return 0;
}
