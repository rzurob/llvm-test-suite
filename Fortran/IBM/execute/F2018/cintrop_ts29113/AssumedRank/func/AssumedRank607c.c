#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void fcheck(int* flag, CFI_cdesc_t* a); 
/* flag set to 0 when pointer is dissociated, any non zero value when pointer is associated
flag set to 1 when lbound is 0 
flag set to 2 when lbound is 1 
flag set to -1 when array is not contiguous 
*/

int main()
{
  CFI_CDESC_T(1) arr, base_arr, point_arr;

  CFI_cdesc_t * desc_arr = (CFI_cdesc_t *) &arr;
  CFI_cdesc_t * base_desc_arr = (CFI_cdesc_t *) &base_arr;
  CFI_cdesc_t * point_desc_arr = (CFI_cdesc_t *) &point_arr;

  CFI_index_t extents[1] = {10};
  CFI_index_t strides[1] = {2};
  CFI_index_t lbounds[1] = {0};
  CFI_index_t ubounds[1] = {10};

  int rc, flag;
  int c_arr[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10} ;
  
  // establishing a non-allocatable non-pointer array of rank 1
  rc = CFI_establish(desc_arr,
                     c_arr,
                     CFI_attribute_other,
                     CFI_type_int,
                     sizeof(int),
                     1,
                     extents);

  assert(rc == CFI_SUCCESS);
  assert(arr.attribute == CFI_attribute_other);
  assert(arr.type == CFI_type_int);
  assert(arr.rank == 1);
  assert(desc_arr->dim[0].lower_bound == 0);

  // call fcheck with non-pointer non-allocatable object 
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Non-pointer Non-allocatable object of rank %d with %d elements\n", desc_arr->rank, desc_arr->dim[0].extent);
  printf("lower bound on the C-side is: %d\n", desc_arr->dim[0].lower_bound);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // establishing the pointer to a contig target
  rc = CFI_establish(point_desc_arr,
                     c_arr,
                     CFI_attribute_pointer,
                     CFI_type_int,
                     sizeof(int),
                     1,
                     extents);

  assert(rc == CFI_SUCCESS);
  assert(point_arr.attribute == CFI_attribute_pointer);
  assert(point_arr.type == CFI_type_int);
  assert(point_arr.rank == 1);

  // test when a pointer that is associated  
  flag = 1;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Pointer array of rank %d and has %d elements\n", point_desc_arr->rank, point_desc_arr->dim[0].extent);
  printf("lower bound on the C-side is: %d\n", point_desc_arr->dim[0].lower_bound);
  fflush(stdout);
#endif
  fcheck(&flag, point_desc_arr);

  // disassociating the pointer
  rc = CFI_setpointer(point_desc_arr, NULL, NULL);
  assert(CFI_SUCCESS == rc);

  // establishing a base pointer to a contig target
  rc = CFI_establish(base_desc_arr,
                     c_arr,
                     CFI_attribute_pointer,
                     CFI_type_int,
                     sizeof(int),
                     1,
                     extents);
  
  assert(rc == CFI_SUCCESS);
  assert(point_arr.attribute == CFI_attribute_pointer);
  assert(point_arr.type == CFI_type_int);
  assert(point_arr.rank == 1);
  
  // Associate point_arr with base_arr
  // Lower bound is now "lbounds"
  rc = CFI_setpointer(point_desc_arr, base_desc_arr, lbounds);
  assert(CFI_SUCCESS == rc);

  // test when the pointer is associated and bounds are changed 
  flag = 2;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Pointer array of rank %d and has %d elements\n", point_desc_arr->rank, point_desc_arr->dim[0].extent);
  printf("lower bound on the C-side is: %d\n", point_desc_arr->dim[0].lower_bound);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // disassociate point_arr again
  rc = CFI_setpointer(point_desc_arr, NULL, NULL);
  assert(CFI_SUCCESS == rc);

  // now set arr to a section of base_arr base_arr[2:8:2]
  lbounds[0] = 2;
  ubounds[0] = 8;
  rc = CFI_section(desc_arr, base_desc_arr, lbounds, ubounds, strides);
  assert(CFI_SUCCESS == rc);
  assert(desc_arr->dim[0].lower_bound == 0);

  // test when a non-contiguous array section 
  flag = -1;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Pointer array of rank %d and has %d elements\n", desc_arr->rank, desc_arr->dim[0].extent);
  printf("lower bound on the C-side is: %d\n", desc_arr->dim[0].lower_bound);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // disassociate base_desc_arr 
  rc = CFI_setpointer(base_desc_arr, NULL, NULL);
  assert(CFI_SUCCESS == rc);

  return 0;
}
