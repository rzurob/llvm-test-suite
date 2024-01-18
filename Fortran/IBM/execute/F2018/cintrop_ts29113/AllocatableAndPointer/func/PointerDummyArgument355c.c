#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void fcheck(int* flag, CFI_cdesc_t* a); 
/* flag set to 0 when pointer is dissociated, any non zero value when pointer is associated
flag set to 1 when lbound is 0 
flag set to 2 when lbound is 1 
flag set to -1 when array is not contiguous 
*/
void fdisassociate(CFI_cdesc_t* a); // disassociate the pointer on the Fortran side

int main()
{
  CFI_CDESC_T(1) arr;
  CFI_CDESC_T(1) base_arr;
  CFI_CDESC_T(1) point_arr;

  CFI_cdesc_t * desc_arr = (CFI_cdesc_t *) &arr;
  CFI_cdesc_t * base_desc_arr = (CFI_cdesc_t *) &base_arr;
  CFI_cdesc_t * point_desc_arr = (CFI_cdesc_t *) &point_arr;

  CFI_index_t extents[1] = {10};
  CFI_index_t strides[1] = {2};
  CFI_index_t lbounds[1] = {1};

  int rc, flag;
  int carray[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10} ;
  
  // establishing the pointer to a contig target
  rc = CFI_establish(desc_arr,
                     carray,
                     CFI_attribute_pointer,
                     CFI_type_int,
                     sizeof(int),
                     1,
                     extents);
  assert(CFI_SUCCESS == rc);

  // test when the pointer is associated  
  flag = 1;
#if _DEBUG
  printf("Call Fortran with flag set to : %d and the pointer is associated\n", flag);
  printf("Pointer array has: %d elements\n", extents[0]);
  printf("lower bound is: %d\n", 0);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // disassociating the pointer
  rc = CFI_setpointer(desc_arr, NULL, NULL);
  assert(CFI_SUCCESS == rc);

  // test when the pointer is dissociated 
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d and the pointer is not associated\n", flag);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // establishing a base pointer to a contig target
  rc = CFI_establish(base_desc_arr,
                     carray,
                     CFI_attribute_pointer,
                     CFI_type_int,
                     sizeof(int),
                     1,
                     extents);
  assert(CFI_SUCCESS == rc);
  
  // now associate arr with base_arr
  rc = CFI_setpointer(desc_arr, base_desc_arr, lbounds);
  assert(CFI_SUCCESS == rc);

  // test when the pointer is associated  
  flag = 2;
#if _DEBUG
  printf("Call Fortran with flag set to : %d and the pointer is associated\n", flag);
  printf("Pointer array has: %d elements\n", extents[0]);
  printf("lower bound is: %d\n", lbounds[0]);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // disassociate arr again
  rc = CFI_setpointer(desc_arr, NULL, NULL);
  assert(CFI_SUCCESS == rc);

  // test when the pointer is not associated  
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d and the pointer is not associated\n", flag);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);
  
  // now set arr to a section of base_arr
  // Note we can use extents as ubounds, because lbounds are 1 <--- change THIS !!!!
  rc = CFI_section(desc_arr, base_desc_arr, lbounds, extents, strides);
  assert(CFI_SUCCESS == rc);

  // test when the pointer is associated  
  flag = -1;
#if _DEBUG
  printf("Call Fortran with flag set to : %d and the pointer is associated\n", flag);
  printf("Pointer array has: %d elements\n", extents[0]);
  printf("lower bound is: %d\n", lbounds[0]);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // disassociate arr on the Fortran side this time 
  fdisassociate(desc_arr); 

  // test when the pointer is not associated  
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d and the pointer is not associated\n", flag);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

/* add more array computation/processing in the Fortran side */

  return 0;
}
