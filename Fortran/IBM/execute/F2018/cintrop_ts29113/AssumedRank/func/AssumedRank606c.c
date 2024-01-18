#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define ZERO 0
#define ONE 1
#define TWO 2
#define BIG 15

void fcheck(CFI_cdesc_t* a); 

int main()
{
  CFI_CDESC_T(ZERO) i1;
  CFI_CDESC_T(ONE) arr;
  CFI_CDESC_T(TWO) arr2D;
  CFI_CDESC_T(BIG) arr_big;

  CFI_cdesc_t * desc_i1 = (CFI_cdesc_t *) &i1;
  CFI_cdesc_t * desc_arr = (CFI_cdesc_t *) &arr;
  CFI_cdesc_t * desc_arr2D = (CFI_cdesc_t *) &arr2D;
  CFI_cdesc_t * desc_arr_big = (CFI_cdesc_t *) &arr_big;

  CFI_index_t lb1[ONE] = {-19}, ub1[ONE] = {20};
  CFI_index_t lb2[TWO] = {1, 1}, ub2[TWO] = {2, 3};  // array of 2 rows and 3 columns
  CFI_index_t lb15[BIG] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
  CFI_index_t ub15[BIG] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3};

  int rc;
  
  // establishing an allocatable array of rank 0
  rc = CFI_establish(desc_i1,
                     NULL,
                     CFI_attribute_allocatable,
                     CFI_type_float,
                     sizeof(float),
                     ZERO,
                     NULL);

  assert(CFI_SUCCESS == rc);
  assert(i1.attribute == CFI_attribute_allocatable);
  assert(i1.type == CFI_type_float);
  assert(i1.rank == ZERO);

  // call fcheck with a non-allocated allocatable scalar (rank 0)
  fcheck(desc_i1);
  if (desc_i1->base_addr != NULL)
      exit(1);

  // Allocate rank 0 (scalar) on the C side
  rc = CFI_allocate(desc_i1, ZERO, ZERO, ZERO);
  assert(CFI_SUCCESS == rc);
  assert(desc_i1->base_addr);

  // call fcheck with an allocated allocatable scalar 
  fcheck(desc_i1);
  if (desc_i1->base_addr != NULL)
      exit(2);

  // establishing an allocatable array of rank 1
  rc = CFI_establish(desc_arr,
                     NULL,
                     CFI_attribute_allocatable,
                     CFI_type_float,
                     sizeof(float),
                     ONE,
                     NULL);

  assert(rc == CFI_SUCCESS);
  assert(arr.attribute == CFI_attribute_allocatable);
  assert(arr.type == CFI_type_float);
  assert(arr.rank == ONE);

  // call fcheck with a non-allocated allocatable object (rank 1)
  fcheck(desc_arr);
  if (desc_arr->base_addr != NULL)
      exit(3);

  // Allocate rank 1 object on the C side
  rc = CFI_allocate(desc_arr, lb1, ub1, ZERO);
  assert(CFI_SUCCESS == rc);
  assert(desc_arr->base_addr);

  // call fcheck with an allocated allocatable object 
  fcheck(desc_arr);
  if (desc_arr->base_addr != NULL)
      exit(4);

  // call fcheck with a non-allocated allocatable object (rank 1)
  fcheck(desc_arr);
  if (desc_arr->base_addr != NULL)
      exit(5);

  // Allocate rank 1 object on the C side
  lb1[0] = 2;
  ub1[0] = 5;
  rc = CFI_allocate(desc_arr, lb1, ub1, ZERO);
  assert(CFI_SUCCESS == rc);
  assert(desc_arr->base_addr);

  // call fcheck with an allocated allocatable object 
  fcheck(desc_arr);
  if (desc_arr->base_addr != NULL)
      exit(6);

  // call fcheck with a non-allocated allocatable object (rank 1)
  fcheck(desc_arr);
  if (desc_arr->base_addr != NULL)
      exit(7);

  // establishing an allocatable array of rank 2
  rc = CFI_establish(desc_arr2D,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_float,
                      sizeof(float),
                      TWO,
                      NULL);

  assert(rc == CFI_SUCCESS);
  assert(arr2D.attribute == CFI_attribute_allocatable);
  assert(arr2D.type == CFI_type_float);
  assert(arr2D.rank == TWO);

  // call fcheck with a non-allocated allocatable object of rank 2
  fcheck(desc_arr2D);
  if (desc_arr2D->base_addr != NULL)
      exit(8);

  // Allocate rank 2 object on the C side
  rc = CFI_allocate(desc_arr2D, lb2, ub2, ZERO);
  assert(CFI_SUCCESS == rc);
  assert(desc_arr2D->base_addr);

  // call fcheck with an allocated allocatable object 
  fcheck(desc_arr2D);
  if (desc_arr2D->base_addr != NULL)
      exit(9);

  // call fcheck with a non-allocated allocatable object (rank 2)
  fcheck(desc_arr2D);
  if (desc_arr2D->base_addr != NULL)
      exit(10);

  // establishing an allocatable array of rank 15
  rc = CFI_establish(desc_arr_big,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_float,
                      sizeof(float),
                      BIG,
                      NULL);

  assert(rc == CFI_SUCCESS);
  assert(arr_big.attribute == CFI_attribute_allocatable);
  assert(arr_big.type == CFI_type_float);
  assert(arr_big.rank == BIG);

  // call fcheck with a non-allocated allocatable object of rank 15
  fcheck(desc_arr_big);
  if (desc_arr_big->base_addr != NULL)
      exit(11);

  // Allocate rank 1 object on the C side
  rc = CFI_allocate(desc_arr_big, lb15, ub15, ZERO);
  assert(CFI_SUCCESS == rc);
  assert(desc_arr_big->base_addr);
  if (desc_arr_big->base_addr == NULL)
      exit(12);

  // call fcheck with an allocated allocatable object of rank 15
  fcheck(desc_arr_big);
  if (desc_arr_big->base_addr != NULL)
      exit(13);

  // call fcheck with a non-allocated allocatable object (rank 15)
  fcheck(desc_arr_big);
  if (desc_arr_big->base_addr != NULL)
      exit(14);

  return 0;
}
