#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

#define ZERO 0
#define ONE 1
#define TWO 2
#define BIG 15

void fcheck(int* flag, CFI_cdesc_t* a); 

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

  int rc, flag;
  
  // establishing an allocatable array of rank 0
  rc = CFI_establish(desc_i1,
                     NULL,
                     CFI_attribute_allocatable,
                     CFI_type_int,
                     sizeof(int),
                     ZERO,
                     NULL);

  assert(CFI_SUCCESS == rc);
  assert(i1.attribute == CFI_attribute_allocatable);
  assert(i1.type == CFI_type_int);
  assert(i1.rank == ZERO);

  // call fcheck with a non-allocated allocatable scalar (rank 0)
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  fflush(stdout);
#endif
  fcheck(&flag, desc_i1);

  // Allocate rank 0 (scalar) on the C side
  rc = CFI_allocate(desc_i1, ZERO, ZERO, ZERO);
  assert(CFI_SUCCESS == rc);
  assert(desc_i1->base_addr);
  int *actual_i1 = i1.base_addr;

  // call fcheck with an allocated allocatable scalar 
  flag = 1;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Allocatable scalar (rank is %d)\n", desc_arr->rank);
  fflush(stdout);
#endif
  fcheck(&flag, desc_i1);

  // establishing an allocatable array of rank 1
  rc = CFI_establish(desc_arr,
                     NULL,
                     CFI_attribute_allocatable,
                     CFI_type_int,
                     sizeof(int),
                     ONE,
                     NULL);

  assert(rc == CFI_SUCCESS);
  assert(arr.attribute == CFI_attribute_allocatable);
  assert(arr.type == CFI_type_int);
  assert(arr.rank == ONE);

  // call fcheck with a non-allocated allocatable object (rank 1)
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // Allocate rank 1 object on the C side
  rc = CFI_allocate(desc_arr, lb1, ub1, ZERO);
  assert(CFI_SUCCESS == rc);
  assert(desc_arr->base_addr);

  // call fcheck with an allocated allocatable object 
  flag = 2;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Allocatable object of rank %d with %d elements\n", desc_arr->rank, desc_arr->dim[0].extent);
  printf("lower bound on the C-side is: %d\n", desc_arr->dim[0].lower_bound);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // deallocate arr on the C side
  rc = CFI_deallocate(desc_arr);
  assert(CFI_SUCCESS == rc);

  // call fcheck with a non-allocated allocatable object (rank 1)
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // Allocate rank 1 object on the C side
  lb1[0] = 2;
  ub1[0] = 5;
  rc = CFI_allocate(desc_arr, lb1, ub1, ZERO);
  assert(CFI_SUCCESS == rc);
  assert(desc_arr->base_addr);

  // call fcheck with an allocated allocatable object 
  flag = 3;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Allocatable object of rank %d with %d elements\n", desc_arr->rank, desc_arr->dim[0].extent);
  printf("lower bound on the C-side is: %d\n", desc_arr->dim[0].lower_bound);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // deallocate arr on the C side
  rc = CFI_deallocate(desc_arr);
  assert(CFI_SUCCESS == rc);

  // call fcheck with a non-allocated allocatable object (rank 1)
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // establishing an allocatable array of rank 2
  rc = CFI_establish(desc_arr2D,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_int,
                      sizeof(int),
                      TWO,
                      NULL);

  assert(rc == CFI_SUCCESS);
  assert(arr2D.attribute == CFI_attribute_allocatable);
  assert(arr2D.type == CFI_type_int);
  assert(arr2D.rank == TWO);

  // call fcheck with a non-allocated allocatable object of rank 2
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr2D);

  // Allocate rank 1 object on the C side
  rc = CFI_allocate(desc_arr2D, lb2, ub2, ZERO);
  assert(CFI_SUCCESS == rc);
  assert(desc_arr2D->base_addr);

  // call fcheck with an allocated allocatable object 
  flag = 4;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Allocatable object of rank %d with %d rows and %d columns\n", desc_arr2D->rank, desc_arr2D->dim[0].extent, desc_arr2D->dim[1].extent);
  printf("lower bounds on the C-side is: %d , %d\n", desc_arr2D->dim[0].lower_bound, desc_arr2D->dim[1].lower_bound);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr2D);

  // deallocate arr on the C side
  rc = CFI_deallocate(desc_arr2D);
  assert(CFI_SUCCESS == rc);

  // call fcheck with a non-allocated allocatable object (rank 1)
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr2D);

  // establishing an allocatable array of rank 15
  rc = CFI_establish(desc_arr_big,
                      NULL,
                      CFI_attribute_allocatable,
                      CFI_type_int,
                      sizeof(int),
                      BIG,
                      NULL);

  assert(rc == CFI_SUCCESS);
  assert(arr_big.attribute == CFI_attribute_allocatable);
  assert(arr_big.type == CFI_type_int);
  assert(arr_big.rank == BIG);

  // call fcheck with a non-allocated allocatable object of rank 2
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr_big);

  // Allocate rank 1 object on the C side
  rc = CFI_allocate(desc_arr_big, lb15, ub15, ZERO);
  assert(CFI_SUCCESS == rc);
  assert(desc_arr_big->base_addr);

  // call fcheck with an allocated allocatable object 
  flag = 5;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Allocatable object of rank %d \n", desc_arr_big->rank);
  for (int i=0; i<BIG; i++) {
      printf("extent: %d and lower bound %d of dimension %d \n", desc_arr_big->dim[i].extent, desc_arr_big->dim[i].lower_bound, i);
  }
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr_big);

  // deallocate arr on the C side
  rc = CFI_deallocate(desc_arr_big);
  assert(CFI_SUCCESS == rc);

  // call fcheck with a non-allocated allocatable object (rank 1)
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr_big);

  return 0;
}
