#include <stdio.h>
#include <assert.h>
#include <inttypes.h>
#include "ISO_Fortran_binding.h"

#define ZERO 0
#define ONE 1
#define TWO 2
#define BIG 15

void fcheck(int* flag, CFI_cdesc_t* a); 
/* Non-allocatable non-pointer assumed rank object: 
lower boud is 0 on the C-side and 1 on the Fortran side 
flag set to 0: rank 0 
flag set to 0: rank 0 
flag set to 0: rank 0 
*/

int main()
{

  CFI_CDESC_T(ZERO) i1;
  CFI_CDESC_T(ONE) arr, src;
  CFI_CDESC_T(TWO) arr2D;
  CFI_CDESC_T(BIG) arr_big;

  CFI_cdesc_t * desc_i1 = (CFI_cdesc_t *) &i1;
  CFI_cdesc_t * desc_arr = (CFI_cdesc_t *) &arr;
  CFI_cdesc_t * desc_src = (CFI_cdesc_t *) &src;
  CFI_cdesc_t * desc_arr2D = (CFI_cdesc_t *) &arr2D;
  CFI_cdesc_t * desc_arr_big = (CFI_cdesc_t *) &arr_big;

  CFI_index_t lb1[ONE] = {0}, ub1[ONE] = {10};
  CFI_index_t lb2[TWO] = {1, 1}, ub2[TWO] = {2, 3};  // array of 2 rows and 3 columns
  CFI_index_t lb15[BIG] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
  CFI_index_t ub15[BIG] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3};

  int rc, flag;
  int_least8_t c_arr[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10} ;
  int_least8_t matrix[3][2] = { {3, -2} ,{2, 4}, {1, 1} }; // Since Fortran is column-major and C is row-major,
                                                     // the input matrixes defined on the C side are transposed.

  // establishing a non-allocatable non-pointer array of rank 0
  rc = CFI_establish(desc_i1,
                     NULL,
                     CFI_attribute_other,
                     CFI_type_int_least8_t, 
                     NULL,
                     ZERO,
                     NULL);
  printf(" %d\n", rc);

  assert(CFI_SUCCESS == rc);
  assert(i1.attribute == CFI_attribute_other); 
  assert(i1.type == CFI_type_int_least8_t);
  assert(i1.rank == ZERO);

  // call fcheck with a non-allocated allocatable scalar (rank 0)
  flag = 0;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Non-pointer Non-allocatable object of rank %d \n", desc_arr->rank);
  fflush(stdout);
#endif
  fcheck(&flag, desc_i1);

   /*
   A C descriptor with attribute CFI attribute other and base_addr a null pointer can be used as the
   argument result in calls to CFI section or CFI select part, which will produce a C descriptor for a nonal-
   locatable nonpointer data object.
   */

  // establishing a non-allocatable non-pointer array of rank 1
  rc = CFI_establish(desc_arr,
                     NULL,
                     CFI_attribute_other,
                     CFI_type_int_least8_t,
                     NULL,
                     ONE,
                     ub1);
  assert(rc == CFI_SUCCESS);
  assert(arr.attribute == CFI_attribute_other);
  assert(arr.type == CFI_type_int_least8_t);
  assert(arr.rank == ONE);

  // establishing a non-allocatable non-pointer array of rank 1
  rc = CFI_establish(desc_src,
                     c_arr,
                     CFI_attribute_other,
                     CFI_type_int_least8_t,
                     sizeof(int),
                     ONE,
                     ub1);
  assert(rc == CFI_SUCCESS);
  assert(src.attribute == CFI_attribute_other);
  assert(src.type == CFI_type_int_least8_t);
  assert(src.rank == ONE);
  assert(desc_src->dim[0].extent == 10);
  assert(desc_src->dim[0].lower_bound == 0);

  rc = CFI_section(desc_arr, desc_src, NULL, NULL, NULL);
  assert(rc == CFI_SUCCESS);
  assert(desc_arr->dim[0].extent == 10);
  assert(desc_arr->dim[0].lower_bound == 0);

  // call fcheck with non-pointer non-allocatable object 
  flag = 1;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Non-pointer Non-allocatable object of rank %d with %d elements\n", desc_arr->rank, desc_arr->dim[0].extent);
  printf("lower bound on the C-side is: %d\n", desc_arr->dim[0].lower_bound);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Non-pointer Non-allocatable object of rank %d with %d elements\n", desc_src->rank, desc_src->dim[0].extent);
  printf("lower bound on the C-side is: %d\n", desc_src->dim[0].lower_bound);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr);

  // establishing a non-allocatable non-pointer array of rank 2
  rc = CFI_establish(desc_arr2D,
                     matrix,
                     CFI_attribute_other,
                     CFI_type_int_least8_t,
                     sizeof(int),
                     TWO,
                     ub2);
  assert(rc == CFI_SUCCESS);
  assert(arr2D.attribute == CFI_attribute_other);
  assert(arr2D.type == CFI_type_int_least8_t);
  assert(arr2D.rank == TWO);
  assert(desc_arr2D->dim[0].extent == 2);
  assert(desc_arr2D->dim[0].lower_bound == 0);
  assert(desc_arr2D->dim[1].extent == 3);
  assert(desc_arr2D->dim[1].lower_bound == 0);

  // call fcheck with non-pointer non-allocatable object 
  flag = 2;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Non-pointer Non-allocatable object of rank %d with %d rows and %d columns\n", desc_arr2D->rank, desc_arr2D->dim[0].extent, desc_arr2D->dim[1].extent);
  printf("lower bounds on the C-side are %d and %d\n", desc_arr2D->dim[0].lower_bound, desc_arr2D->dim[1].lower_bound);
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr2D);

  // establishing a non-allocatable non-pointer array of rank 15
  rc = CFI_establish(desc_arr_big,
                     c_arr,
                     CFI_attribute_other,
                     CFI_type_int_least8_t,
                     sizeof(int),
                     BIG,
                     ub15);
  assert(rc == CFI_SUCCESS);
  assert(arr_big.attribute == CFI_attribute_other);
  assert(arr_big.type == CFI_type_int_least8_t);
  assert(arr_big.rank == BIG);

  // call fcheck with non-pointer non-allocatable object 
  flag = 15;
#if _DEBUG
  printf("Call Fortran with flag set to : %d\n", flag);
  printf("Allocatable object of rank %d \n", desc_arr_big->rank);
  for (int i=0; i<BIG; i++) {
      printf("extent: %d and lower bound %d of dimension %d \n", desc_arr_big->dim[i].extent, desc_arr_big->dim[i].lower_bound, i);
  }
  fflush(stdout);
#endif
  fcheck(&flag, desc_arr_big);

  for (int i=0; i<BIG; i++) {
      if(i == BIG-1)
          assert(desc_arr_big->dim[i].extent == 3);
      else 
          assert(desc_arr_big->dim[i].extent == 1);
      assert(desc_arr_big->dim[i].lower_bound == 0);
  }

  return 0;
}
