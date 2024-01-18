#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"


typedef struct dt1 DT1;
typedef struct dt2 DT2;

struct dt1 {
    int a;
    int b[2];
};

struct dt2 {
    float   c[2];
    struct dt1 d1;
};


void c_sub_1d(short int *arg1, CFI_cdesc_t * ptr, short int *arg2)
{
  int i, j;
  CFI_index_t subscripts[1];
  CFI_index_t lower[1], extents[1], strides[1];

  printf ("In c_sub_1D \n");

  if (ptr->base_addr == NULL) {
      printf("ptr is not associated...\n");
      printf("...expecting an associated pointer\n");
      printf("...exiting... \n");
      exit(1);
  }

  strides[0] = ptr->dim[0].sm;
  extents[0] = ptr->dim[0].extent;
  lower[0] = ptr->dim[0].lower_bound;
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0, extents[0], lower[0], strides[0]);
  assert(lower[0] == 1);
  assert(extents[0] == 5);
  assert(strides[0] == 12); 

  DT1 *address;
  for (int i=lower[0]; i<(lower[0]+extents[0]); i++) {
      subscripts[0] = i;
      address = (DT1*) CFI_address(ptr, subscripts);
      printf("the value stored in the field a of element %d is: %d\n", i, address->a);
      printf("the values stored in the field b of element %d are: %d, %d\n", i, address->b[0], address->b[1]);
  }

  if (arg1 != NULL)
    printf("arg1 = %d\n", *arg1);
  else
    printf("arg1 = absent\n");

  if (arg2 != NULL)
    printf("arg2 = %d\n", *arg2);
  else
    printf("arg2 = absent\n");
}

void c_sub_1d_opt(CFI_cdesc_t *arg1, CFI_cdesc_t *ptr,CFI_cdesc_t *arg2)
{
  int i, j;
  CFI_index_t subscripts[1];
  CFI_index_t lower[1], extents[1], strides[1];

  printf ("In c_sub_1D_opt\n");

  if (arg1 != NULL){
	  if (arg1->base_addr == NULL) {
	      printf("arg1 is not associated...\n");
	      printf("...expecting an associated pointer\n");
	      printf("...exiting... \n");
	      exit(3);
	  }
          short *address;
          address = (short*) CFI_address(arg1, NULL);
	  printf("arg1 = %d\n", *address);
  }
  else{
	  printf("arg1 = absent\n");
  }

  if (ptr->base_addr == NULL) {
      printf("ptr is not associated...\n");
      printf("...expecting an associated pointer\n");
      printf("...exiting... \n");
      exit(1);
  }

  if (arg2 != NULL){
	  if (arg2->base_addr == NULL) {
	      printf("arg2 is not associated...\n");
	      printf("...expecting an associated pointer\n");
	      printf("...exiting... \n");
	      exit(4);
	  }
          short *address;
          address = (short*) CFI_address(arg2, NULL);
	  printf("arg2 = %d\n", *address);
  }
  else{
	  printf("arg2 = absent\n");
  }

  strides[0] = ptr->dim[0].sm;
  extents[0] = ptr->dim[0].extent;
  lower[0] = ptr->dim[0].lower_bound;
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0, extents[0], lower[0], strides[0]);
  assert(lower[0] == 1);
  assert(extents[0] == 5);
  assert(strides[0] == 12); 

  DT1 *address;
  for (int i=lower[0]; i<(lower[0]+extents[0]); i++) {
      subscripts[0] = i;
      address = (DT1*) CFI_address(ptr, subscripts);
      printf("the value stored in the field a of element %d is: %d\n", i, address->a);
      printf("the values stored in the field b of element %d are: %d, %d\n", i, address->b[0], address->b[1]);
  }

}

int c_fnc_2d(CFI_cdesc_t * ptr)
{
  int i, j;
  int rc;
  CFI_index_t subscripts[2];
  CFI_index_t lower[2], extents[2], strides[2];

  printf ("In c_fnc_2D \n");

  if (ptr->base_addr == NULL) {
      printf("ptr is not associated... \n");
      printf("...expecting an associated var \n");
      printf("...exiting... \n");
      exit(2);
  }

  /* test lower bound, extent, stride for each dinension */

  strides[0] = ptr->dim[0].sm;
  extents[0] = ptr->dim[0].extent;
  lower[0] = ptr->dim[0].lower_bound;
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 0, extents[0], lower[0], strides[0]);
  assert(lower[0] == 1);
  assert(extents[0] == 5);
  assert(strides[0] == 20);

  strides[1] = ptr->dim[1].sm;
  extents[1] = ptr->dim[1].extent;
  lower[1] = ptr->dim[1].lower_bound;
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", 1, extents[1], lower[1], strides[1]);
  assert(lower[1] == 1);
  assert(extents[1] == 3);
  assert(strides[1] == (extents[0]*strides[0])); 

  /* verify values */

  DT2 *address;

  for (int j=lower[1]; j<(lower[1]+extents[1]); j++) {
      subscripts[1] = j;
      for (int i=lower[0]; i<(lower[0]+extents[0]); i++) {
              subscripts[0] = i;
	      address = (DT2*) CFI_address(ptr, subscripts);
              printf("the values stored in c of element %d are: %f, %f\n", i, address->c[0], address->c[1]);
	      printf("the value stored in the field a of element %d is: %d\n", i, address->d1.a);
	      printf("the values stored in the field b of element %d are: %d, %d\n", i, address->d1.b[0], address->d1.b[1]);
      }
  }

  rc = extents[0]*extents[1];
  return rc;
}
