#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

// Prototype for the function that is defined on the Fortran side.
void matrix_mult_fortran(CFI_cdesc_t * res, CFI_cdesc_t * a, CFI_cdesc_t * b);

// Function to print a Fortran array described by a CFI descriptor
void print2DFortranArrayOfDoubles(CFI_cdesc_t * d);

/***********************************************************************
 * This program shows how to use C-interoperable allocatable and pointer
 * arrays to multiply the following matrixes on the Fortran side and
 * display the resulting matrix on the C side.
 *
 * | 3, 5|   |1 0|
 * | 6, 7| X |   |
 * |-1, 2|   |2 1|
 *
 ***********************************************************************/
int main()
{
  CFI_CDESC_T(2) a, b, res;
  CFI_cdesc_t * desc_a = (CFI_cdesc_t *) &a;
  CFI_cdesc_t * desc_b = (CFI_cdesc_t *) &b;
  CFI_cdesc_t * desc_res = (CFI_cdesc_t *) &res;

  // Since Fortran is column-major and C is row-major, the input matrixes
  // defined on the C side are transposed.
  double matrixa[2][3] = { {3, 6, -1}, {5, 7, 2} };
  double matrixb[2][2] = { {1, 2}, {0, 1} };
  const CFI_index_t a_extents[2] = { 3, 2 };
  const CFI_index_t b_extents[2] = { 2, 2 };
  int rc;
  
  rc = CFI_establish(desc_a,
                     matrixa,
                     CFI_attribute_pointer,
                     CFI_type_double,
                     sizeof(double),
                     (CFI_rank_t)2,
                     a_extents);
  assert(CFI_SUCCESS == rc);

  rc = CFI_establish(desc_b,
                     matrixb,
                     CFI_attribute_pointer,
                     CFI_type_double,
                     sizeof(double),
                     (CFI_rank_t)2,
                     b_extents);
  assert(CFI_SUCCESS == rc);

  rc = CFI_establish(desc_res,
                     NULL,
                     CFI_attribute_allocatable,
                     CFI_type_double,
                     sizeof(double),
                     (CFI_rank_t)2,
                     NULL);
  assert(CFI_SUCCESS == rc);

  // Call Fortran to do the actual matrix multiply
  matrix_mult_fortran(desc_res, desc_a, desc_b);

  // Make sure the result was allocated on the Fortran side
  assert(desc_res->base_addr); 

  print2DFortranArrayOfDoubles(desc_res);

  rc = CFI_deallocate(desc_res);
  assert(CFI_SUCCESS == rc);
  return 0;
}

void print2DFortranArrayOfDoubles(CFI_cdesc_t * d)
{
  CFI_index_t extents[2];
  CFI_index_t strides[2];
  int i, j;
  char * prow;

  prow = (char *)d->base_addr;
  extents[0] = d->dim[0].extent;
  extents[1] = d->dim[1].extent;
  strides[0] = d->dim[0].sm;
  strides[1] = d->dim[1].sm;

  // Print the values of the array one row at a time.
  // Note: This is not cache friendly, since Fortran arrays are column major.
  for(i = 0; i < extents[0]; i++)
  {
    char * tp = prow;
    for(j = 0; j < extents[1]; j++)
    {
      printf("%.1f ", *(double *)tp);
      tp += strides[1];
    }
    prow += strides[0];
    printf("\n");
  }
}
