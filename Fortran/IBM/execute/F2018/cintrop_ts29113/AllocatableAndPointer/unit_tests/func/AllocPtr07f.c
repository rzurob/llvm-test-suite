#include <stdlib.h>
#include <stdio.h>
#include "ISO_Fortran_binding.h"

void print2dfarr(CFI_cdesc_t * d, int test);

void initfptr(CFI_cdesc_t * d, int * st, CFI_cdesc_t * src)
{
  int rc;
  CFI_index_t strides[2];
  
  if (st != NULL)
  {
    strides[0] = (CFI_index_t)(*st);
    strides[1] = 1;
  }

  if (src->base_addr == NULL)
  {
    fprintf(stderr, "Source array is not associated!\n");
    exit(1);
  }

  if (src->rank != 2)
  {
    fprintf(stderr, "Source array should have rank 2!\n");
    exit(1);
  }

  if (st != NULL)
    rc = CFI_section(d, src, NULL, NULL, strides);
  else
    rc = CFI_section(d, src, NULL, NULL, NULL);

  if (rc != CFI_SUCCESS)
  {
    fprintf(stderr, "Failed to associate section!\n");
    exit(2);
  }
  
}

void print2dfarr(CFI_cdesc_t * d, int test)
{
  CFI_index_t extents[2];
  CFI_index_t strides[2];
  int i, j;
  char * prow;

  if (d->rank != 2)
  {
    fprintf(stderr, "Expected rank 2 array!\n");
    exit(3);
  }
  
  if (d->type != CFI_type_double)
  {
    fprintf(stderr, "Expected array of type double!\n");
    exit(4);
  }

  printf("Test %d:\n", test);

  if (d->attribute == CFI_attribute_allocatable)
    printf("Attr: CFI_allocatable\n");
  else if (d->attribute == CFI_attribute_pointer)
    printf("Attr: CFI_pointer\n");
  else
    printf("Attr: CFI_other\n");

  prow = (char *)d->base_addr;
  extents[0] = d->dim[0].extent;
  extents[1] = d->dim[1].extent;
  strides[0] = d->dim[0].sm;
  strides[1] = d->dim[1].sm;

  if (prow == NULL)
  {
    printf("Null Array\n");
    return;
  }

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
