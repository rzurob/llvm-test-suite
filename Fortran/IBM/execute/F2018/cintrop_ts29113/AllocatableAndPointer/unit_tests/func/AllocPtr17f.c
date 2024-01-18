#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

short test_contig(CFI_cdesc_t * a);
short test_assoc(CFI_cdesc_t * a);

typedef struct _Point
{
  int x;
  int y;
} Point;

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

  int rc;
  int carray[10];
  Point parray[10];
  
  // You cannot test the storage before establishing the C-descriptor 

  // establishing the pointer to a contig target
  rc = CFI_establish(desc_arr,
                     carray,
                     CFI_attribute_pointer,
                     CFI_type_int,
                     sizeof(int),
                     1,
                     extents);
  assert(CFI_SUCCESS == rc);

  if (!test_assoc(desc_arr))
    exit(3);
  if (!test_contig(desc_arr))
    exit(4);

  // disassociating the pointer
  rc = CFI_setpointer(desc_arr, NULL, NULL);
  assert(CFI_SUCCESS == rc);

  if (test_assoc(desc_arr))
    exit(5);

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

  if (!test_assoc(desc_arr))
    exit(6);
  if (!test_contig(desc_arr))
    exit(7);

  // disassociate arr again
  rc = CFI_setpointer(desc_arr, NULL, NULL);
  assert(CFI_SUCCESS == rc);

  if (test_assoc(desc_arr))
    exit(8);
  
  // now set arr to a section of base_arr
  // Note we can use extents as ubounds, because lbounds are 1
  rc = CFI_section(desc_arr, base_desc_arr, lbounds, extents, strides);
  assert(CFI_SUCCESS == rc);

  if (!test_assoc(desc_arr))
    exit(9);
  if (test_contig(desc_arr))
    exit(10);

  // establishing the array of points
  rc = CFI_establish(point_desc_arr,
                     parray,
                     CFI_attribute_pointer,
                     CFI_type_struct,
                     sizeof(Point),
                     1,
                     extents);
  assert(CFI_SUCCESS == rc);

  if (!test_assoc(point_desc_arr))
    exit(11);

  // disassociate arr again
  rc = CFI_setpointer(desc_arr, NULL, NULL);
  assert(CFI_SUCCESS == rc);
  if (test_assoc(desc_arr))
    exit(12);

  // Now test select_part...
  rc = CFI_select_part(desc_arr, point_desc_arr, sizeof(int), NULL);
  assert(CFI_SUCCESS == rc);

  if (!test_assoc(desc_arr))
    exit(13);
  if (test_contig(desc_arr))
    exit(14);

  return 0;
}
