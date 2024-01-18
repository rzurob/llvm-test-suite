#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void check_function_allocate(CFI_cdesc_t* a)
{
       CFI_index_t lower_bounds[] = {0}; // igonered when scalar
       CFI_index_t upper_bounds[] = {0}; // igonered when scalar
       int ind = CFI_allocate(a, lower_bounds, upper_bounds, 0);
       if (ind != CFI_SUCCESS)
       {
          fprintf(stderr, "CFI_allocate returns: %s\n", __xlf_CFI_strerror(ind));
       }
       return;
}
void check_function_deallocate(CFI_cdesc_t* a)
{
       int ind = CFI_deallocate(a);
       if (ind != CFI_SUCCESS)
       {
          fprintf(stderr, "CFI_deallocate: %s\n", __xlf_CFI_strerror(ind));
       }
       return;
}
