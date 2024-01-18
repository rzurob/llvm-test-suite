#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void show_rank_all(CFI_cdesc_t* a, int d0_ext, int d1_ext);
void show_content_r2(CFI_cdesc_t* a);

void asar2_2(CFI_cdesc_t* a,int *c_len, int *d0_ext, int *arr_size)
{
   #if _DEBUG
     show_rank_all(a, *d0_ext, -1);
     show_content_r2(a);
  #endif

  assert(a->base_addr != NULL);
  assert(CFI_is_contiguous(a)==1);
  assert(a->elem_len  == *c_len);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 2);
  assert(a->dim[0].extent == (*c_len+4));
  assert(a->dim[0].lower_bound == 0);
  assert(a->dim[0].sm == *c_len);
  assert(a->dim[1].extent == -1);
  assert(a->dim[1].lower_bound == 0);
  assert(a->dim[1].sm == (*c_len)*(*c_len+4));
  return;
}
void show_rank_all(CFI_cdesc_t* a, int d0_ext, int d1_ext){
     for(int i=0; i<a->rank ; i++){
        printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", i,  a->dim[i].extent, a->dim[i].lower_bound, a->dim[i].sm);        
     }
     return;
}
void show_content_r2(CFI_cdesc_t* a){
  char *p = (char *) a->base_addr;
  printf("content of a is :\n");
  for(int i=0;i<a->rank; i++){
     printf("Dim %d elements\n", i);
     for(int j=0; j< a->dim[i].extent; j++){
        for(int k=0; k<a->dim[0].sm; k++){
          printf("%c", p[k]);
        }
        printf(" ");
        p = p+a->elem_len;
     } 
     printf("\n");
  } 
}

   


     


