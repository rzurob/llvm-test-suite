#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void show_rank_all(CFI_cdesc_t* a, int d0_ext, int d1_ext, int d2_ext);
void show_content_r3(CFI_cdesc_t* a);

void esa3r_1(CFI_cdesc_t* a,int *c_len, int *d0_ext, int *d1_ext, int *d2_ext)
{
  #if _DEBUG
  show_rank_all(a, *d0_ext, *d1_ext, *d2_ext);
  show_content_r3(a);
  #endif

  assert(a->base_addr != NULL);
  assert(a->elem_len  == *c_len);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 3);
  assert(a->dim[0].extent == *d0_ext);
  assert(a->dim[0].lower_bound == 0);
  assert(a->dim[0].sm == *c_len);
  assert(a->dim[1].extent == *d1_ext);
  assert(a->dim[1].lower_bound == 0);
  assert(a->dim[1].sm == (*c_len)*(*d0_ext));
  assert(a->dim[2].extent == *d2_ext);
  assert(a->dim[2].lower_bound == 0);
  assert(a->dim[2].sm == (*c_len)*(*d0_ext)*(*d1_ext));
  assert(CFI_is_contiguous(a)==1);
  return;
}
void show_rank_all(CFI_cdesc_t* a, int d0_ext, int d1_ext, int d2_ext){
     for(int i=0; i<a->rank ; i++){
        printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", i,  a->dim[i].extent, a->dim[i].lower_bound, a->dim[i].sm);        
     }     
     return;
}

void show_content_r3(CFI_cdesc_t* a){
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

   


     


