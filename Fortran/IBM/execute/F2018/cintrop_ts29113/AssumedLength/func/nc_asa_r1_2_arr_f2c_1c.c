#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"
void show_rank_all(CFI_cdesc_t* a, int c_len);
void show_content_r1(CFI_cdesc_t* a, int ext);

void asar1_1(CFI_cdesc_t* a,int *c_len, int *ext)
{

  #if _DEBUG
    show_rank_all(a, *c_len);
    //show_content_r1(a, *ext);
  #endif

  assert(a->base_addr != NULL);
  assert(CFI_is_contiguous(a)==1);
  assert(a->elem_len  == *c_len);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == 1);
  assert(a->dim[0].extent == -1);
  assert(a->dim[0].lower_bound == 0);
  assert(a->dim[0].sm == *c_len);
  return;
}
void show_rank_all(CFI_cdesc_t* a, int c_len){
     printf("Show Rank\n");
     for(int i=0; i<a->rank ; i++){
        printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", i,  a->dim[i].extent, a->dim[i].lower_bound, a->dim[i].sm);
     }     
     return;
}
void show_content_r1(CFI_cdesc_t* a, int ext){
  char *p = (char *) a->base_addr;
  printf(" content of a is : ");
  for(int i=0; i<ext; i++){
      for(int j=0; j<a->dim[0].sm; j++){
           printf("%c", p[j]);
      }
      printf("\n");
      p =(p+a->dim[0].sm); 
  }
}

   


     


