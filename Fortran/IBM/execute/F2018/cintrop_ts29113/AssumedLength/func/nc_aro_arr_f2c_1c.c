#include <stdio.h>
#include <assert.h>
#include "ISO_Fortran_binding.h"

void show_rank_all(CFI_cdesc_t* a);
void show_content_r1(CFI_cdesc_t* a);
void show_content_r2(CFI_cdesc_t* a);
void show_content_r3(CFI_cdesc_t* a);

void check_f_to_c(CFI_cdesc_t* a,int *c_len, int *rank, int *test_no)
{
    #if _DEBUG
    printf("Test No: %d\n", *test_no);
    show_rank_all(a);
    if(a->rank == 1){
       show_content_r1(a);
    }
    else if(a->rank == 2){
       show_content_r2(a);
    }
    else if(a->rank == 3){    
      // show_content_r3(a);
    }
    #endif

  assert(a->base_addr != NULL);
  if(a->rank ==1){
     CFI_index_t subscripts_a[1] = {0};
     assert(a->base_addr == CFI_address(a, subscripts_a));
  }
  if(a->rank ==2){
     CFI_index_t subscripts_a[2] = {0,0};
     assert(a->base_addr == CFI_address(a, subscripts_a));
  }
  if(a->rank ==3){
     CFI_index_t subscripts_a[3] = {0,0,0};
     assert(a->base_addr == CFI_address(a, subscripts_a));
  }
  assert(a->elem_len  == *c_len);
  assert(a->attribute == 4);
  assert(a->type == 1);
  assert(a->rank == *rank);
  switch(*test_no)
  {
    case 1:
        assert(a->dim[0].extent == 1);
        assert(a->dim[0].sm == *c_len);
        break;
    case 2:
        assert(a->dim[0].extent == 1);
        assert(a->dim[0].sm == *c_len);
        assert(a->dim[1].extent == 1);
        assert(a->dim[1].sm == *c_len);
        break;
    case 3:
        assert(a->dim[0].extent == 3);
        assert(a->dim[0].sm == *c_len);
        break;
    case 4:
        assert(a->dim[0].extent == 5);
        assert(a->dim[0].sm == *c_len);
        assert(a->dim[1].extent == 5);
        break;
    case 5:
        assert(a->dim[0].extent == 5);
        assert(a->dim[1].extent == 7);
        assert(a->dim[2].extent == 10 );
        assert(a->dim[0].sm == *c_len);
        break;
    case 6:
        assert(a->dim[0].extent == 10);
        assert(a->dim[1].extent == 10);
        break;
    case 7:
        assert(a->dim[0].extent == 5);
        assert(a->dim[1].extent == 5);
        assert(a->dim[2].extent == 5 );
        break;
    case 8:
        assert(a->dim[0].extent == 3);
        assert(a->dim[1].extent == 3);
        break;
    case 9:
        assert(a->dim[0].extent == 2);
        assert(a->dim[1].extent == 2);
        break;
    case 10:
        assert(a->dim[0].extent == 5);
        assert(a->dim[1].extent == 5);
        break;
  }   
  for(int i=0; i<a->rank ; i++){
     assert(a->dim[i].lower_bound == 0);
  } 
  return;
}
void show_rank_all(CFI_cdesc_t* a){
  for(int i=0; i<a->rank ; i++){
  printf("For dim %d, the extent is %d, lower bound is %d and the stride is %d\n", i,  a->dim[i].extent, a->dim[i].lower_bound, a->dim[i].sm);       
  }
  return;
}
void show_content_r1(CFI_cdesc_t* a){
  char *p = (char *) a->base_addr;
  if(a->dim[0].extent != -1){
  printf("content of a is :\n");
  for(int i=0;i<a->dim[0].extent; i++){
     for(int j=0; j< a->elem_len; j++){
        printf("%c", p[j]);
     }
     printf(" ");
     p = p+a->elem_len;
  }
  printf("\n");
  }  
}
void show_content_r2(CFI_cdesc_t* a){
  char *p = (char *) a->base_addr;
  if(a->dim[1].extent != -1){
  printf("content of a is :\n");
  for(int i=0;i<a->rank; i++){
     printf("Dim %d:", i);
     for(int j=0; j< a->dim[i].extent; j++){
        for(int k=0; k< a->elem_len; k++){
          printf("%c", p[k]);
        }
        printf(" ");
        p = p+a->elem_len;
     } 
     printf("\n");
  } 
}
}


















   


     


