#include <stdio.h>
#include <string.h>
#include <complex.h> 

void c_sub(void* a, int buffer, int flag)
{
   if(flag == 1){
      short *p= (short*)a;
      for (int i=0; i<buffer; ++i) {
        p[i] = i; 
      }
   }
   else if(flag == 2){
      int *p= (int*)a;
      for (int i=0; i<buffer; ++i) {
        p[i] = i; 
      }
   } 
   else if(flag == 3){
      long *p= (long*)a;
      for (int i=0; i<buffer; ++i) {
        p[i] = 100; 
      }
   } 
   else if(flag == 4){
      float *p= (float*)a;
      for (int i=0; i<buffer; ++i) {
        p[i] = 99.*(i+1); 
      }
   } 
   else if(flag == 5){
      double *p= (double*)a;
      for (int i=0; i<buffer; ++i) {
        p[i] = 10.3*2*(i+1); 
      }
   } 
   else if(flag == 6){
      float complex *p = (float complex *)a; 
      *p = 10.0f + 10.0f * I;
   }
   else if(flag == 7){
      _Bool *p= (_Bool*)a;
      for (int i=0; i<buffer; ++i) {
        p[i] = 0; 
      }
   } 

  return;
}
