#include <stdio.h>
#include <math.h>
#include "cmplx.h"

 void extsub_int(signed char *i, signed short *j, signed int *k, signed long long *l, signed int *b){
     
      if(*b <= 0) return;
         else {
                *i = *i + 1;
                *j = *j + 1;
                *k = *k + 1;
                *l = *l + 1;
                *b = *b - 1;
                extsub_int(i, j, k, l, b);
              }   
 }

  void extsub_real(float *i, double *j, signed int *b){
      if(*b <= 0) return;
         else {
             *i = *i - 1;
             *j = *j - 1;
             *b = *b - 1;
             extsub_real(i, j, b);
              }
  }

   void extsub_char(char *i){
   if (*i != 'd') {
      *i = *i - 1;
      extsub_char(i);
    }
   }
   
   void extsub_comp(float _Complex *a, double _Complex *b, int *i) {
      if ( *i <= 0 ) return;
      *a = createcomplexf(crealf(*a) - 1.0f, cimagf(*a) - 2.0f);
      *b = createcomplex(creal(*b) - 1.0, cimag(*b) - 2.0);
      *i = *i - 1;
      extsub_comp(a, b, i);
   }  
/*  void extsub_log(unsigned char *i, unsigned short *j, unsigned int *k, unsigned long long *l){

     *i = *l;
     *j = *l;
     *k = *i;
     *l = *i;
   }
*/








