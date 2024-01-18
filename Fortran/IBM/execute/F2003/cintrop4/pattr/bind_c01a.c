#include <stdio.h>
#include <math.h>
#include "cmplx.h"

 void extsub_int(signed char *i, signed short *j, signed int *k, signed long long *l){

     *i = *i + 3;
     *j = *j + 3;
     *k = *k + 3;
     *l = *l + 3;
 }

  void extsub_real(float *i, double *j){

     *i = *i * 2;
     *j = *j * 2;
  }

   void extsub_char(char *i){
   *i = 'd';
   }

   void extsub_log(unsigned char *i ){

     *i = 1;
     
   }

   void extsub_comp(float _Complex *a, double _Complex *b){
    *a = createcomplexf(1.0f, 1.0f);
    *b = createcomplex(1.0, 1.0);
     
   }

