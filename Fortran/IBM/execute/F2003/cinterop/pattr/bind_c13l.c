#include <stdio.h>
#include <math.h>
#include "cmplx.h"


  void extsub_real(long double *k){

     *k = *k * 2;
  }


   void extsub_comp(long double _Complex *b){
    *b = createcomplexl(1.0, 1.0);
     
   }

