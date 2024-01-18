#include <stdio.h>
#include <math.h>
#include "cmplx.h"

void extsub_int(signed char i, signed short j, signed int k, 
                signed long long l, signed long long *m){

     *m = i + j + k + l;
     i = i + 3;
     j = j + 3;
     k = k + 3;
     l = l + 3;
 }

  void extsub_real(float i, double j, double *l){

     *l = i + j ;
     i = i * 2;
     j = j * 2;
  }

   void extsub_char(char j, char *i){
   *i = j + 3;
    j = *i;
   }

   void extsub_log(unsigned char j, unsigned char *i ){

     *i = j + 1;
     j = 1;
     
   }

   void extsub_comp(float _Complex a, double _Complex b, 
                    float _Complex *c, double _Complex *d ){
     *c = createcomplexf(crealf(a), cimagf(a));
     *d = createcomplex(creal(b), cimag(b));
     a = createcomplexf(1.0f, 1.0f);
     b = createcomplex(1.0, 1.0);
     
   }

