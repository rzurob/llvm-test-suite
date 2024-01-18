#include <stdio.h>
#include <math.h>
#include "cmplx.h"

void extsub_int(signed char *i, signed short *j, signed int *k, 
                signed long long *l, signed long long *m){

     *m = *i + *j + *k + *l;
 }

  void extsub_real(float *i, double *j, double *l){

     *l = *i + *j ;
  }

   void extsub_char(char *j, char *i){
   *i = *j + 3;
   }

   void extsub_log(unsigned char *j, unsigned char *i ){

     *i = *j + 1;
     
   }

   void extsub_comp(float _Complex *a, double _Complex *b, 
                    float _Complex *c, double _Complex *d ){
     *c = createcomplexf(crealf(*a), cimagf(*a));
     *d = createcomplex(creal(*b), cimag(*b));
     
   }

