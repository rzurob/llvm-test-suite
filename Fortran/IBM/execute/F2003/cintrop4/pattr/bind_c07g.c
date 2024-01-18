#include <stdio.h>
#include <math.h>
#include "cmplx.h"

 signed char exfun_int1(signed char i){

     return i + 3;
 }

 signed short exfun_int2(signed short j){

     return j + 3;
 }

 signed int exfun_int4(signed int k){

     return k + 3;
 }

 signed long long exfun_int8(signed long long l){

     return  l + 3;
 }

  float exfun_real4(float i){

     return i * 2;
  }


  double exfun_real8(double j){

     return  j * 2;
  }


   char exfun_char(char i){
      return i;
   }

   unsigned char exfun_log1(unsigned char i){

     return i + 1;
   }

  float _Complex exfun_comp1(float _Complex i) {
    
    return createcomplexf(crealf(i) + 1.0f, cimagf(i) + 1.0f);
  }
  
  double _Complex exfun_comp2(double _Complex i) {
    
    return createcomplex(creal(i) + 1.0, cimag(i) + 1.0);
  }

