#include <stdio.h>
#include <math.h>
#include "cmplx.h"

 signed char exfun_int1(signed char *i){
    if (*i >  0){
       *i = *i - 1;
      return 2 + exfun_int1(i);
     }
    return 0;
 }

 signed short exfun_int2(signed short *i){
    
     if (*i>  0){
       *i = *i - 1;
      return 2 + exfun_int2(i);
     }
     return 0;
 }

 signed int exfun_int4(signed int *i){

     if (*i>  0){
       *i = *i - 1;
      return 2 + exfun_int4(i);
     }
     return *i; 
 }

 signed long long exfun_int8(signed long long *i){

    if (*i > 0){
       *i = *i - 1;
      return 2 + exfun_int8(i);
     }
    return *i;
 }

  float exfun_real4(float *i, int *j){
     if ( *j > 0 ) {
       *j = *j - 1;
       return *i + exfun_real4(i, j);
     }
     return *i;
  }


  double exfun_real8(double *j, int *i){
     if (*i > 0)
      {  *i = *i - 1;
         return *j + exfun_real8(j, i);
      }
     return *j;
  }


  float _Complex exfun_comp1(float _Complex *i, int *j) {
     float _Complex temp; 
     if (*j > 0) {
       *j = *j - 1;
       temp = (exfun_comp1(i, j));
       return createcomplexf(1 + crealf(temp), 2 + cimagf(temp));
     }
     return *i;
  }
  
  double _Complex exfun_comp2(double _Complex *i, int *j) {
    double _Complex temp;
       if (*j > 0) {
       *j = *j - 1;
       temp = (exfun_comp2(i, j));
       return createcomplexf(1 + creal(temp), 2 + cimag(temp));
     }
     return *i;

  }

