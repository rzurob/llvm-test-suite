/**********************************************************************
*  ===================================================================
*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
*  ===================================================================
*
*  TEST CASE TITLE            : AssumedType01f
*
*  PROGRAMMER                 : Dorra Bouchiha 
*  DATE                       : June 13, 2012
*  ORIGIN                     : AIX Compiler Development,
*                             : IBM Software Solutions Toronto Lab
*
*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
*
*  SECONDARY FUNCTIONS TESTED : None
*
*  DRIVER STANZA              : xlc
*  REQUIRED COMPILER OPTIONS  :
*
*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
*                               where the procedure is defined in C
*                               This test case focuses on C types that 
*                               are interoperable with INTEGER
*   
***********************************************************************
1234567890123456789012345678901234567890123456789012345678901234567890*/

#include <stdio.h>
#include <complex.h>
#include <stdint.h>

void c_sub(void* a, int flag)
{
      union
      {
        float complex zfloat;
        float r[2];
      } s;

      union
      {
        double complex zdouble;
        double p[2];
      } y;

      union
      {
        long double complex zlong;
        long double q[2];
      } x;

   if(flag == 1){
      printf("Type is short, the received value is %hd\n",*(short*)a);      
      fflush(stdout);
      }
   else if(flag == 2){
      printf("Type is int, the received value is %d\n",*(int*)a);      
      fflush(stdout);
      }
   else if(flag == 3){
      printf("Type is long, the received value is %ld\n",*(long*)a);    
      fflush(stdout);
      }
   else if(flag == 4){
      printf("Type is long_long, the received value is %lld\n",*(long long *)a);  
      fflush(stdout);
      }
   else if(flag == 5){
      printf("Type is signed char, the received value is %d\n",*(signed char *)a);  
      fflush(stdout);
      }
   else if(flag == 6){
      printf("Type is int8_t, the received value is %d\n",*(int8_t *)a);  
      fflush(stdout);
      }
   else if(flag == 7){
      printf("Type is int16_t, the received value is %d\n",*(int16_t *)a);  
      fflush(stdout);
      }
   else if(flag == 8){
      printf("Type is int32_t, the received value is %d\n",*(int32_t *)a);  
      fflush(stdout);
      }
   else if(flag == 9){
      printf("Type is int64_t, the received value is %lld\n",*(int64_t *)a);  
      fflush(stdout);
      }
   else if(flag == 10){
      printf("Type is int_least8_t, the received value is %d\n",*(int_least8_t *)a);  
      fflush(stdout);
      }
   else if(flag == 11){
      printf("Type is int_least16_t, the received value is %d\n",*(int_least16_t *)a);  
      fflush(stdout);
      }
   else if(flag == 12){
      printf("Type is int_least32_t, the received value is %d\n",*(int_least32_t *)a);  
      fflush(stdout);
      }
   else if(flag == 13){
      printf("Type is int_least64_t, the received value is %lld\n",*(int_least64_t *)a);  
      fflush(stdout);
      }
   else if(flag == 14){
      printf("Type is int_fast8_t, the received value is %d\n",*(int_fast8_t *)a);  
      fflush(stdout);
      }
   else if(flag == 15){
      printf("Type is int_fast16_t, the received value is %d\n",*(int_fast16_t *)a);  
      fflush(stdout);
      }
   else if(flag == 16){
      printf("Type is int_fast32_t, the received value is %d\n",*(int_fast32_t *)a);  
      fflush(stdout);
      }
   else if(flag == 17){
      printf("Type is int_fast64_t, the received value is %lld\n",*(int_fast64_t *)a);  
      fflush(stdout);
      }
   else if(flag == 18){
      printf("Type is intmax_t, the received value is %lld\n",*(intmax_t *)a);  
      fflush(stdout);
      }
   else if(flag == 19){
      printf("Type is intptr_t, the received value is %d\n",*(intptr_t *)a);  
      fflush(stdout);
      }
   else if(flag == 20){
      printf("Type is float, the received value is %f\n",*(float *)a);  
      fflush(stdout);
      }
   else if(flag == 21){
      printf("Type is double, the received value is %f\n",*(double *)a);  
      fflush(stdout);
      }
   else if(flag == 22){
      printf("Type is long_double, the received value is %Lf\n",*(long double *)a);  
      fflush(stdout);
      }
   else if(flag == 23){
      s.zfloat = *(float complex *) a;
      printf("Type is float_complex, the received value is %f %f\n",s.r[0], s.r[1]);  
      fflush(stdout);
      }
   else if(flag == 24){
      y.zdouble = *(double complex *) a;
      printf("Type is double_complex, the received value is %f %f\n",y.p[0], y.p[1]);  
      fflush(stdout);
      }
   else if(flag == 25){
      x.zlong = *(long double complex *) a;
      printf("Type is long_double_complex, the received value is %Lf %Lf\n",x.q[0], x.q[1]);  
      fflush(stdout);
      }
   else if(flag == 26){
      printf("Type is bool, the received value is %d\n",*(_Bool *)a);  
      fflush(stdout);
      }
   else if(flag == 27){
      printf("Type is char, the received value is %c\n",*(char *)a);  
      fflush(stdout);
      }
   else if(flag == 28){
      printf("Type is size_t, the received value is %zd\n",*(size_t*)a);  
      fflush(stdout);
      }

  return;
}
