   #include <inttypes.h>

/* --------------------------------------------------------------* 
*        This C code is used by fxcmn_blk101.f                  * 
*        Please refer to fxcmn_blk101.f for further details.    * 
* --------------------------------------------------------------*/ 
  
  
#include <stdio.h> 
#include <stdlib.h> 
#include <stddef.h> 
  
  
signed char             _[2][2][2]; 
  
void csub_int(){ 
  
int i,j,k,a; 
  
// Temporary matrix used in modifying values passed from Fortran
         signed char             tmp_s1[2][2][2]; 
  
// Comparison matrix 
         signed char   cmp_s1[2][2][2] =  {127,0,-128,127,127,0,0,-111 }; 
  
  
 /* --------------------------------------------------------------* 
 *       1) Verify values from Fortran code                       * 
 * --------------------------------------------------------------*/ 
  
   a=0; 
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
         if ( _[i][j][k] !=  cmp_s1[i][j][k]             )    exit (30+a); 
  
         a=a+1; 
       } 
      } 
     } 
  
  
 /* --------------------------------------------------------------* 
 *      2) Modify the values and pass to Fortran                  * 
 *         - switch first dimension with the third                * 
 * --------------------------------------------------------------*/ 
  
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
         tmp_s1[k][j][i]  =              _[i][j][k];   
  
       } 
      } 
     } 
  
  
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
          _[k][j][i]  =     tmp_s1[k][j][i]; 
  
       } 
      } 
     } 
  
  
  
 /* --------------------------------------------------------------* 
 *       3) Verify values before returning to Fortran             * 
 * --------------------------------------------------------------*/ 
  
   a=0;
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
         if ( _[k][j][i] !=  cmp_s1[i][j][k]             )    exit (40+a); 
  
         a=a+1; 
       } 
      } 
     } 
  
 } 
