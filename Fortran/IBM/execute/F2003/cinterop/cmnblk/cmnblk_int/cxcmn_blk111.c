   #include <inttypes.h>

/* --------------------------------------------------------------* 
*        This C code is used by fxcmn_blk111.f                  * 
*        Please refer to fxcmn_blk111.f for further details.    * 
* --------------------------------------------------------------*/ 
  
  
#include <stdio.h> 
#include <stdlib.h> 
#include <stddef.h> 
/* #include <inttypes.h>  */
  
  
intptr_t                _[2][2][2]; 
  
void csub_int(){ 
  
int i,j,k,a; 
  
// Temporary matrix used in modifying values passed from Fortran
         intptr_t                tmp_c_intptr_t[2][2][2]; 
  
// Comparison matrix 
         intptr_t      cmp_c_intptr_t[2][2][2] =  {1000000000,1000000000,1000000000,1000000000,1000000000,1000000000,1000000000,1000000000 }; 
  
  
 /* --------------------------------------------------------------* 
 *       1) Verify values from Fortran code                       * 
 * --------------------------------------------------------------*/ 
  
   a=0; 
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
         if ( _[i][j][k] !=  cmp_c_intptr_t[i][j][k]     )    exit (30+a); 
  
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
  
         tmp_c_intptr_t[k][j][i]  =      _[i][j][k];       
  
       } 
      } 
     } 
  
  
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
          _[k][j][i]  =     tmp_c_intptr_t[k][j][i]; 
  
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
  
         if ( _[k][j][i] !=  cmp_c_intptr_t[i][j][k]     )    exit (40+a); 
  
         a=a+1; 
       } 
      } 
     } 
  
 } 
