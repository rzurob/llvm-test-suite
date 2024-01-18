   #include <inttypes.h>

/* --------------------------------------------------------------* 
*        This C code is used by fxcmn_blk127.f                  * 
*        Please refer to fxcmn_blk127.f for further details.    * 
* --------------------------------------------------------------*/ 
  
  
#include <stdio.h> 
#include <stdlib.h> 
#include <stddef.h> 
#include "precision_r.inc"
  
  
         long double              _1[2][2][2];  
  
 void csub_real(){ 
  
 int i,j,k,a; 
  
 /* --------------------------------------------------------------* 
 *       1) Verify values from Fortran code                       * 
 * --------------------------------------------------------------*/ 
  
  // Temporary matrix used in modifying values passed from Fortran
         long double                     tmp_real_s16[2][2][2]; 
  
  // Comparison matrix 
         long double  cmp_real_s16[2][2][2]      = {1.797693e+308l, -2.225073e-308l, 0.0e0l, 2.225073e-308l, -1.797693e+308l, 1.0l, -1.0l, -2.9l}; 
  
  
 /* --------------------------------------------------------------* 
 *       1) Verify values from Fortran code                       * 
 * --------------------------------------------------------------*/ 

   a=0; 
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
           if ( precision_dbl( _1[k][j][i]         ,   cmp_real_s16[k][j][i]        )  !=  1 ) exit (30+a); 
  
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
  
           tmp_real_s16[k][j][i]            = _1[i][j][k]; 
  
       } 
      } 
     } 
  
  
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
           _1[k][j][i]           =       tmp_real_s16[k][j][i]; 
  
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
  
           if ( precision_ldbl( _1[k][j][i]       ,   cmp_real_s16[i][j][k]        )  !=  1 ) exit (40+i); 
  
         a=a+1; 
       } 
      } 
     } 
  
 } 
