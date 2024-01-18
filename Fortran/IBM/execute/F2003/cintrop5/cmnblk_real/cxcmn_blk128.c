   #include <inttypes.h>

/* --------------------------------------------------------------* 
*        This C code is used by fxcmn_blk128.f                  * 
*        Please refer to fxcmn_blk128.f for further details.    * 
* --------------------------------------------------------------*/ 
  
  
#include <stdio.h> 
#include <stdlib.h> 
#include <stddef.h> 
/* #include <inttypes.h>  */
#include "precision_r.inc"
  
  
         float                    _1[2][2][2]; 
  
 void csub_real(){ 
  
 int i,j,k,a; 
  
 /* --------------------------------------------------------------* 
 *       1) Verify values from Fortran code                       * 
 * --------------------------------------------------------------*/ 
  
  // Temporary matrix used in modifying values passed from Fortran
         float                           tmp_r_c_float_s4[2][2][2]; 
  
  // Comparison matrix 
         float        cmp_r_c_float_s4[2][2][2]  = {3.402823e+38f, -1.175494e-38f, 0.0, 1.175494e-38f, -3.402823e+38f,1.0f, -1.0f, -2.9f}; 
  
  
 /* --------------------------------------------------------------* 
 *       1) Verify values from Fortran code                       * 
 * --------------------------------------------------------------*/ 
  
   a=0; 
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
           if ( precision_flt( _1[k][j][i]          ,   cmp_r_c_float_s4[k][j][i]    )  !=  1 ) exit (30+a); 
  
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
  
           tmp_r_c_float_s4[k][j][i]        = _1[i][j][k];     
  
       } 
      } 
     } 
  
  
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
           _1[k][j][i]           =       tmp_r_c_float_s4[k][j][i]; 
  
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
  
           if ( precision_flt( _1[k][j][i]        ,   cmp_r_c_float_s4[i][j][k]    )  !=  1 ) exit (40+i); 
  
         a=a+1; 
       } 
      } 
     } 
  
 } 
