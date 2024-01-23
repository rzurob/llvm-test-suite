   #include <inttypes.h>

/* --------------------------------------------------------------* 
*        This C code is used by fxcmn_blk109.f                  * 
*        Please refer to fxcmn_blk109.f for further details.    * 
* --------------------------------------------------------------*/ 
  
  
#include <stdio.h> 
#include <stdlib.h> 
#include <stddef.h> 
/* #include <inttypes.h>  */
  
  
long long               _[2][2][2]; 
  
void csub_int(){ 
  
int i,j,k,a; 
  
// Temporary matrix used in modifying values passed from Fortran
         long long               tmp_c_long_long[2][2][2]; 
  
// Comparison matrix 
         long long     cmp_c_long_long[2][2][2] =  {-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL }; 
  
  
 /* --------------------------------------------------------------* 
 *       1) Verify values from Fortran code                       * 
 * --------------------------------------------------------------*/ 
  
   a=0; 
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
         if ( _[i][j][k] !=  cmp_c_long_long[i][j][k]    )    exit (30+a); 
  
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
  
         tmp_c_long_long[k][j][i]  =     _[i][j][k];  
  
       } 
      } 
     } 
  
  
   for ( i = 0; i < 2; i++ ) { 
     for ( j = 0; j < 2; j++ ) { 
       for ( k = 0; k < 2; k++ ) { 
  
          _[k][j][i]  =     tmp_c_long_long[k][j][i]; 
  
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
  
         if ( _[k][j][i] !=  cmp_c_long_long[i][j][k]    )    exit (40+a); 
  
         a=a+1; 
       } 
      } 
     } 
  
 } 
