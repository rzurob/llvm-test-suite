#include <inttypes.h>
#include <stdio.h> 
#include <stdlib.h> 
#include <stddef.h> 
  
#if (defined(_AIX) && ! defined(_AIX52))
  #define int_fast16_t short
#endif

  
 int_fast16_t     _______________________________________________________________________[5]; 
  
 void csub_int(){ 
  
 int i; 
  
 /* --------------------------------------------------------------* 
 *       1) Verify values from Fortran code                       * 
 * --------------------------------------------------------------*/ 
  
         for ( i = 0; i < 5; i++ ) {  
            if ( _______________________________________________________________________[i]   != 64 )  exit (30); 
         }   
  
 /* --------------------------------------------------------------* 
 *      2) Modify the values and pass to Fortran                  * 
 * --------------------------------------------------------------*/ 
  
         for ( i = 0; i < 5; i++ ) {
             _______________________________________________________________________[i]   = 32767; 
         } 
  
  
  
 /* --------------------------------------------------------------* 
 *       3) Verify values before returning to Fortran             * 
 * --------------------------------------------------------------*/ 
  
         for ( i = 0; i < 5; i++ ) {  
            if ( _______________________________________________________________________[i]   != 32767 )  exit (35); 
         } 
    
 } 