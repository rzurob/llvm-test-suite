   #include <inttypes.h>

 #include <stdio.h> 
 #include <stdlib.h> 
 #include <stddef.h> 
  
  
 int_least8_t     _______________________________________________________________________[5]; 
  
 void csub_int(){ 
  
 int i; 
  
 /* --------------------------------------------------------------* 
 *       1) Verify values from Fortran code                       * 
 * --------------------------------------------------------------*/ 
  
         for ( i = 0; i < 5; i++ ) {  
            if ( _______________________________________________________________________[i] != -128 )  exit (30); 
         }   
  
 /* --------------------------------------------------------------* 
 *      2) Modify the values and pass to Fortran                  * 
 * --------------------------------------------------------------*/ 
  
         for ( i = 0; i < 5; i++ ) {
             _______________________________________________________________________[i] =  127 ; 
         } 
  
  
  
 /* --------------------------------------------------------------* 
 *       3) Verify values before returning to Fortran             * 
 * --------------------------------------------------------------*/ 
  
         for ( i = 0; i < 5; i++ ) {  
            if ( _______________________________________________________________________[i] !=  127 )  exit (35); 
         } 
  
 } 