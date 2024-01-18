   #include <inttypes.h>

#include <stdio.h> 
#include <stdlib.h> 
#include <stddef.h> 
#include "precision_r.inc"
 /* #include <inttypes.h>  */
  
  
 double                          _L_a_b_e_l_4_U[5]; 
  
 void csub_real(){ 
  
 int i; 
  
 /* --------------------------------------------------------------* 
 *       1) Verify values from Fortran code                       * 
 * --------------------------------------------------------------*/ 
  
         for ( i = 0; i < 5; i++ ) { 
            if ( precision_dbl( _L_a_b_e_l_4_U[i]  , -1.797693e+308  ) !=  1 ) exit (30+i); 
         } 
  
  
 /* --------------------------------------------------------------* 
 *      2) Modify the values and pass to Fortran                  * 
 * --------------------------------------------------------------*/ 
  
         for ( i = 0; i < 5; i++) { 
            _L_a_b_e_l_4_U[i]  = 1.797693e+308 ; 
         } 
  
  
  
 /* --------------------------------------------------------------* 
 *       3) Verify values before returning to Fortran             * 
 * --------------------------------------------------------------*/ 
  
         for ( i = 0; i < 5; i++ ) { 
            if ( precision_dbl( _L_a_b_e_l_4_U[i]  , 1.797693e+308  ) !=  1 ) exit (40+i); 
         } 
  
 } 
