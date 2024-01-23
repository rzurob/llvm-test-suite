   #include <inttypes.h>

#include <stdio.h> 
#include <stdlib.h> 
#include <stddef.h> 
#include "precision_r.inc"
 /* #include <inttypes.h>  */
  
  
 float                           _L_a_b_e_l_4_U[5]; 
  
 void csub_real(){ 
  
 int i; 
  
 /* --------------------------------------------------------------* 
 *       1) Verify values from Fortran code                       * 
 * --------------------------------------------------------------*/ 
  
         if ( precision_flt( _L_a_b_e_l_4_U[0]           ,  3.402823e+38f )  !=  1 ) exit (30); 
         if ( precision_flt( _L_a_b_e_l_4_U[1]           , -1.175494e-38f )  !=  1 ) exit (31); 
         if ( precision_flt( _L_a_b_e_l_4_U[2]           ,  0.0f          )  !=  1 ) exit (32); 
         if ( precision_flt( _L_a_b_e_l_4_U[3]           ,  1.175494e-38f )  !=  1 ) exit (33); 
         if ( precision_flt( _L_a_b_e_l_4_U[4]           , -3.402823e+38f )  !=  1 ) exit (34); 
  
  
 /* --------------------------------------------------------------* 
 *      2) Modify the values and pass to Fortran                  * 
 * --------------------------------------------------------------*/ 
  
         _L_a_b_e_l_4_U[4]           =  3.402823e+38f; 
         _L_a_b_e_l_4_U[3]           = -1.175494e-38f; 
         _L_a_b_e_l_4_U[2]           =  0.0f         ; 
         _L_a_b_e_l_4_U[1]           =  1.175494e-38f; 
         _L_a_b_e_l_4_U[0]           = -3.402823e+38f; 
  
  
  
 /* --------------------------------------------------------------* 
 *       3) Verify values before returning to Fortran             * 
 * --------------------------------------------------------------*/ 
  
         if ( precision_flt( _L_a_b_e_l_4_U[4]           ,  3.402823e+38f )  !=  1 ) exit (40); 
         if ( precision_flt( _L_a_b_e_l_4_U[3]           , -1.175494e-38f )  !=  1 ) exit (41); 
         if ( precision_flt( _L_a_b_e_l_4_U[2]           ,  0.0f          )  !=  1 ) exit (42); 
         if ( precision_flt( _L_a_b_e_l_4_U[1]           ,  1.175494e-38f )  !=  1 ) exit (43); 
         if ( precision_flt( _L_a_b_e_l_4_U[0]           , -3.402823e+38f )  !=  1 ) exit (44); 
  
 } 
