   #include <inttypes.h>
   #include <stddef.h>

#include <stdio.h>
#include <stdlib.h>


int_fast8_t   
Bnd_Lbl15;


void csub1(){


/* --------------------------------------------------------------*
*       1) Verify values from Fortran code                       *
* --------------------------------------------------------------*/
  if ( Bnd_Lbl15  !=  -128 
)   exit (20);


/* --------------------------------------------------------------*
*      2) Modify the values and pass to Fortran                  *
* --------------------------------------------------------------*/
  Bnd_Lbl15 = 127;


/* --------------------------------------------------------------*
*       3) Verify values before returning to Fortran             *
* --------------------------------------------------------------*/
  if ( Bnd_Lbl15  !=  127 
)   exit (25);

}
