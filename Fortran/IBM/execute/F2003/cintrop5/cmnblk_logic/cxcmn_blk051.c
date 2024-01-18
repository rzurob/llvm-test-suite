#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


_Bool   		Bnd_Lbl15;


void csub1(){

/* --------------------------------------------------------------*
*       1) Verify values from Fortran code                       *
* --------------------------------------------------------------*/
  if ( Bnd_Lbl15  	!=  1	)   	exit (20);


/* --------------------------------------------------------------*
*      2) Modify the values and pass to Fortran                  *
* --------------------------------------------------------------*/
  Bnd_Lbl15 = 0;


/* --------------------------------------------------------------*
*       3) Verify values before returning to Fortran             *
* --------------------------------------------------------------*/
  if ( Bnd_Lbl15  	!=  0	)   	exit (25);

}
