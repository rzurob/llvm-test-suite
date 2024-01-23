#include <inttypes.h>
#include <stddef.h>

#include <stdio.h>
#include <stdlib.h>

#if (defined(_AIX) && ! defined(_AIX52))
  #define int_fast16_t short
#endif


int_fast16_t  		Bnd_Lbl15;


void csub1(){


/* --------------------------------------------------------------*
*       1) Verify values from Fortran code                       *
* --------------------------------------------------------------*/
  if ( Bnd_Lbl15  !=  64 )   		exit (25);


/* --------------------------------------------------------------*
*      2) Modify the values and pass to Fortran                  *
* --------------------------------------------------------------*/

  Bnd_Lbl15 = 32767;


/* --------------------------------------------------------------*
*       3) Verify values before returning to Fortran             *
* --------------------------------------------------------------*/
  if ( Bnd_Lbl15  !=  32767 )   	exit (30);

}

