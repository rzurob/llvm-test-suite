#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


_Bool   		Bnd_Lbl15[5];


void csub1(){

/* --------------------------------------------------------------*
*       1) Verify values from Fortran code                       *
* --------------------------------------------------------------*/
  if ( Bnd_Lbl15[0]  	!=  1	)   	exit (20);
  if ( Bnd_Lbl15[1]  	!=  0	)   	exit (21);
  if ( Bnd_Lbl15[2]  	!=  1	)   	exit (22);
  if ( Bnd_Lbl15[3]  	!=  0	)   	exit (23);
  if ( Bnd_Lbl15[4]  	!=  1	)   	exit (24);


/* --------------------------------------------------------------*
*      2) Modify the values and pass to Fortran                  *
* --------------------------------------------------------------*/
  Bnd_Lbl15[2] = 0;
  Bnd_Lbl15[3] = 1;


/* --------------------------------------------------------------*
*       3) Verify values before returning to Fortran             *
* --------------------------------------------------------------*/
  if ( Bnd_Lbl15[0]  	!=  1	)   	exit (25);
  if ( Bnd_Lbl15[1]  	!=  0	)   	exit (26);
  if ( Bnd_Lbl15[2]  	!=  0	)   	exit (27);
  if ( Bnd_Lbl15[3]  	!=  1	)   	exit (28);
  if ( Bnd_Lbl15[4]  	!=  1	)   	exit (29);

}
