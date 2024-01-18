   #include <inttypes.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"


float                           Bnd_Lbl15;

void csub_real(){

/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/

        if ( precision_flt( Bnd_Lbl15      , -3.402823e+38f )  !=  1 ) exit (20);


/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

        Bnd_Lbl15   =  1.175494e-38f;


/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/

        if ( precision_flt( Bnd_Lbl15      ,  1.175494e-38f )  !=  1 ) exit (25);

}

