   #include <inttypes.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"


double                           Bnd_Lbl15;

void csub_real(){

/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/

        if ( precision_dbl( Bnd_Lbl15      , -1.797693e+300 )  !=  1 ) exit (20);


/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

        Bnd_Lbl15   =  2.225073e-308;


/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/

        if ( precision_dbl( Bnd_Lbl15      ,  2.225073e-308 )  !=  1 ) exit (25);

}

