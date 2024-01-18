   #include <inttypes.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"


long double                           Bnd_Lbl15;

void csub_real(){

/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/

        if ( precision_ldbl( Bnd_Lbl15      , -2.225073e-308l )  !=  1 ) exit (20);


/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

        Bnd_Lbl15   =  1.797693e+308l;


/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/

        if ( precision_ldbl( Bnd_Lbl15      ,  1.797693e+308l )  !=  1 ) exit (25);

}

