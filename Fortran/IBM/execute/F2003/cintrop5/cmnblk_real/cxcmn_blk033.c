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

// Use union to avoid conversion of the literal into a binary number
      union {
        unsigned long long ix;
        double dx;
      } tiny;
      tiny.ix = 0x8010000000000000ULL;

        if ( precision_dbl( Bnd_Lbl15      , tiny.dx )  !=  1 ) exit (20);


/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

        Bnd_Lbl15   =  1.797693e+308;


/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/

        if ( precision_dbl( Bnd_Lbl15      ,  1.797693e+308 )  !=  1 ) exit (25);

}

