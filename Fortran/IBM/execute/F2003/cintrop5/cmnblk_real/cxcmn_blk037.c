   #include <inttypes.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"


long double Bnd_Lbl15;

void csub_real(){
  long double ldbl_u = 1.7976931348623157e+308; //DBL_MAX;
  long double ldbl_d = 2.2250738585072014e-308; //DBL_MIN;
/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/

  if ( precision_ldbl( Bnd_Lbl15, -2.0041683600089728e-292 )  !=  1 ) exit (20);

/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

  Bnd_Lbl15   =  ldbl_u;

/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/

  if ( precision_ldbl( Bnd_Lbl15, 1.7976931348623157e+308)  !=  1 ) exit (25);
}

