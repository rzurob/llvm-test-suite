   #include <inttypes.h>
   #include <stddef.h>

#include <stdio.h>
#include <stdlib.h>


size_t  
Bnd_Lbl15;


void csub1(){

// size_t is similar to unsigned long long with -q64; so to compare a negative 
// value, need to type-cast it to a long long.

  long long  tmp_Bnd_Lbl15;

  tmp_Bnd_Lbl15 = Bnd_Lbl15;


/* --------------------------------------------------------------*
*       1) Verify values from Fortran code                       *
* --------------------------------------------------------------*/
  if ( tmp_Bnd_Lbl15  !=  -9223372036854775808LL    )   exit (20);


/* --------------------------------------------------------------*
*      2) Modify the values and pass to Fortran                  *
* --------------------------------------------------------------*/
  Bnd_Lbl15 = 9223372036854775807LL;


/* --------------------------------------------------------------*
*       3) Verify values before returning to Fortran             *
* --------------------------------------------------------------*/
  if ( Bnd_Lbl15  !=  9223372036854775807LL     )   exit (25);

}

