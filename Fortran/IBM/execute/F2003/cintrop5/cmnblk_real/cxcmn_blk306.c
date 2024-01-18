   #include <inttypes.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"

/* ------------------------------------------------------------------------*
*   3-dimensional real arrays                                              *
*  ---------------------------------------------------------------------- */

struct {
        float                           real_s4[2][2][2];
        double                          real_s8[2][2][2];
        long double                     real_s16[2][2][2];
        float                           r_c_float_s4[2][2][2];
        double                          r_c_double_s8[2][2][2];

} blk_real;



void csub_real(){

/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/

int i, j, k,a;

// Temporary matrices used in modifying values passed from Fortran
        float                           tmp_real_s4[2][2][2];
        double                          tmp_real_s8[2][2][2];
        long double                     tmp_real_s16[2][2][2];
        float                           tmp_r_c_float_s4[2][2][2];
        double                          tmp_r_c_double_s8[2][2][2];

// Comparison matrices

        float        cmp_real_s4[2][2][2]       = {3.402823e+38f, -1.175494e-38f, 0.0f, 1.175494e-38f, -3.402823e+38f, 1.0f, -1.0f, -2.9f};
        double       cmp_real_s8[2][2][2]       = {-2.225073e-308,-2.225073e-308,-2.225073e-308,-2.225073e-308,-2.225073e-308,-2.225073e-308,-2.225073e-308,-2.225073e-308};
        long double  cmp_real_s16[2][2][2]      = {1.797693e+308l, -2.225073e-308l, 0.0e0l, 2.225073e-308l, -1.797693e+308l, 1.0l, -1.0l, -2.9l};
        float        cmp_r_c_float_s4[2][2][2]  = {3.402823e+38f, -1.175494e-38f, 0.0, 1.175494e-38f, -3.402823e+38f,1.0f, -1.0f, -2.9f};
        double       cmp_r_c_double_s8[2][2][2] = {1.797693e+308, -2.225073e-308, 0.0e0, 2.225073e-308, -1.797693e+308, 1.00000e+308, -1.00000e+308, -1.00000e+100};

a=0;
    for ( i = 0; i < 2; i++ ) {
      for ( j = 0; j < 2; j++ ) {
        for ( k = 0; k < 2; k++ ) {

          if ( precision_flt( blk_real.real_s4[k][j][i]        ,   cmp_real_s4[k][j][i] 
)  !=  1 ) exit (60+a);
          if ( precision_dbl( blk_real.real_s8[k][j][i]        ,   cmp_real_s8[k][j][i]
)  !=  1 ) exit (70+a);
          if ( precision_ldbl( blk_real.real_s16[k][j][i]      ,   cmp_real_s16[k][j][i]
)  !=  1 ) exit (80+a);
          if ( precision_flt( blk_real.r_c_float_s4[k][j][i]   ,   cmp_r_c_float_s4[k][j][i] 
)  !=  1 ) exit (90+a);
          if ( precision_dbl( blk_real.r_c_double_s8[k][j][i]  ,   cmp_r_c_double_s8[k][j][i]
)  !=  1 ) exit (100+a);

          a=a+1;
        }
      }
    }



/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

    for ( i = 0; i < 2; i++ ) {
      for ( j = 0; j < 2; j++ ) {
        for ( k = 0; k < 2; k++ ) {

          tmp_real_s4[k][j][i]             = blk_real.real_s4[i][j][k];
          tmp_real_s8[k][j][i]             = blk_real.real_s8[i][j][k];
          tmp_real_s16[k][j][i]            = blk_real.real_s16[i][j][k];
          tmp_r_c_float_s4[k][j][i]        = blk_real.r_c_float_s4[i][j][k];
          tmp_r_c_double_s8[k][j][i]       = blk_real.r_c_double_s8[i][j][k];

        }
      }
    }

    for ( i = 0; i < 2; i++ ) {
      for ( j = 0; j < 2; j++ ) {
        for ( k = 0; k < 2; k++ ) {

          blk_real.real_s4[k][j][i]
=
tmp_real_s4[k][j][i];
          blk_real.real_s8[k][j][i]
=
tmp_real_s8[k][j][i];
          blk_real.real_s16[k][j][i]
=
tmp_real_s16[k][j][i];
          blk_real.r_c_float_s4[k][j][i]
=
tmp_r_c_float_s4[k][j][i];
          blk_real.r_c_double_s8[k][j][i]
=
tmp_r_c_double_s8[k][j][i];

        }
      }
    }



/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/

    for ( i = 0; i < 2; i++ ) {
      for ( j = 0; j < 2; j++ ) {
        for ( k = 0; k < 2; k++ ) {

          if ( precision_flt( blk_real.real_s4[k][j][i]        ,   cmp_real_s4[i][j][k]         )  !=  1 ) exit (130);
          if ( precision_dbl( blk_real.real_s8[k][j][i]        ,   cmp_real_s8[i][j][k]         )  !=  1 ) exit (140);
          if ( precision_ldbl( blk_real.real_s16[k][j][i]      ,   cmp_real_s16[i][j][k]        )  !=  1 ) exit (150);
          if ( precision_flt( blk_real.r_c_float_s4[k][j][i]   ,   cmp_r_c_float_s4[i][j][k]    )  !=  1 ) exit (160);
          if ( precision_dbl( blk_real.r_c_double_s8[k][j][i]  ,   cmp_r_c_double_s8[i][j][k]   )  !=  1 ) exit (170);

        }
      }
    }


}

