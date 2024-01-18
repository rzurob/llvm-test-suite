   #include <inttypes.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"


float                           blk_real_s4a[5];
float                           blk_real_s4b[5];
float                           blk_real_s4c[5];

double                          blk_real_s8a[5];
double                          blk_real_s8b[5];
double                          blk_real_s8c[5];

long double                     blk_real_s16a[5];
long double                     blk_real_s16b[5];
long double                     blk_real_s16c[5];

float                           blk_r_c_float_s4a[5];
float                           blk_r_c_float_s4b[5];
float                           blk_r_c_float_s4c[5];

double                          blk_r_c_double_s8a[5];
double                          blk_r_c_double_s8b[5];
double                          blk_r_c_double_s8c[5];

int i;


void csub_real(){

/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/

        if ( precision_flt( blk_real_s4a[0]           ,  3.402823e+38f )  !=  1 ) exit (120);
        if ( precision_flt( blk_real_s4a[1]           , -1.175494e-38f )  !=  1 ) exit (121);
        if ( precision_flt( blk_real_s4a[2]           ,  0.0f 
    )  
  !=  1 ) exit (122);
        if ( precision_flt( blk_real_s4a[3]           ,  1.175494e-38f )  !=  1 ) exit (123);
        if ( precision_flt( blk_real_s4a[4]           , -3.402823e+38f )  !=  1 ) exit (124);

    
for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_real_s4b[i]        , -1.175494e-38f )  !=  1 ) exit (125+i);
        }

        if ( precision_flt( blk_real_s4c[0]           ,  3.402823e+38f )  !=  1 ) exit (130);
        if ( precision_flt( blk_real_s4c[1]           , -1.175494e-38f )  !=  1 ) exit (131);
        if ( precision_flt( blk_real_s4c[2]           ,  0.0f          )  !=  1 ) exit (132);
        if ( precision_flt( blk_real_s4c[3]           ,  1.175494e-38f )  !=  1 ) exit (133);
        if ( precision_flt( blk_real_s4c[4]           , -3.402823e+38f )  !=  1 ) exit (134);

        if ( precision_dbl( blk_real_s8a[0]           ,  1.797693e+308 )  !=  1 ) exit (135);
        if ( precision_dbl( blk_real_s8a[1]           , -2.225073e-308 )  !=  1 ) exit (136);
        if ( precision_dbl( blk_real_s8a[2]           ,  0.0e0         )  !=  1 ) exit (137);
        if ( precision_dbl( blk_real_s8a[3]           ,  2.225073e-308 )  !=  1 ) exit (138);
        if ( precision_dbl( blk_real_s8a[4]           , -1.797693e+308 )  !=  1 ) exit (139);

        if ( precision_dbl( blk_real_s8b[0]           ,  1.797693e+308 )  !=  1 ) exit (140);
        if ( precision_dbl( blk_real_s8b[1]           , -2.225073e-308 )  !=  1 ) exit (141);
        if ( precision_dbl( blk_real_s8b[2]           ,  0.0e0         )  !=  1 ) exit (142);
        if ( precision_dbl( blk_real_s8b[3]           ,  2.225073e-308 )  !=  1 ) exit (143);
        if ( precision_dbl( blk_real_s8b[4]           , -1.797693e+308 )  !=  1 ) exit (144);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_real_s8c[i]        , -1.797693e+308 )  !=  1 ) exit (145+i);
        }

        if ( precision_ldbl( blk_real_s16a[0]         ,  1.797693e+308l ) !=  1 ) exit (150);
        if ( precision_ldbl( blk_real_s16a[1]         , -2.225073e-308l ) !=  1 ) exit (151);
        if ( precision_ldbl( blk_real_s16a[2]         ,  0.0e0l         ) !=  1 ) exit (152);
        if ( precision_ldbl( blk_real_s16a[3]         ,  2.225073e-308l ) !=  1 ) exit (153);
        if ( precision_ldbl( blk_real_s16a[4]         , -1.797693e+308l ) !=  1 ) exit (154);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_ldbl( blk_real_s16b[i]      , -2.225073e-308l ) !=  1 ) exit (155+i);
        }

        if ( precision_ldbl( blk_real_s16c[0]         ,  1.797693e+308l ) !=  1 ) exit (160);
        if ( precision_ldbl( blk_real_s16c[1]         , -2.225073e-308l ) !=  1 ) exit (161);
        if ( precision_ldbl( blk_real_s16c[2]         ,  0.0e0l         ) !=  1 ) exit (162);
        if ( precision_ldbl( blk_real_s16c[3]         ,  2.225073e-308l ) !=  1 ) exit (163);
        if ( precision_ldbl( blk_real_s16c[4]         , -1.797693e+308l ) !=  1 ) exit (164);

        if ( precision_flt( blk_r_c_float_s4a[0]      ,  3.402823e+38f  ) !=  1 ) exit (165);
        if ( precision_flt( blk_r_c_float_s4a[1]      , -1.175494e-38f  ) !=  1 ) exit (166);
        if ( precision_flt( blk_r_c_float_s4a[2]      ,  0.0f  
        ) !=  1 ) exit (167);
        if ( precision_flt( blk_r_c_float_s4a[3]      ,  1.175494e-38f  ) !=  1 ) exit (168);
        if ( precision_flt( blk_r_c_float_s4a[4]      , -3.402823e+38f  ) !=  1 ) exit (169);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_r_c_float_s4b[i]   ,  1.175494e-38f  ) !=  1 ) exit (170+i);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_r_c_float_s4c[i]   , -3.402823e+38f  ) !=  1 ) exit (175+i);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_r_c_double_s8a[i]   , 1.797693e+308  ) !=  1 ) exit (180+i);
        }

        if ( precision_dbl( blk_r_c_double_s8b[0]     ,  1.797693e+308  ) !=  1 ) exit (185);
        if ( precision_dbl( blk_r_c_double_s8b[1]     , -2.225073e-308  ) !=  1 ) exit (186);
        if ( precision_dbl( blk_r_c_double_s8b[2]     ,  0.0e0 
        ) !=  1 ) exit (187);
        if ( precision_dbl( blk_r_c_double_s8b[3]     ,  2.225073e-308  ) !=  1 ) exit (188);
        if ( precision_dbl( blk_r_c_double_s8b[4]     , -1.797693e+308  ) !=  1 ) exit (189);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_r_c_double_s8c[i]  , -1.797693e+308  ) !=  1 ) exit (190+i);
        }



/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

        blk_real_s4a[4]           =  3.402823e+38f;
        blk_real_s4a[3]           = -1.175494e-38f;
        blk_real_s4a[2]           =  0.0f         ;
        blk_real_s4a[1]           =  1.175494e-38f;
        blk_real_s4a[0]           = -3.402823e+38f;

        for ( i = 0; i < 5; i++) {
           blk_real_s4b[i]        = 1.175494e-38f;
        }

        blk_real_s4c[4]           =  3.402823e+38f;
        blk_real_s4c[3]           = -1.175494e-38f;
        blk_real_s4c[2]           =  0.0f         ;
        blk_real_s4c[1]           =  1.175494e-38f;
        blk_real_s4c[0]           = -3.402823e+38f;

        blk_real_s8a[4]           =  1.797693e+308;
        blk_real_s8a[3]           = -2.225073e-308;
        blk_real_s8a[2]           =  0.0e0        ;
        blk_real_s8a[1]           =  2.225073e-308;
        blk_real_s8a[0]           = -1.797693e+308;

        blk_real_s8b[4]           =  1.797693e+308;
        blk_real_s8b[3]           = -2.225073e-308;
        blk_real_s8b[2]           =  0.0e0        ;
        blk_real_s8b[1]           =  2.225073e-308;
        blk_real_s8b[0]           = -1.797693e+308;

        for ( i = 0; i < 5; i++) {
           blk_real_s8c[i]        = 1.797693e+308;
        }

        blk_real_s16a[4]         =  1.797693e+308l;
        blk_real_s16a[3]         = -2.225073e-308l;
        blk_real_s16a[2]         =  0.0e0l        ;
        blk_real_s16a[1]         =  2.225073e-308l;
        blk_real_s16a[0]         = -1.797693e+308l;

        for ( i = 0; i < 5; i++ ) {
           blk_real_s16b[i]      =  2.225073e-308l;
        }

        blk_real_s16c[4]         =  1.797693e+308l;
        blk_real_s16c[3]         = -2.225073e-308l;
        blk_real_s16c[2]         =  0.0e0l        ;
        blk_real_s16c[1]         =  2.225073e-308l;
        blk_real_s16c[0]         = -1.797693e+308l;

        blk_r_c_float_s4a[4]      =  3.402823e+38f ;
        blk_r_c_float_s4a[3]      = -1.175494e-38f ;
        blk_r_c_float_s4a[2]      =  0.0f          ;
        blk_r_c_float_s4a[1]      =  1.175494e-38f ;
        blk_r_c_float_s4a[0]      = -3.402823e+38f ;

        for ( i = 0; i < 5; i++) {
           blk_r_c_float_s4b[i]   =  -1.175494e-38f ;
        }

        for ( i = 0; i < 5; i++) {
           blk_r_c_float_s4c[i]   = 3.402823e+38f ;
        }

        for ( i = 0; i < 5; i++) {
           blk_r_c_double_s8a[i]   = -1.797693e+308 ;
        }

        blk_r_c_double_s8b[4]     =  1.797693e+308 ;
        blk_r_c_double_s8b[3]     = -2.225073e-308 ;
        blk_r_c_double_s8b[2]     =  0.0e0         ;
        blk_r_c_double_s8b[1]     =  2.225073e-308 ;
        blk_r_c_double_s8b[0]     = -1.797693e+308 ;

        for ( i = 0; i < 5; i++) {
           blk_r_c_double_s8c[i]  = 1.797693e+308 ;
        }



/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/

        if ( precision_flt( blk_real_s4a[4]           ,  3.402823e+38f )  !=  1 ) exit (200);
        if ( precision_flt( blk_real_s4a[3]           , -1.175494e-38f )  !=  1 ) exit (200);
        if ( precision_flt( blk_real_s4a[2]           ,  0.0f          )  !=  1 ) exit (200);
        if ( precision_flt( blk_real_s4a[1]           ,  1.175494e-38f )  !=  1 ) exit (200);
        if ( precision_flt( blk_real_s4a[0]           , -3.402823e+38f )  !=  1 ) exit (200);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_real_s4b[i]        , 1.175494e-38f )  !=  1 ) exit (201);
        }

        if ( precision_flt( blk_real_s4c[4]           ,  3.402823e+38f )  !=  1 ) exit (202);
        if ( precision_flt( blk_real_s4c[3]           , -1.175494e-38f )  !=  1 ) exit (202);
        if ( precision_flt( blk_real_s4c[2]           ,  0.0f          )  !=  1 ) exit (202);
        if ( precision_flt( blk_real_s4c[1]           ,  1.175494e-38f )  !=  1 ) exit (202);
        if ( precision_flt( blk_real_s4c[0]           , -3.402823e+38f )  !=  1 ) exit (202);

        if ( precision_dbl( blk_real_s8a[4]           ,  1.797693e+308 )  !=  1 ) exit (203);
        if ( precision_dbl( blk_real_s8a[3]           , -2.225073e-308 )  !=  1 ) exit (203);
        if ( precision_dbl( blk_real_s8a[2]           ,  0.0e0         )  !=  1 ) exit (203);
        if ( precision_dbl( blk_real_s8a[1]           ,  2.225073e-308 )  !=  1 ) exit (203);
        if ( precision_dbl( blk_real_s8a[0]           , -1.797693e+308 )  !=  1 ) exit (203);

        if ( precision_dbl( blk_real_s8b[4]           ,  1.797693e+308 )  !=  1 ) exit (204);
        if ( precision_dbl( blk_real_s8b[3]           , -2.225073e-308 )  !=  1 ) exit (204);
        if ( precision_dbl( blk_real_s8b[2]           ,  0.0e0         )  !=  1 ) exit (204);
        if ( precision_dbl( blk_real_s8b[1]           ,  2.225073e-308 )  !=  1 ) exit (204);
        if ( precision_dbl( blk_real_s8b[0]           , -1.797693e+308 )  !=  1 ) exit (204);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_real_s8c[i]        , 1.797693e+308 )  !=  1 ) exit (205);
        }

        if ( precision_flt( blk_r_c_float_s4a[4]      ,  3.402823e+38f  ) !=  1 ) exit (206);
        if ( precision_flt( blk_r_c_float_s4a[3]      , -1.175494e-38f  ) !=  1 ) exit (206);
        if ( precision_flt( blk_r_c_float_s4a[2]      ,  0.0f           ) !=  1 ) exit (206);
        if ( precision_flt( blk_r_c_float_s4a[1]      ,  1.175494e-38f  ) !=  1 ) exit (206);
        if ( precision_flt( blk_r_c_float_s4a[0]      , -3.402823e+38f  ) !=  1 ) exit (206);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_r_c_float_s4b[i]   , -1.175494e-38f  ) !=  1 ) exit (207);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_r_c_float_s4c[i]   ,  3.402823e+38f  ) !=  1 ) exit (208);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_r_c_double_s8a[i]   ,-1.797693e+308  ) !=  1 ) exit (209);
        }

        if ( precision_dbl( blk_r_c_double_s8b[4]     ,  1.797693e+308  ) !=  1 ) exit (210);
        if ( precision_dbl( blk_r_c_double_s8b[3]     , -2.225073e-308  ) !=  1 ) exit (210);
        if ( precision_dbl( blk_r_c_double_s8b[2]     ,  0.0e0          ) !=  1 ) exit (210);
        if ( precision_dbl( blk_r_c_double_s8b[1]     ,  2.225073e-308  ) !=  1 ) exit (210);
        if ( precision_dbl( blk_r_c_double_s8b[0]     , -1.797693e+308  ) !=  1 ) exit (210);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_r_c_double_s8c[i]  , 1.797693e+308  ) !=  1 ) exit (211);
        }

        if ( precision_ldbl( blk_real_s16a[4]         ,  1.797693e+308l ) !=  1 ) exit (212);
        if ( precision_ldbl( blk_real_s16a[3]         , -2.225073e-308l ) !=  1 ) exit (212);
        if ( precision_ldbl( blk_real_s16a[2]         ,  0.0e0l         ) !=  1 ) exit (212);
        if ( precision_ldbl( blk_real_s16a[1]         ,  2.225073e-308l ) !=  1 ) exit (212);
        if ( precision_ldbl( blk_real_s16a[0]         , -1.797693e+308l ) !=  1 ) exit (212);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_ldbl( blk_real_s16b[i]      ,  2.225073e-308l ) !=  1 ) exit (213);
        }

        if ( precision_ldbl( blk_real_s16c[4]         ,  1.797693e+308l ) !=  1 ) exit (214);
        if ( precision_ldbl( blk_real_s16c[3]         , -2.225073e-308l ) !=  1 ) exit (214);
        if ( precision_ldbl( blk_real_s16c[2]         ,  0.0e0l         ) !=  1 ) exit (214);
        if ( precision_ldbl( blk_real_s16c[1]         ,  2.225073e-308l ) !=  1 ) exit (214);
        if ( precision_ldbl( blk_real_s16c[0]         , -1.797693e+308l ) !=  1 ) exit (214);


}

