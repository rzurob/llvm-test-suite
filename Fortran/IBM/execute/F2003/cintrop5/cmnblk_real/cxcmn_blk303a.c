   #include <inttypes.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"


struct {
        float                           real_s4a[5];
        float                           real_s4b[5];
        float                           real_s4c[5];

        double                          real_s8a[5];
        double                          real_s8b[5];
        double                          real_s8c[5];

        float                           r_c_float_s4a[5];
        float                           r_c_float_s4b[5];
        float                           r_c_float_s4c[5];

        double                          r_c_double_s8a[5];
        double                          r_c_double_s8b[5];
        double                          r_c_double_s8c[5];

        long double                     r_c_long_double_s8a[5];
        long double                     r_c_long_double_s8b[5];
        long double                     r_c_long_double_s8c[5];
        long double                     r_c_long_double_s8d[5];

} blk_real;


int i;

void csub_real(){

/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/

        if ( precision_flt( blk_real.real_s4a[0]           ,  3.402823e+38f )  !=  1 ) exit (120);
        if ( precision_flt( blk_real.real_s4a[1]           , -1.175494e-38f )  !=  1 ) exit (121);
        if ( precision_flt( blk_real.real_s4a[2]           ,  0.0f 
    )  !=  1 ) exit (122);
        if ( precision_flt( blk_real.real_s4a[3]           ,  1.175494e-38f )  !=  1 ) exit (123);
        if ( precision_flt( blk_real.real_s4a[4]           , -3.402823e+38f )  !=  1 ) exit (124);

    
for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_real.real_s4b[i]        , -1.175494e-38f )  !=  1 ) exit (125+i);
        }

        if ( precision_flt( blk_real.real_s4c[0]           ,  3.402823e+38f )  !=  1 ) exit (130);
        if ( precision_flt( blk_real.real_s4c[1]           , -1.175494e-38f )  !=  1 ) exit (131);
        if ( precision_flt( blk_real.real_s4c[2]           ,  0.0f          )  !=  1 ) exit (132);
        if ( precision_flt( blk_real.real_s4c[3]           ,  1.175494e-38f )  !=  1 ) exit (133);
        if ( precision_flt( blk_real.real_s4c[4]           , -3.402823e+38f )  !=  1 ) exit (134);

        if ( precision_dbl( blk_real.real_s8a[0]           ,  1.797693e+308 )  !=  1 ) exit (135);
        if ( precision_dbl( blk_real.real_s8a[1]           , -2.225073e-308 )  !=  1 ) exit (136);
        if ( precision_dbl( blk_real.real_s8a[2]           ,  0.0e0         )  !=  1 ) exit (137);
        if ( precision_dbl( blk_real.real_s8a[3]           ,  2.225073e-308 )  !=  1 ) exit (138);
        if ( precision_dbl( blk_real.real_s8a[4]           , -1.797693e+308 )  !=  1 ) exit (139);

        if ( precision_dbl( blk_real.real_s8b[0]           ,  1.797693e+308 )  !=  1 ) exit (140);
        if ( precision_dbl( blk_real.real_s8b[1]           , -2.225073e-308 )  !=  1 ) exit (141);
        if ( precision_dbl( blk_real.real_s8b[2]           ,  0.0e0         )  !=  1 ) exit (142);
        if ( precision_dbl( blk_real.real_s8b[3]           ,  2.225073e-308 )  !=  1 ) exit (143);
        if ( precision_dbl( blk_real.real_s8b[4]           , -1.797693e+308 )  !=  1 ) exit (144);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_real.real_s8c[i]        , -1.797693e+308 )  !=  1 ) exit (145+i);
        }

        if ( precision_flt( blk_real.r_c_float_s4a[0]      ,  3.402823e+38f  ) !=  1 ) exit (150);
        if ( precision_flt( blk_real.r_c_float_s4a[1]      , -1.175494e-38f  ) !=  1 ) exit (151);
        if ( precision_flt( blk_real.r_c_float_s4a[2]      ,  0.0f  
     ) !=  1 ) exit (152);
        if ( precision_flt( blk_real.r_c_float_s4a[3]      ,  1.175494e-38f  ) !=  1 ) exit (153);
        if ( precision_flt( blk_real.r_c_float_s4a[4]      , -3.402823e+38f  ) !=  1 ) exit (154);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_real.r_c_float_s4b[i]   ,  1.175494e-38f  ) !=  1 ) exit (155+i);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_real.r_c_float_s4c[i]   , -3.402823e+38f  ) !=  1 ) exit (160+i);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_real.r_c_double_s8a[i]   , 1.797693e+308  ) !=  1 ) exit (165+i);
        }

        if ( precision_dbl( blk_real.r_c_double_s8b[0]     ,  1.797693e+308  ) !=  1 ) exit (170);
        if ( precision_dbl( blk_real.r_c_double_s8b[1]     , -2.225073e-308  ) !=  1 ) exit (171);
        if ( precision_dbl( blk_real.r_c_double_s8b[2]     ,  0.0e0 
     ) !=  1 ) exit (172);
        if ( precision_dbl( blk_real.r_c_double_s8b[3]     ,  2.225073e-308  ) !=  1 ) exit (173);
        if ( precision_dbl( blk_real.r_c_double_s8b[4]     , -1.797693e+308  ) !=  1 ) exit (174);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_real.r_c_double_s8c[i]  , -1.797693e+308  ) !=  1 ) exit (175+i);
        }

        if ( precision_ldbl( blk_real.r_c_long_double_s8a[0]           ,  1.797693e+308L )  !=  1 ) exit (180);
        if ( precision_ldbl( blk_real.r_c_long_double_s8a[1]           , -2.004168e-292L )  !=  1 ) exit (181);
        if ( precision_ldbl( blk_real.r_c_long_double_s8a[2]           ,  0.0e0L         )  !=  1 ) exit (182);
        if ( precision_ldbl( blk_real.r_c_long_double_s8a[3]           ,  2.004168e-292L )  !=  1 ) exit (183);
        if ( precision_ldbl( blk_real.r_c_long_double_s8a[4]           , -1.797693e+308L )  !=  1 ) exit (184);

        if ( precision_ldbl( blk_real.r_c_long_double_s8b[0]           ,  1.797693e+308L )  !=  1 ) exit (185);
        if ( precision_ldbl( blk_real.r_c_long_double_s8b[1]           , -2.004168e-292L )  !=  1 ) exit (186);
        if ( precision_ldbl( blk_real.r_c_long_double_s8b[2]           ,  0.0e0L         )  !=  1 ) exit (187);
        if ( precision_ldbl( blk_real.r_c_long_double_s8b[3]           ,  2.004168e-292L )  !=  1 ) exit (188);
        if ( precision_ldbl( blk_real.r_c_long_double_s8b[4]           , -1.797693e+308L )  !=  1 ) exit (189);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_ldbl( blk_real.r_c_long_double_s8c[i]        , -1.797693e+308L )  !=  1 ) exit (190+i);
        }



/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

        blk_real.real_s4a[4]           =  3.402823e+38f;
        blk_real.real_s4a[3]           = -1.175494e-38f;
        blk_real.real_s4a[2]           =  0.0f         ;
        blk_real.real_s4a[1]           =  1.175494e-38f;
        blk_real.real_s4a[0]           = -3.402823e+38f;

        for ( i = 0; i < 5; i++) {
           blk_real.real_s4b[i]        = 1.175494e-38f;
        }

        blk_real.real_s4c[4]           =  3.402823e+38f;
        blk_real.real_s4c[3]           = -1.175494e-38f;
        blk_real.real_s4c[2]           =  0.0f         ;
        blk_real.real_s4c[1]           =  1.175494e-38f;
        blk_real.real_s4c[0]           = -3.402823e+38f;

        blk_real.real_s8a[4]           =  1.797693e+308;
        blk_real.real_s8a[3]           = -2.225073e-308;
        blk_real.real_s8a[2]           =  0.0e0        ;
        blk_real.real_s8a[1]           =  2.225073e-308;
        blk_real.real_s8a[0]           = -1.797693e+308;

        blk_real.real_s8b[4]           =  1.797693e+308;
        blk_real.real_s8b[3]           = -2.225073e-308; 
        blk_real.real_s8b[2]           =  0.0e0        ;
        blk_real.real_s8b[1]           =  2.225073e-308;
        blk_real.real_s8b[0]           = -1.797693e+308;

        for ( i = 0; i < 5; i++) {
           blk_real.real_s8c[i]        = 1.797693e+308; 
        }

        blk_real.r_c_float_s4a[4]      =  3.402823e+38f ; 
        blk_real.r_c_float_s4a[3]      = -1.175494e-38f ; 
        blk_real.r_c_float_s4a[2]      =  0.0f          ; 
        blk_real.r_c_float_s4a[1]      =  1.175494e-38f ; 
        blk_real.r_c_float_s4a[0]      = -3.402823e+38f ; 

        for ( i = 0; i < 5; i++) {
           blk_real.r_c_float_s4b[i]   =  -1.175494e-38f;
        }

        for ( i = 0; i < 5; i++) {
           blk_real.r_c_float_s4c[i]   = 3.402823e+38f ; 
        }

        for ( i = 0; i < 5; i++) {
           blk_real.r_c_double_s8a[i]   = -1.797693e+308 ;
        }

        blk_real.r_c_double_s8b[4]     =  1.797693e+308 ; 
        blk_real.r_c_double_s8b[3]     = -2.225073e-308 ;
        blk_real.r_c_double_s8b[2]     =  0.0e0         ; 
        blk_real.r_c_double_s8b[1]     =  2.225073e-308 ; 
        blk_real.r_c_double_s8b[0]     = -1.797693e+308 ;

        for ( i = 0; i < 5; i++) {
           blk_real.r_c_double_s8c[i]  = 1.797693e+308 ; 
        }

        blk_real.r_c_long_double_s8a[4]           =  1.797693e+308L;
        blk_real.r_c_long_double_s8a[3]           = -2.004168e-292L;
        blk_real.r_c_long_double_s8a[2]           =  0.0e0L        ;
        blk_real.r_c_long_double_s8a[1]           =  2.004168e-292L;
        blk_real.r_c_long_double_s8a[0]           = -1.797693e+308L;

        blk_real.r_c_long_double_s8b[4]           =  1.797693e+308L;
        blk_real.r_c_long_double_s8b[3]           = -2.004168e-292L;
        blk_real.r_c_long_double_s8b[2]           =  0.0e0L        ;
        blk_real.r_c_long_double_s8b[1]           =  2.004168e-292L;
        blk_real.r_c_long_double_s8b[0]           = -1.797693e+308L;

        for ( i = 0; i < 5; i++) {
           blk_real.r_c_long_double_s8c[i]        = 1.797693e+308L;
        }


/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/
        if ( precision_flt( blk_real.real_s4a[4]           ,  3.402823e+38f )  !=  1 ) exit (200);
        if ( precision_flt( blk_real.real_s4a[3]           , -1.175494e-38f )  !=  1 ) exit (200);
        if ( precision_flt( blk_real.real_s4a[2]           ,  0.0f          )  !=  1 ) exit (200);
        if ( precision_flt( blk_real.real_s4a[1]           ,  1.175494e-38f )  !=  1 ) exit (200);
        if ( precision_flt( blk_real.real_s4a[0]           , -3.402823e+38f )  !=  1 ) exit (200);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_real.real_s4b[i]        , 1.175494e-38f )  !=  1 ) exit (201);
        }

        if ( precision_flt( blk_real.real_s4c[4]           ,  3.402823e+38f )  !=  1 ) exit (202);
        if ( precision_flt( blk_real.real_s4c[3]           , -1.175494e-38f )  !=  1 ) exit (202);
        if ( precision_flt( blk_real.real_s4c[2]           ,  0.0f          )  !=  1 ) exit (202);
        if ( precision_flt( blk_real.real_s4c[1]           ,  1.175494e-38f )  !=  1 ) exit (202);
        if ( precision_flt( blk_real.real_s4c[0]           , -3.402823e+38f )  !=  1 ) exit (202);

        if ( precision_dbl( blk_real.real_s8a[4]           ,  1.797693e+308 )  !=  1 ) exit (203);
        if ( precision_dbl( blk_real.real_s8a[3]           , -2.225073e-308 )  !=  1 ) exit (203);
        if ( precision_dbl( blk_real.real_s8a[2]           ,  0.0e0         )  !=  1 ) exit (203);
        if ( precision_dbl( blk_real.real_s8a[1]           ,  2.225073e-308 )  !=  1 ) exit (203);
        if ( precision_dbl( blk_real.real_s8a[0]           , -1.797693e+308 )  !=  1 ) exit (203);

        if ( precision_dbl( blk_real.real_s8b[4]           ,  1.797693e+308 )  !=  1 ) exit (204);
        if ( precision_dbl( blk_real.real_s8b[3]           , -2.225073e-308 )  !=  1 ) exit (204);
        if ( precision_dbl( blk_real.real_s8b[2]           ,  0.0e0         )  !=  1 ) exit (204);
        if ( precision_dbl( blk_real.real_s8b[1]           ,  2.225073e-308 )  !=  1 ) exit (204);
        if ( precision_dbl( blk_real.real_s8b[0]           , -1.797693e+308 )  !=  1 ) exit (204);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_real.real_s8c[i]        , 1.797693e+308 )  !=  1 ) exit (205);
        }

        if ( precision_flt( blk_real.r_c_float_s4a[4]      ,  3.402823e+38f  ) !=  1 ) exit (206);
        if ( precision_flt( blk_real.r_c_float_s4a[3]      , -1.175494e-38f  ) !=  1 ) exit (206);
        if ( precision_flt( blk_real.r_c_float_s4a[2]      ,  0.0f           ) !=  1 ) exit (206);
        if ( precision_flt( blk_real.r_c_float_s4a[1]      ,  1.175494e-38f  ) !=  1 ) exit (206);
        if ( precision_flt( blk_real.r_c_float_s4a[0]      , -3.402823e+38f  ) !=  1 ) exit (206);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_real.r_c_float_s4b[i]   , -1.175494e-38f  ) !=  1 ) exit (207);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( precision_flt( blk_real.r_c_float_s4c[i]   ,  3.402823e+38f  ) !=  1 ) exit (208);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_real.r_c_double_s8a[i]   ,-1.797693e+308  ) !=  1 ) exit (209);
        }

        if ( precision_dbl( blk_real.r_c_double_s8b[4]     ,  1.797693e+308  ) !=  1 ) exit (210);
        if ( precision_dbl( blk_real.r_c_double_s8b[3]     , -2.225073e-308  ) !=  1 ) exit (210);
        if ( precision_dbl( blk_real.r_c_double_s8b[2]     ,  0.0e0          ) !=  1 ) exit (210);
        if ( precision_dbl( blk_real.r_c_double_s8b[1]     ,  2.225073e-308  ) !=  1 ) exit (210);
        if ( precision_dbl( blk_real.r_c_double_s8b[0]     , -1.797693e+308  ) !=  1 ) exit (210);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_dbl( blk_real.r_c_double_s8c[i]  , 1.797693e+308  ) !=  1 ) exit (211);
        }

        if ( precision_ldbl( blk_real.r_c_long_double_s8a[4]           ,  1.797693e+308L )  !=  1 ) exit (212);
        if ( precision_ldbl( blk_real.r_c_long_double_s8a[3]           , -2.004168e-292L )  !=  1 ) exit (212);
        if ( precision_ldbl( blk_real.r_c_long_double_s8a[2]           ,  0.0e0L         )  !=  1 ) exit (212);
        if ( precision_ldbl( blk_real.r_c_long_double_s8a[1]           ,  2.004168e-292L )  !=  1 ) exit (212);
        if ( precision_ldbl( blk_real.r_c_long_double_s8a[0]           , -1.797693e+308L )  !=  1 ) exit (212);

        if ( precision_ldbl( blk_real.r_c_long_double_s8b[4]           ,  1.797693e+308L )  !=  1 ) exit (213);
        if ( precision_ldbl( blk_real.r_c_long_double_s8b[3]           , -2.004168e-292L )  !=  1 ) exit (213);
        if ( precision_ldbl( blk_real.r_c_long_double_s8b[2]           ,  0.0e0L         )  !=  1 ) exit (213);
        if ( precision_ldbl( blk_real.r_c_long_double_s8b[1]           ,  2.004168e-292L )  !=  1 ) exit (213);
        if ( precision_ldbl( blk_real.r_c_long_double_s8b[0]           , -1.797693e+308L )  !=  1 ) exit (213);

        for ( i = 0; i < 5; i++ ) {
           if ( precision_ldbl( blk_real.r_c_long_double_s8c[i]        , 1.797693e+308L )  !=  1 ) exit (214);
        }
}
