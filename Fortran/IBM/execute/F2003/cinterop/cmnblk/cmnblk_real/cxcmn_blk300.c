   #include <inttypes.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"


struct {
        float                           real_s4a;
        float                           real_s4b;
        float                           real_s4c;
        float                           real_s4d;

        double                          real_s8a;
        double                          real_s8b;
        double                          real_s8c;
        double                          real_s8d;

        long double                     real_s16a;
        long double                     real_s16b;
        long double                     real_s16c;
        long double                     real_s16d;

        float                           r_c_float_s4a;
        float                           r_c_float_s4b;
        float                           r_c_float_s4c;
        float                           r_c_float_s4d;

        double                          r_c_double_s8a;
        double                          r_c_double_s8b;
        double                          r_c_double_s8c;
        double                          r_c_double_s8d;

} blk_real;



void csub_real(){

/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/

        if ( precision_flt( blk_real.real_s4a              ,  3.402823e+38f )  !=  1 ) exit (120);
        if ( precision_flt( blk_real.real_s4b              ,  1.175494e-38f )  !=  1 ) exit (121);
        if ( precision_flt( blk_real.real_s4c              , -3.402823e+38f )  !=  1 ) exit (122);
        if ( precision_flt( blk_real.real_s4d              , -1.175494e-38f )  !=  1 ) exit (123);

        if ( precision_dbl( blk_real.real_s8a              ,  1.797693e+308 )  !=  1 ) exit (124);
        if ( precision_dbl( blk_real.real_s8b              ,  2.225073e-308 )  !=  1 ) exit (125);
        if ( precision_dbl( blk_real.real_s8c              , -1.797693e+308 )  !=  1 ) exit (126);
        if ( precision_dbl( blk_real.real_s8d              , -2.225073e-308 )  !=  1 ) exit (127);

        if ( precision_ldbl( blk_real.real_s16a            ,  1.797693e+308l )  !=  1 ) exit (128);
        if ( precision_ldbl( blk_real.real_s16b            ,  2.225073e-308l )  !=  1 ) exit (129);
        if ( precision_ldbl( blk_real.real_s16c            , -1.797693e+308l )  !=  1 ) exit (130);
        if ( precision_ldbl( blk_real.real_s16d            , -2.225073e-308l )  !=  1 ) exit (131);

        if ( precision_flt( blk_real.r_c_float_s4a         ,  3.402823e+38f )  !=  1 ) exit (132);
        if ( precision_flt( blk_real.r_c_float_s4b         ,  1.175494e-38f )  !=  1 ) exit (133);
        if ( precision_flt( blk_real.r_c_float_s4c         , -3.402823e+38f )  !=  1 ) exit (134);
        if ( precision_flt( blk_real.r_c_float_s4d         , -1.175494e-38f )  !=  1 ) exit (135);

        if ( precision_dbl( blk_real.r_c_double_s8a        ,  1.797693e+308 )  !=  1 ) exit (136);
        if ( precision_dbl( blk_real.r_c_double_s8b        ,  2.225073e-308 )  !=  1 ) exit (137);
        if ( precision_dbl( blk_real.r_c_double_s8c        , -1.797693e+308 )  !=  1 ) exit (138);
        if ( precision_dbl( blk_real.r_c_double_s8d        , -2.225073e-308 )  !=  1 ) exit (139);


/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

        blk_real.real_s4d              =  3.402823e+38f;
        blk_real.real_s4c              =  1.175494e-38f;
        blk_real.real_s4b              = -3.402823e+38f;
        blk_real.real_s4a              = -1.175494e-38f;

        blk_real.real_s8d              =  1.797693e+308;
        blk_real.real_s8c              =  2.225073e-308;
        blk_real.real_s8b              = -1.797693e+308;
        blk_real.real_s8a              = -2.225073e-308;

        blk_real.real_s16d            =  1.797693e+308l;
        blk_real.real_s16c            =  2.225073e-308l;
        blk_real.real_s16b            = -1.797693e+308l;
        blk_real.real_s16a            = -2.225073e-308l;

        blk_real.r_c_float_s4d         =  3.402823e+38f;
        blk_real.r_c_float_s4c         =  1.175494e-38f;
        blk_real.r_c_float_s4b         = -3.402823e+38f;
        blk_real.r_c_float_s4a         = -1.175494e-38f;

        blk_real.r_c_double_s8d        =  1.797693e+308;
        blk_real.r_c_double_s8c        =  2.225073e-308;
        blk_real.r_c_double_s8b        = -1.797693e+308;
        blk_real.r_c_double_s8a        = -2.225073e-308;


/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/

        if ( precision_flt( blk_real.real_s4d              ,  3.402823e+38f )  !=  1 ) exit (150);
        if ( precision_flt( blk_real.real_s4c              ,  1.175494e-38f )  !=  1 ) exit (151);
        if ( precision_flt( blk_real.real_s4b              , -3.402823e+38f )  !=  1 ) exit (152);
        if ( precision_flt( blk_real.real_s4a              , -1.175494e-38f )  !=  1 ) exit (153);

        if ( precision_dbl( blk_real.real_s8d              ,  1.797693e+308 )  !=  1 ) exit (154);
        if ( precision_dbl( blk_real.real_s8c              ,  2.225073e-308 )  !=  1 ) exit (155);
        if ( precision_dbl( blk_real.real_s8b              , -1.797693e+308 )  !=  1 ) exit (156);
        if ( precision_dbl( blk_real.real_s8a              , -2.225073e-308 )  !=  1 ) exit (157);

        if ( precision_ldbl( blk_real.real_s16d            ,  1.797693e+308l ) !=  1 ) exit (158);
        if ( precision_ldbl( blk_real.real_s16c            ,  2.225073e-308l ) !=  1 ) exit (159);
        if ( precision_ldbl( blk_real.real_s16b            , -1.797693e+308l ) !=  1 ) exit (160);
        if ( precision_ldbl( blk_real.real_s16a            , -2.225073e-308l ) !=  1 ) exit (161);

        if ( precision_flt( blk_real.r_c_float_s4d         ,  3.402823e+38f )  !=  1 ) exit (162);
        if ( precision_flt( blk_real.r_c_float_s4c         ,  1.175494e-38f )  !=  1 ) exit (163);
        if ( precision_flt( blk_real.r_c_float_s4b         , -3.402823e+38f )  !=  1 ) exit (164);
        if ( precision_flt( blk_real.r_c_float_s4a         , -1.175494e-38f )  !=  1 ) exit (165);

        if ( precision_dbl( blk_real.r_c_double_s8d        ,  1.797693e+308 )  !=  1 ) exit (166);
        if ( precision_dbl( blk_real.r_c_double_s8c        ,  2.225073e-308 )  !=  1 ) exit (167);
        if ( precision_dbl( blk_real.r_c_double_s8b        , -1.797693e+308 )  !=  1 ) exit (168);
        if ( precision_dbl( blk_real.r_c_double_s8a        , -2.225073e-308 )  !=  1 ) exit (169);


}

