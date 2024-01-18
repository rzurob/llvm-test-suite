   #include <inttypes.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"


float                           blk_real_s4a;
float                           blk_real_s4b;
float                           blk_real_s4c;
float                           blk_real_s4d;

double                          blk_real_s8a;
double                          blk_real_s8b;
double                          blk_real_s8c;
double                          blk_real_s8d;

float                           blk_r_c_float_s4a;
float                           blk_r_c_float_s4b;
float                           blk_r_c_float_s4c;
float                           blk_r_c_float_s4d;

double                          blk_r_c_double_s8a;
double                          blk_r_c_double_s8b;
double                          blk_r_c_double_s8c;
double                          blk_r_c_double_s8d;

long double                     blk_r_c_long_double_s8a;
long double                     blk_r_c_long_double_s8b;
long double                     blk_r_c_long_double_s8c;
long double                     blk_r_c_long_double_s8d;


void csub_real(){

/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/

        if ( precision_flt( blk_real_s4a              ,  3.402823e+38f )  !=  1 ) exit (120);
        if ( precision_flt( blk_real_s4b              ,  1.175494e-38f )  !=  1 ) exit (121);
        if ( precision_flt( blk_real_s4c              , -3.402823e+38f )  !=  1 ) exit (122);
        if ( precision_flt( blk_real_s4d              , -1.175494e-38f )  !=  1 ) exit (123);

        if ( precision_dbl( blk_real_s8a              ,  1.797693e+308 )  !=  1 ) exit (124);
        if ( precision_dbl( blk_real_s8b              ,  2.225073e-308 )  !=  1 ) exit (125);
        if ( precision_dbl( blk_real_s8c              , -1.797693e+308 )  !=  1 ) exit (126);
        if ( precision_dbl( blk_real_s8d              , -2.225073e-308 )  !=  1 ) exit (127);

        if ( precision_flt( blk_r_c_float_s4a         ,  3.402823e+38f )  !=  1 ) exit (132);
        if ( precision_flt( blk_r_c_float_s4b         ,  1.175494e-38f )  !=  1 ) exit (133);
        if ( precision_flt( blk_r_c_float_s4c         , -3.402823e+38f )  !=  1 ) exit (134);
        if ( precision_flt( blk_r_c_float_s4d         , -1.175494e-38f )  !=  1 ) exit (135);

        if ( precision_dbl( blk_r_c_double_s8a        ,  1.797693e+308 )  !=  1 ) exit (136);
        if ( precision_dbl( blk_r_c_double_s8b        ,  2.225073e-308 )  !=  1 ) exit (137);
        if ( precision_dbl( blk_r_c_double_s8c        , -1.797693e+308 )  !=  1 ) exit (138);
        if ( precision_dbl( blk_r_c_double_s8d        , -2.225073e-308 )  !=  1 ) exit (139);

        if ( precision_ldbl( blk_r_c_long_double_s8a   ,  1.797693e+308l )  !=  1 ) exit (140);
        if ( precision_ldbl( blk_r_c_long_double_s8b   ,  2.004168e-292l )  !=  1 ) exit (141);
        if ( precision_ldbl( blk_r_c_long_double_s8c   , -1.797693e+308l )  !=  1 ) exit (142);
        if ( precision_ldbl( blk_r_c_long_double_s8d   , -2.004168e-292l )  !=  1 ) exit (143);


/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

        blk_real_s4d              =  3.402823e+38f;
        blk_real_s4c              =  1.175494e-38f;
        blk_real_s4b              = -3.402823e+38f;
        blk_real_s4a              = -1.175494e-38f;

        blk_real_s8d              =  1.797693e+308;
        blk_real_s8c              =  2.225073e-308;
        blk_real_s8b              = -1.797693e+308;
        blk_real_s8a              = -2.225073e-308;

        blk_r_c_float_s4d         =  3.402823e+38f;
        blk_r_c_float_s4c         =  1.175494e-38f;
        blk_r_c_float_s4b         = -3.402823e+38f;
        blk_r_c_float_s4a         = -1.175494e-38f;

        blk_r_c_double_s8d        =  1.797693e+308;
        blk_r_c_double_s8c        =  2.225073e-308;
        blk_r_c_double_s8b        = -1.797693e+308;
        blk_r_c_double_s8a        = -2.225073e-308;

        blk_r_c_long_double_s8d   =  1.797693e+308l;
        blk_r_c_long_double_s8c   =  2.004168e-292l;
        blk_r_c_long_double_s8b   = -1.797693e+308l;
        blk_r_c_long_double_s8a   = -2.004168e-292l;


/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/

        if ( precision_flt( blk_real_s4d              ,  3.402823e+38f )  !=  1 ) exit (150);
        if ( precision_flt( blk_real_s4c              ,  1.175494e-38f )  !=  1 ) exit (151);
        if ( precision_flt( blk_real_s4b              , -3.402823e+38f )  !=  1 ) exit (152);
        if ( precision_flt( blk_real_s4a              , -1.175494e-38f )  !=  1 ) exit (153);

        if ( precision_dbl( blk_real_s8d              ,  1.797693e+308 )  !=  1 ) exit (154);
        if ( precision_dbl( blk_real_s8c              ,  2.225073e-308 )  !=  1 ) exit (155);
        if ( precision_dbl( blk_real_s8b              , -1.797693e+308 )  !=  1 ) exit (156);
        if ( precision_dbl( blk_real_s8a              , -2.225073e-308 )  !=  1 ) exit (157);

        if ( precision_flt( blk_r_c_float_s4d         ,  3.402823e+38f )  !=  1 ) exit (162);
        if ( precision_flt( blk_r_c_float_s4c         ,  1.175494e-38f )  !=  1 ) exit (163);
        if ( precision_flt( blk_r_c_float_s4b         , -3.402823e+38f )  !=  1 ) exit (164);
        if ( precision_flt( blk_r_c_float_s4a         , -1.175494e-38f )  !=  1 ) exit (165);

        if ( precision_dbl( blk_r_c_double_s8d        ,  1.797693e+308 )  !=  1 ) exit (166);
        if ( precision_dbl( blk_r_c_double_s8c        ,  2.225073e-308 )  !=  1 ) exit (167);
        if ( precision_dbl( blk_r_c_double_s8b        , -1.797693e+308 )  !=  1 ) exit (168);
        if ( precision_dbl( blk_r_c_double_s8a        , -2.225073e-308 )  !=  1 ) exit (169);

        if ( precision_ldbl( blk_r_c_long_double_s8d   ,  1.797693e+308l ) !=  1 ) exit (170);
        if ( precision_ldbl( blk_r_c_long_double_s8c   ,  2.004168e-292l ) !=  1 ) exit (171);
        if ( precision_ldbl( blk_r_c_long_double_s8b   , -1.797693e+308l ) !=  1 ) exit (172);
        if ( precision_ldbl( blk_r_c_long_double_s8a   , -2.004168e-292l ) !=  1 ) exit (173);

}

