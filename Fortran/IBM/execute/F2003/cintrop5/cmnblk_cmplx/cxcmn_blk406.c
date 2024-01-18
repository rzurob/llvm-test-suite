#include "cmplx.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"


struct {

        float _Complex        cmplx_8a[2][2][2];
        float _Complex        cmplx_8b[2][2][2];
        float _Complex        cmplx_8c[2][2][2];
        float _Complex        cmplx_8d[2][2][2];

        double _Complex        cmplx_16a[2][2][2];
        double _Complex        cmplx_16b[2][2][2];
        double _Complex        cmplx_16c[2][2][2];
        double _Complex        cmplx_16d[2][2][2];

        //  c-code must be compiled with xlc -qlongdouble for complex*32
        long double _Complex        cmplx_32a[2][2][2];
        long double _Complex        cmplx_32b[2][2][2];
        long double _Complex        cmplx_32c[2][2][2];
        long double _Complex        cmplx_32d[2][2][2];

        float _Complex       cmplx_float_complex_a[2][2][2];
        float _Complex       cmplx_float_complex_b[2][2][2];
        float _Complex       cmplx_float_complex_c[2][2][2];
        float _Complex       cmplx_float_complex_d[2][2][2];

        double _Complex        cmplx_double_complex_a[2][2][2];
        double _Complex        cmplx_double_complex_b[2][2][2];
        double _Complex        cmplx_double_complex_c[2][2][2];
        double _Complex        cmplx_double_complex_d[2][2][2];

} blk_cmplx;




void csub_cmplx(){

        // variables used to get the real part of complex values from fortran
        float    r_cmplx_8a[2][2][2];
        float    r_cmplx_8b[2][2][2];
        float    r_cmplx_8c[2][2][2];
        float    r_cmplx_8d[2][2][2];

        double    r_cmplx_16a[2][2][2];
        double    r_cmplx_16b[2][2][2];
        double    r_cmplx_16c[2][2][2];
        double    r_cmplx_16d[2][2][2];

        long double    r_cmplx_32a[2][2][2];
        long double    r_cmplx_32b[2][2][2];
        long double    r_cmplx_32c[2][2][2];
        long double    r_cmplx_32d[2][2][2];

        float   r_cmplx_float_complex_a[2][2][2];
        float   r_cmplx_float_complex_b[2][2][2];
        float   r_cmplx_float_complex_c[2][2][2];
        float   r_cmplx_float_complex_d[2][2][2];

        double    r_cmplx_double_complex_a[2][2][2];
        double    r_cmplx_double_complex_b[2][2][2];
        double    r_cmplx_double_complex_c[2][2][2];
        double    r_cmplx_double_complex_d[2][2][2];

        // variables used to get the imaginary part of complex values from fortran
        float    i_cmplx_8a[2][2][2];
        float    i_cmplx_8b[2][2][2];
        float    i_cmplx_8c[2][2][2];
        float    i_cmplx_8d[2][2][2];

        double    i_cmplx_16a[2][2][2];
        double    i_cmplx_16b[2][2][2];
        double    i_cmplx_16c[2][2][2];
        double    i_cmplx_16d[2][2][2];

        long double    i_cmplx_32a[2][2][2];
        long double    i_cmplx_32b[2][2][2];
        long double    i_cmplx_32c[2][2][2];
        long double    i_cmplx_32d[2][2][2];

        float   i_cmplx_float_complex_a[2][2][2];
        float   i_cmplx_float_complex_b[2][2][2];
        float   i_cmplx_float_complex_c[2][2][2];
        float   i_cmplx_float_complex_d[2][2][2];

        double    i_cmplx_double_complex_a[2][2][2];
        double    i_cmplx_double_complex_b[2][2][2];
        double    i_cmplx_double_complex_c[2][2][2];
        double    i_cmplx_double_complex_d[2][2][2];

 	int i,j,k;

/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/


     for ( i = 0; i < 2; i++ ) {
       for ( j = 0; j < 2; j++ ) {
         for ( k = 0; k < 2; k++ ) {

        // get the real part of complex values from fortran
        r_cmplx_8a[k][j][i]  =  crealf( blk_cmplx.cmplx_8a[k][j][i]);
        r_cmplx_8b[k][j][i]  =  crealf( blk_cmplx.cmplx_8b[k][j][i]);
        r_cmplx_8c[k][j][i]  =  crealf( blk_cmplx.cmplx_8c[k][j][i]);
        r_cmplx_8d[k][j][i]  =  crealf( blk_cmplx.cmplx_8d[k][j][i]);
 
        r_cmplx_16a[k][j][i]  =  creal( blk_cmplx.cmplx_16a[k][j][i]);
        r_cmplx_16b[k][j][i]  =  creal( blk_cmplx.cmplx_16b[k][j][i]);
        r_cmplx_16c[k][j][i]  =  creal( blk_cmplx.cmplx_16c[k][j][i]);
        r_cmplx_16d[k][j][i]  =  creal( blk_cmplx.cmplx_16d[k][j][i]);
 
        r_cmplx_32a[k][j][i]  =  creall( blk_cmplx.cmplx_32a[k][j][i]);
        r_cmplx_32b[k][j][i]  =  creall( blk_cmplx.cmplx_32b[k][j][i]);
        r_cmplx_32c[k][j][i]  =  creall( blk_cmplx.cmplx_32c[k][j][i]);
        r_cmplx_32d[k][j][i]  =  creall( blk_cmplx.cmplx_32d[k][j][i]);
 
        r_cmplx_float_complex_a[k][j][i]  =  crealf( blk_cmplx.cmplx_float_complex_a[k][j][i]);
        r_cmplx_float_complex_b[k][j][i]  =  crealf( blk_cmplx.cmplx_float_complex_b[k][j][i]);
        r_cmplx_float_complex_c[k][j][i]  =  crealf( blk_cmplx.cmplx_float_complex_c[k][j][i]);
        r_cmplx_float_complex_d[k][j][i]  =  crealf( blk_cmplx.cmplx_float_complex_d[k][j][i]);
 
        r_cmplx_double_complex_a[k][j][i]  =  creal( blk_cmplx.cmplx_double_complex_a[k][j][i]);
        r_cmplx_double_complex_b[k][j][i]  =  creal( blk_cmplx.cmplx_double_complex_b[k][j][i]);
        r_cmplx_double_complex_c[k][j][i]  =  creal( blk_cmplx.cmplx_double_complex_c[k][j][i]);
        r_cmplx_double_complex_d[k][j][i]  =  creal( blk_cmplx.cmplx_double_complex_d[k][j][i]);
 

        // get the imaginary part of complex values from fortran
        i_cmplx_8a[k][j][i]  =  cimagf( blk_cmplx.cmplx_8a[k][j][i]);
        i_cmplx_8b[k][j][i]  =  cimagf( blk_cmplx.cmplx_8b[k][j][i]);
        i_cmplx_8c[k][j][i]  =  cimagf( blk_cmplx.cmplx_8c[k][j][i]);
        i_cmplx_8d[k][j][i]  =  cimagf( blk_cmplx.cmplx_8d[k][j][i]);
 
        i_cmplx_16a[k][j][i]  =  cimag( blk_cmplx.cmplx_16a[k][j][i]);
        i_cmplx_16b[k][j][i]  =  cimag( blk_cmplx.cmplx_16b[k][j][i]);
        i_cmplx_16c[k][j][i]  =  cimag( blk_cmplx.cmplx_16c[k][j][i]);
        i_cmplx_16d[k][j][i]  =  cimag( blk_cmplx.cmplx_16d[k][j][i]);
 
        i_cmplx_32a[k][j][i]  =  cimagl( blk_cmplx.cmplx_32a[k][j][i]);
        i_cmplx_32b[k][j][i]  =  cimagl( blk_cmplx.cmplx_32b[k][j][i]);
        i_cmplx_32c[k][j][i]  =  cimagl( blk_cmplx.cmplx_32c[k][j][i]);
        i_cmplx_32d[k][j][i]  =  cimagl( blk_cmplx.cmplx_32d[k][j][i]);
 
        i_cmplx_float_complex_a[k][j][i]  =  cimagf( blk_cmplx.cmplx_float_complex_a[k][j][i]);
        i_cmplx_float_complex_b[k][j][i]  =  cimagf( blk_cmplx.cmplx_float_complex_b[k][j][i]);
        i_cmplx_float_complex_c[k][j][i]  =  cimagf( blk_cmplx.cmplx_float_complex_c[k][j][i]);
        i_cmplx_float_complex_d[k][j][i]  =  cimagf( blk_cmplx.cmplx_float_complex_d[k][j][i]);
 
        i_cmplx_double_complex_a[k][j][i]  =  cimag( blk_cmplx.cmplx_double_complex_a[k][j][i]);
        i_cmplx_double_complex_b[k][j][i]  =  cimag( blk_cmplx.cmplx_double_complex_b[k][j][i]);
        i_cmplx_double_complex_c[k][j][i]  =  cimag( blk_cmplx.cmplx_double_complex_c[k][j][i]);
        i_cmplx_double_complex_d[k][j][i]  =  cimag( blk_cmplx.cmplx_double_complex_d[k][j][i]);
 
         }
       }
     } 


     for ( i = 0; i < 2; i++ ) {
       for ( j = 0; j < 2; j++ ) {
         for ( k = 0; k < 2; k++ ) {

	// Verify REAL part of COMPLEX
        if ( precision_flt( r_cmplx_8a[k][j][i]                 ,  3.4f  )  	!=  1 ) exit (10);
        if ( precision_flt( r_cmplx_8b[k][j][i]                 ,  3.4f  )  	!=  1 ) exit (11);
        if ( precision_flt( r_cmplx_8c[k][j][i]                 , -1.17f )  	!=  1 ) exit (12);
        if ( precision_flt( r_cmplx_8d[k][j][i]                 , -3.4f  )  	!=  1 ) exit (13);

        if ( precision_dbl( r_cmplx_16a[k][j][i]                ,  1.79 )  	!=  1 ) exit (14);
        if ( precision_dbl( r_cmplx_16b[k][j][i]                ,  1.79 )  	!=  1 ) exit (15);
        if ( precision_dbl( r_cmplx_16c[k][j][i]                , -2.22 )  	!=  1 ) exit (16);
        if ( precision_dbl( r_cmplx_16d[k][j][i]                ,  1.79 )  	!=  1 ) exit (17);

        if ( precision_ldbl( r_cmplx_32a[k][j][i]               ,  1.797693l  )  !=  1 ) exit (18);
        if ( precision_ldbl( r_cmplx_32b[k][j][i]               ,  1.797693l  )  !=  1 ) exit (19);
        if ( precision_ldbl( r_cmplx_32c[k][j][i]               , -2.225073l  )  !=  1 ) exit (20);
        if ( precision_ldbl( r_cmplx_32d[k][j][i]               ,  1.797693l  )  !=  1 ) exit (21);

        if ( precision_flt( r_cmplx_float_complex_a[k][j][i]    ,  3.4028f )  	!=  1 ) exit (22);
        if ( precision_flt( r_cmplx_float_complex_b[k][j][i]    ,  3.4028f )  	!=  1 ) exit (23);
        if ( precision_flt( r_cmplx_float_complex_c[k][j][i]    , -1.1754f )  	!=  1 ) exit (24);
        if ( precision_flt( r_cmplx_float_complex_d[k][j][i]    , -3.4028f )  	!=  1 ) exit (25);

        if ( precision_dbl( r_cmplx_double_complex_a[k][j][i]   ,  1.797693 )  	!=  1 ) exit (26);
        if ( precision_dbl( r_cmplx_double_complex_b[k][j][i]   ,  1.797693 )  	!=  1 ) exit (27);
        if ( precision_dbl( r_cmplx_double_complex_c[k][j][i]   , -2.225073 )  	!=  1 ) exit (28);
        if ( precision_dbl( r_cmplx_double_complex_d[k][j][i]   ,  1.797693 )  	!=  1 ) exit (29);


	// Verify IMAGINARY part of COMPLEX
        if ( precision_flt( i_cmplx_8a[k][j][i]                 ,  3.4f   )      !=  1 ) exit (30);
        if ( precision_flt( i_cmplx_8b[k][j][i]                 , -3.4f   )      !=  1 ) exit (31);
        if ( precision_flt( i_cmplx_8c[k][j][i]                , -3.4f   )      !=  1 ) exit (32);
        if ( precision_flt( i_cmplx_8d[k][j][i]                 , -1.175f )      !=  1 ) exit (33);

        if ( precision_dbl( i_cmplx_16a[k][j][i]                ,  1.79 )        !=  1 ) exit (34);
        if ( precision_dbl( i_cmplx_16b[k][j][i]                , -1.79 )        !=  1 ) exit (35);
        if ( precision_dbl( i_cmplx_16c[k][j][i]                , -1.79 )        !=  1 ) exit (36);
        if ( precision_dbl( i_cmplx_16d[k][j][i]                , -2.22 )        !=  1 ) exit (37);

        if ( precision_ldbl( i_cmplx_32a[k][j][i]               ,  1.797693l  )  !=  1 ) exit (38);
        if ( precision_ldbl( i_cmplx_32b[k][j][i]               , -1.797693l  )  !=  1 ) exit (39);
        if ( precision_ldbl( i_cmplx_32c[k][j][i]               , -1.797693l  )  !=  1 ) exit (40);
        if ( precision_ldbl( i_cmplx_32d[k][j][i]               , -2.225073l  )  !=  1 ) exit (41);

        if ( precision_flt( i_cmplx_float_complex_a[k][j][i]    ,  3.4028f )     !=  1 ) exit (42);
        if ( precision_flt( i_cmplx_float_complex_b[k][j][i]    , -3.4028f )     !=  1 ) exit (43);
        if ( precision_flt( i_cmplx_float_complex_c[k][j][i]    , -3.4028f )     !=  1 ) exit (44);
        if ( precision_flt( i_cmplx_float_complex_d[k][j][i]    , -1.1754f )     !=  1 ) exit (45);

        if ( precision_dbl( i_cmplx_double_complex_a[k][j][i]   ,  1.797693 )    !=  1 ) exit (46);
        if ( precision_dbl( i_cmplx_double_complex_b[k][j][i]   , -1.797693 )    !=  1 ) exit (47);
        if ( precision_dbl( i_cmplx_double_complex_c[k][j][i]   , -1.797693 )    !=  1 ) exit (48);
        if ( precision_dbl( i_cmplx_double_complex_d[k][j][i]   , -2.225073 )    !=  1 ) exit (49);

         }
       }
     }



/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

     for ( i = 0; i < 2; i++ ) {
       for ( j = 0; j < 2; j++ ) {
         for ( k = 0; k < 2; k++ ) {

        // Modify REAL part of COMPLEX 
        r_cmplx_8a[k][j][i]  =  300.119f;
        r_cmplx_8b[k][j][i]  = -300.119f;
        r_cmplx_8c[k][j][i]  =  1000.009f;
        r_cmplx_8d[k][j][i]  = -1000.009f;

        r_cmplx_16a[k][j][i]  =  1234300.11911;
        r_cmplx_16b[k][j][i]  = -1234300.11911;
        r_cmplx_16c[k][j][i]  =  12341000.00911;
        r_cmplx_16d[k][j][i]  = -12341000.00911;

        r_cmplx_32a[k][j][i]  =  987654321300.11998l;
        r_cmplx_32b[k][j][i]  = -987654321300.11998l;
        r_cmplx_32c[k][j][i]  =  9876543211000.00998l;
        r_cmplx_32d[k][j][i]  = -9876543211000.00998l;

        r_cmplx_float_complex_a[k][j][i]  =  300.119f;
        r_cmplx_float_complex_b[k][j][i]  = -300.119f;
        r_cmplx_float_complex_c[k][j][i]  =  1000.009f;
        r_cmplx_float_complex_d[k][j][i]  = -1000.009f;

        r_cmplx_double_complex_a[k][j][i]  =  1234300.11911;
        r_cmplx_double_complex_b[k][j][i]  = -1234300.11911;
        r_cmplx_double_complex_c[k][j][i]  =  12341000.00911;
        r_cmplx_double_complex_d[k][j][i]  = -12341000.00911;


        // Modify IMAGINARY part of COMPLEX
        i_cmplx_8a[k][j][i]  =  300.119f;
        i_cmplx_8b[k][j][i]  = -300.119f;
        i_cmplx_8c[k][j][i]  = -1000.009f;
        i_cmplx_8d[k][j][i]  =  1000.009f;

        i_cmplx_16a[k][j][i]  =  1234300.11911;
        i_cmplx_16b[k][j][i]  = -1234300.11911;
        i_cmplx_16c[k][j][i]  = -12341000.00911;
        i_cmplx_16d[k][j][i]  =  12341000.00911;

        i_cmplx_32a[k][j][i]  =  987654321300.11998l;
        i_cmplx_32b[k][j][i]  = -987654321300.11998l;
        i_cmplx_32c[k][j][i]  = -9876543211000.00998l;
        i_cmplx_32d[k][j][i]  =  9876543211000.00998l;

        i_cmplx_float_complex_a[k][j][i]  =  300.119f;
        i_cmplx_float_complex_b[k][j][i]  = -300.119f;
        i_cmplx_float_complex_c[k][j][i]  = -1000.009f;
        i_cmplx_float_complex_d[k][j][i]  =  1000.009f;

        i_cmplx_double_complex_a[k][j][i]  =  1234300.11911;
        i_cmplx_double_complex_b[k][j][i]  = -1234300.11911;
        i_cmplx_double_complex_c[k][j][i]  = -12341000.00911;
        i_cmplx_double_complex_d[k][j][i]  =  12341000.00911;


         }
       }
     }



     for ( i = 0; i < 2; i++ ) {
       for ( j = 0; j < 2; j++ ) {
         for ( k = 0; k < 2; k++ ) {

	// Create COMPLEX from REAL and IMAGINARY part
        blk_cmplx.cmplx_8a[k][j][i]  = createcomplexf( r_cmplx_8a[k][j][i], i_cmplx_8a[k][j][i] );
        blk_cmplx.cmplx_8b[k][j][i]  = createcomplexf( r_cmplx_8b[k][j][i], i_cmplx_8b[k][j][i] );
        blk_cmplx.cmplx_8c[k][j][i]  = createcomplexf( r_cmplx_8c[k][j][i], i_cmplx_8c[k][j][i] );
        blk_cmplx.cmplx_8d[k][j][i]  = createcomplexf( r_cmplx_8d[k][j][i], i_cmplx_8d[k][j][i] );
     
        blk_cmplx.cmplx_16a[k][j][i]  = createcomplex( r_cmplx_16a[k][j][i], i_cmplx_16a[k][j][i] );
        blk_cmplx.cmplx_16b[k][j][i]  = createcomplex( r_cmplx_16b[k][j][i], i_cmplx_16b[k][j][i] );
        blk_cmplx.cmplx_16c[k][j][i]  = createcomplex( r_cmplx_16c[k][j][i], i_cmplx_16c[k][j][i] );
        blk_cmplx.cmplx_16d[k][j][i]  = createcomplex( r_cmplx_16d[k][j][i], i_cmplx_16d[k][j][i] );
      
        blk_cmplx.cmplx_32a[k][j][i]  = createcomplexl( r_cmplx_32a[k][j][i], i_cmplx_32a[k][j][i] );
        blk_cmplx.cmplx_32b[k][j][i]  = createcomplexl( r_cmplx_32b[k][j][i], i_cmplx_32b[k][j][i] );
        blk_cmplx.cmplx_32c[k][j][i]  = createcomplexl( r_cmplx_32c[k][j][i], i_cmplx_32c[k][j][i] );
        blk_cmplx.cmplx_32d[k][j][i]  = createcomplexl( r_cmplx_32d[k][j][i], i_cmplx_32d[k][j][i] );
     
        blk_cmplx.cmplx_float_complex_a[k][j][i]  = createcomplexf( r_cmplx_float_complex_a[k][j][i], i_cmplx_float_complex_a[k][j][i] );
        blk_cmplx.cmplx_float_complex_b[k][j][i]  = createcomplexf( r_cmplx_float_complex_b[k][j][i], i_cmplx_float_complex_b[k][j][i] );
        blk_cmplx.cmplx_float_complex_c[k][j][i]  = createcomplexf( r_cmplx_float_complex_c[k][j][i], i_cmplx_float_complex_c[k][j][i] );
        blk_cmplx.cmplx_float_complex_d[k][j][i]  = createcomplexf( r_cmplx_float_complex_d[k][j][i], i_cmplx_float_complex_d[k][j][i] );
     
        blk_cmplx.cmplx_double_complex_a[k][j][i]  = createcomplex( r_cmplx_double_complex_a[k][j][i], i_cmplx_double_complex_a[k][j][i] );
        blk_cmplx.cmplx_double_complex_b[k][j][i]  = createcomplex( r_cmplx_double_complex_b[k][j][i], i_cmplx_double_complex_b[k][j][i] );
        blk_cmplx.cmplx_double_complex_c[k][j][i]  = createcomplex( r_cmplx_double_complex_c[k][j][i], i_cmplx_double_complex_c[k][j][i] );
        blk_cmplx.cmplx_double_complex_d[k][j][i]  = createcomplex( r_cmplx_double_complex_d[k][j][i], i_cmplx_double_complex_d[k][j][i] );

         }
       }
     }

     
//        blk_cmplx.cmplx_long_double_complex_a[k][j][i]  = createcomplex( r_cmplx_long_double_complex_a[k][j][i],i_cmplx_long_double_complex_a[k][j][i] );
//        blk_cmplx.cmplx_long_double_complex_b[k][j][i]  = createcomplex( r_cmplx_long_double_complex_b[k][j][i],i_cmplx_long_double_complex_b[k][j][i] );
//        blk_cmplx.cmplx_long_double_complex_c[k][j][i]  = createcomplex( r_cmplx_long_double_complex_c[k][j][i],i_cmplx_long_double_complex_c[k][j][i] );
//        blk_cmplx.cmplx_long_double_complex_d[k][j][i]  = createcomplex( r_cmplx_long_double_complex_d[k][j][i],i_cmplx_long_double_complex_d[k][j][i] );


/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/


     for ( i = 0; i < 2; i++ ) {
       for ( j = 0; j < 2; j++ ) {
         for ( k = 0; k < 2; k++ ) {

        // Verify REAL part of COMPLEX
        if ( precision_flt( r_cmplx_8a[k][j][i],     300.119f )      		!=  1 ) exit (50);
        if ( precision_flt( r_cmplx_8b[k][j][i],    -300.119f )      		!=  1 ) exit (50);
        if ( precision_flt( r_cmplx_8c[k][j][i],     1000.009f )      		!=  1 ) exit (50);
        if ( precision_flt( r_cmplx_8d[k][j][i],    -1000.009f )      		!=  1 ) exit (50);
   
        if ( precision_dbl( r_cmplx_16a[k][j][i],    1234300.11911 )    		!=  1 ) exit (51);
        if ( precision_dbl( r_cmplx_16b[k][j][i],   -1234300.11911 )      	!=  1 ) exit (51);
        if ( precision_dbl( r_cmplx_16c[k][j][i],    12341000.00911 )   		!=  1 ) exit (51);
        if ( precision_dbl( r_cmplx_16d[k][j][i],   -12341000.00911 )      	!=  1 ) exit (51);
    
        if ( precision_ldbl( r_cmplx_32a[k][j][i],     987654321300.11998l  )    !=  1 ) exit (52);
        if ( precision_ldbl( r_cmplx_32b[k][j][i],    -987654321300.11998l  )    !=  1 ) exit (52);
        if ( precision_ldbl( r_cmplx_32c[k][j][i],     9876543211000.00998l )    !=  1 ) exit (52);
        if ( precision_ldbl( r_cmplx_32d[k][j][i],    -9876543211000.00998l )    !=  1 ) exit (52);
    
        if ( precision_flt( r_cmplx_float_complex_a[k][j][i],     300.119f )     !=  1 ) exit (53);
        if ( precision_flt( r_cmplx_float_complex_b[k][j][i],    -300.119f )     !=  1 ) exit (53);
        if ( precision_flt( r_cmplx_float_complex_c[k][j][i],     1000.009f )    !=  1 ) exit (53);
        if ( precision_flt( r_cmplx_float_complex_d[k][j][i],    -1000.009f )    !=  1 ) exit (53);
    
        if ( precision_dbl( r_cmplx_double_complex_a[k][j][i],  1234300.11911 )  !=  1 ) exit (54);
        if ( precision_dbl( r_cmplx_double_complex_b[k][j][i], -1234300.11911 )  !=  1 ) exit (54);
        if ( precision_dbl( r_cmplx_double_complex_c[k][j][i],  12341000.00911 ) !=  1 ) exit (54);
        if ( precision_dbl( r_cmplx_double_complex_d[k][j][i], -12341000.00911 ) !=  1 ) exit (54);
    
//        if ( precision_dbl( r_cmplx_long_double_complex_a[k][j][i],   1234300.119 )      !=  1 ) exit (70);
//        if ( precision_dbl( r_cmplx_long_double_complex_b[k][j][i],   -1234300.119 )      !=  1 ) exit (70);
//        if ( precision_dbl( r_cmplx_long_double_complex_c[k][j][i],   12341000.01 )      !=  1 ) exit (70);
//        if ( precision_dbl( r_cmplx_long_double_complex_d[k][j][i],   -12341000.01 )      !=  1 ) exit (70);


        // Verify IMAGINARY part of COMPLEX
        if ( precision_flt( i_cmplx_8a[k][j][i],      300.119f )      		!=  1 ) exit (55);
        if ( precision_flt( i_cmplx_8b[k][j][i],     -300.119f )      		!=  1 ) exit (55);
        if ( precision_flt( i_cmplx_8c[k][j][i],     -1000.009f )      		!=  1 ) exit (55);
        if ( precision_flt( i_cmplx_8d[k][j][i],      1000.009f )      		!=  1 ) exit (55);
    
        if ( precision_dbl( i_cmplx_16a[k][j][i],    1234300.11911 )      		!=  1 ) exit (56);
        if ( precision_dbl( i_cmplx_16b[k][j][i],    -1234300.11911 )      	!=  1 ) exit (56);
        if ( precision_dbl( i_cmplx_16c[k][j][i],    -12341000.00911 )      	!=  1 ) exit (56);
        if ( precision_dbl( i_cmplx_16d[k][j][i],    12341000.00911 )      		!=  1 ) exit (56);
    
        if ( precision_ldbl( i_cmplx_32a[k][j][i],      987654321300.11998l )    !=  1 ) exit (57);
        if ( precision_ldbl( i_cmplx_32b[k][j][i],     -987654321300.11998l )    !=  1 ) exit (57);
        if ( precision_ldbl( i_cmplx_32c[k][j][i],     -9876543211000.00998l )   !=  1 ) exit (57);
        if ( precision_ldbl( i_cmplx_32d[k][j][i],      9876543211000.00998l )   !=  1 ) exit (57);
    
        if ( precision_flt( i_cmplx_float_complex_a[k][j][i],      300.119f )    !=  1 ) exit (58);
        if ( precision_flt( i_cmplx_float_complex_b[k][j][i],     -300.119f )    !=  1 ) exit (58);
        if ( precision_flt( i_cmplx_float_complex_c[k][j][i],     -1000.009f )   !=  1 ) exit (58);
        if ( precision_flt( i_cmplx_float_complex_d[k][j][i],      1000.009f )   !=  1 ) exit (58);
    
        if ( precision_dbl( i_cmplx_double_complex_a[k][j][i],    1234300.11911 )  !=  1 ) exit (59);
        if ( precision_dbl( i_cmplx_double_complex_b[k][j][i],    -1234300.11911 ) !=  1 ) exit (59);
        if ( precision_dbl( i_cmplx_double_complex_c[k][j][i],    -12341000.00911 ) !=  1 ) exit (59);
        if ( precision_dbl( i_cmplx_double_complex_d[k][j][i],    12341000.00911 )  !=  1 ) exit (59);
    
//        if ( precision_dbl( r_cmplx_long_double_complex_a[k][j][i],   1234300.119 )      !=  1 ) exit (70);
//        if ( precision_dbl( r_cmplx_long_double_complex_b[k][j][i],   -1234300.119 )      !=  1 ) exit (70);
//        if ( precision_dbl( r_cmplx_long_double_complex_c[k][j][i],   -12341000.01 )      !=  1 ) exit (70);
 //       if ( precision_dbl( r_cmplx_long_double_complex_d[k][j][i],   12341000.01 )      !=  1 ) exit (70);

         }
       }
     }



}

