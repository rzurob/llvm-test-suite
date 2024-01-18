#include "cmplx.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"


struct {

        float _Complex        cmplx_8a[3];
        float _Complex        cmplx_8b[3];
        float _Complex        cmplx_8c[3];
        float _Complex        cmplx_8d[3];

        double _Complex        cmplx_16a[3];
        double _Complex        cmplx_16b[3];
        double _Complex        cmplx_16c[3];
        double _Complex        cmplx_16d[3];

        long double _Complex        cmplx_long_double_complex_a[3];
        long double _Complex        cmplx_long_double_complex_b[3];
        long double _Complex        cmplx_long_double_complex_c[3];
        long double _Complex        cmplx_long_double_complex_d[3];

        float _Complex       cmplx_float_complex_a[3];
        float _Complex       cmplx_float_complex_b[3];
        float _Complex       cmplx_float_complex_c[3];
        float _Complex       cmplx_float_complex_d[3];

        double _Complex        cmplx_double_complex_a[3];
        double _Complex        cmplx_double_complex_b[3];
        double _Complex        cmplx_double_complex_c[3];
        double _Complex        cmplx_double_complex_d[3];


} blk_cmplx;




void csub_cmplx(){

        // variables used to get the real part of complex values from fortran
        float    r_cmplx_8a[3];
        float    r_cmplx_8b[3];
        float    r_cmplx_8c[3];
        float    r_cmplx_8d[3];

        double    r_cmplx_16a[3];
        double    r_cmplx_16b[3];
        double    r_cmplx_16c[3];
        double    r_cmplx_16d[3];

        long double    r_cmplx_long_double_complex_a[3];
        long double    r_cmplx_long_double_complex_b[3];
        long double    r_cmplx_long_double_complex_c[3];
        long double    r_cmplx_long_double_complex_d[3];

        float   r_cmplx_float_complex_a[3];
        float   r_cmplx_float_complex_b[3];
        float   r_cmplx_float_complex_c[3];
        float   r_cmplx_float_complex_d[3];

        double    r_cmplx_double_complex_a[3];
        double    r_cmplx_double_complex_b[3];
        double    r_cmplx_double_complex_c[3];
        double    r_cmplx_double_complex_d[3];

        // variables used to get the imaginary part of complex values from fortran
        float    i_cmplx_8a[3];
        float    i_cmplx_8b[3];
        float    i_cmplx_8c[3];
        float    i_cmplx_8d[3];

        double    i_cmplx_16a[3];
        double    i_cmplx_16b[3];
        double    i_cmplx_16c[3];
        double    i_cmplx_16d[3];

        long double    i_cmplx_long_double_complex_a[3];
        long double    i_cmplx_long_double_complex_b[3];
        long double    i_cmplx_long_double_complex_c[3];
        long double    i_cmplx_long_double_complex_d[3];

        float   i_cmplx_float_complex_a[3];
        float   i_cmplx_float_complex_b[3];
        float   i_cmplx_float_complex_c[3];
        float   i_cmplx_float_complex_d[3];

        double    i_cmplx_double_complex_a[3];
        double    i_cmplx_double_complex_b[3];
        double    i_cmplx_double_complex_c[3];
        double    i_cmplx_double_complex_d[3];

 	int k;

/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/


     for ( k = 0; k < 3; k++ ) {

        // get the real part of complex values from fortran
        r_cmplx_8a[k]  =  crealf( blk_cmplx.cmplx_8a[k]);
        r_cmplx_8b[k]  =  crealf( blk_cmplx.cmplx_8b[k]);
        r_cmplx_8c[k]  =  crealf( blk_cmplx.cmplx_8c[k]);
        r_cmplx_8d[k]  =  crealf( blk_cmplx.cmplx_8d[k]);
 
        r_cmplx_16a[k]  =  creal( blk_cmplx.cmplx_16a[k]);
        r_cmplx_16b[k]  =  creal( blk_cmplx.cmplx_16b[k]);
        r_cmplx_16c[k]  =  creal( blk_cmplx.cmplx_16c[k]);
        r_cmplx_16d[k]  =  creal( blk_cmplx.cmplx_16d[k]);
 
        r_cmplx_long_double_complex_a[k]  =  creall( blk_cmplx.cmplx_long_double_complex_a[k]);
        r_cmplx_long_double_complex_b[k]  =  creall( blk_cmplx.cmplx_long_double_complex_b[k]);
        r_cmplx_long_double_complex_c[k]  =  creall( blk_cmplx.cmplx_long_double_complex_c[k]);
        r_cmplx_long_double_complex_d[k]  =  creall( blk_cmplx.cmplx_long_double_complex_d[k]);
 
        r_cmplx_float_complex_a[k]  =  crealf( blk_cmplx.cmplx_float_complex_a[k]);
        r_cmplx_float_complex_b[k]  =  crealf( blk_cmplx.cmplx_float_complex_b[k]);
        r_cmplx_float_complex_c[k]  =  crealf( blk_cmplx.cmplx_float_complex_c[k]);
        r_cmplx_float_complex_d[k]  =  crealf( blk_cmplx.cmplx_float_complex_d[k]);
 
        r_cmplx_double_complex_a[k]  =  creal( blk_cmplx.cmplx_double_complex_a[k]);
        r_cmplx_double_complex_b[k]  =  creal( blk_cmplx.cmplx_double_complex_b[k]);
        r_cmplx_double_complex_c[k]  =  creal( blk_cmplx.cmplx_double_complex_c[k]);
        r_cmplx_double_complex_d[k]  =  creal( blk_cmplx.cmplx_double_complex_d[k]);
 

        // get the imaginary part of complex values from fortran
        i_cmplx_8a[k]  =  cimagf( blk_cmplx.cmplx_8a[k]);
        i_cmplx_8b[k]  =  cimagf( blk_cmplx.cmplx_8b[k]);
        i_cmplx_8c[k]  =  cimagf( blk_cmplx.cmplx_8c[k]);
        i_cmplx_8d[k]  =  cimagf( blk_cmplx.cmplx_8d[k]);
 
        i_cmplx_16a[k]  =  cimag( blk_cmplx.cmplx_16a[k]);
        i_cmplx_16b[k]  =  cimag( blk_cmplx.cmplx_16b[k]);
        i_cmplx_16c[k]  =  cimag( blk_cmplx.cmplx_16c[k]);
        i_cmplx_16d[k]  =  cimag( blk_cmplx.cmplx_16d[k]);
 
        i_cmplx_long_double_complex_a[k]  =  cimagl( blk_cmplx.cmplx_long_double_complex_a[k]);
        i_cmplx_long_double_complex_b[k]  =  cimagl( blk_cmplx.cmplx_long_double_complex_b[k]);
        i_cmplx_long_double_complex_c[k]  =  cimagl( blk_cmplx.cmplx_long_double_complex_c[k]);
        i_cmplx_long_double_complex_d[k]  =  cimagl( blk_cmplx.cmplx_long_double_complex_d[k]);
 
        i_cmplx_float_complex_a[k]  =  cimagf( blk_cmplx.cmplx_float_complex_a[k]);
        i_cmplx_float_complex_b[k]  =  cimagf( blk_cmplx.cmplx_float_complex_b[k]);
        i_cmplx_float_complex_c[k]  =  cimagf( blk_cmplx.cmplx_float_complex_c[k]);
        i_cmplx_float_complex_d[k]  =  cimagf( blk_cmplx.cmplx_float_complex_d[k]);
 
        i_cmplx_double_complex_a[k]  =  cimag( blk_cmplx.cmplx_double_complex_a[k]);
        i_cmplx_double_complex_b[k]  =  cimag( blk_cmplx.cmplx_double_complex_b[k]);
        i_cmplx_double_complex_c[k]  =  cimag( blk_cmplx.cmplx_double_complex_c[k]);
        i_cmplx_double_complex_d[k]  =  cimag( blk_cmplx.cmplx_double_complex_d[k]);
 
      }


     for ( k = 0; k < 3; k++ ) {

	// Verify REAL part of COMPLEX
        if ( precision_flt( r_cmplx_8a[k]                 ,  3.4f  )  	!=  1 ) exit (10);
        if ( precision_flt( r_cmplx_8b[k]                 ,  3.4f  )  	!=  1 ) exit (11);
        if ( precision_flt( r_cmplx_8c[k]                 , -1.17f )  	!=  1 ) exit (12);
        if ( precision_flt( r_cmplx_8d[k]                 , -3.4f  )  	!=  1 ) exit (13);

        if ( precision_dbl( r_cmplx_16a[k]                ,  1.79 )  	!=  1 ) exit (14);
        if ( precision_dbl( r_cmplx_16b[k]                ,  1.79 )  	!=  1 ) exit (15);
        if ( precision_dbl( r_cmplx_16c[k]                , -2.22 )  	!=  1 ) exit (16);
        if ( precision_dbl( r_cmplx_16d[k]                ,  1.79 )  	!=  1 ) exit (17);

        if ( precision_ldbl( r_cmplx_long_double_complex_a[k]               ,  1.797693l  )  !=  1 ) exit (18);
        if ( precision_ldbl( r_cmplx_long_double_complex_b[k]               ,  1.797693l  )  !=  1 ) exit (19);
        if ( precision_ldbl( r_cmplx_long_double_complex_c[k]               , -2.225073l  )  !=  1 ) exit (20);
        if ( precision_ldbl( r_cmplx_long_double_complex_d[k]               ,  1.797693l  )  !=  1 ) exit (21);

        if ( precision_flt( r_cmplx_float_complex_a[k]    ,  3.4028f )  	!=  1 ) exit (22);
        if ( precision_flt( r_cmplx_float_complex_b[k]    ,  3.4028f )  	!=  1 ) exit (23);
        if ( precision_flt( r_cmplx_float_complex_c[k]    , -1.1754f )  	!=  1 ) exit (24);
        if ( precision_flt( r_cmplx_float_complex_d[k]    , -3.4028f )  	!=  1 ) exit (25);

        if ( precision_ldbl( r_cmplx_double_complex_a[k]   ,  1.797693 )  	!=  1 ) exit (26);
        if ( precision_ldbl( r_cmplx_double_complex_b[k]   ,  1.797693 )  	!=  1 ) exit (27);
        if ( precision_ldbl( r_cmplx_double_complex_c[k]   , -2.225073 )  	!=  1 ) exit (28);
        if ( precision_ldbl( r_cmplx_double_complex_d[k]   ,  1.797693 )  	!=  1 ) exit (29);


	// Verify IMAGINARY part of COMPLEX
        if ( precision_flt( i_cmplx_8a[k]                 ,  3.4f   )      !=  1 ) exit (30);
        if ( precision_flt( i_cmplx_8b[k]                 , -3.4f   )      !=  1 ) exit (31);
        if ( precision_flt( i_cmplx_8c[k]                , -3.4f   )      !=  1 ) exit (32);
        if ( precision_flt( i_cmplx_8d[k]                 , -1.175f )      !=  1 ) exit (33);

        if ( precision_dbl( i_cmplx_16a[k]                ,  1.79 )        !=  1 ) exit (34);
        if ( precision_dbl( i_cmplx_16b[k]                , -1.79 )        !=  1 ) exit (35);
        if ( precision_dbl( i_cmplx_16c[k]                , -1.79 )        !=  1 ) exit (36);
        if ( precision_dbl( i_cmplx_16d[k]                , -2.22 )        !=  1 ) exit (37);

        if ( precision_ldbl( i_cmplx_long_double_complex_a[k]               ,  1.797693l  )  !=  1 ) exit (38);
        if ( precision_ldbl( i_cmplx_long_double_complex_b[k]               , -1.797693l  )  !=  1 ) exit (39);
        if ( precision_ldbl( i_cmplx_long_double_complex_c[k]               , -1.797693l  )  !=  1 ) exit (40);
        if ( precision_ldbl( i_cmplx_long_double_complex_d[k]               , -2.225073l  )  !=  1 ) exit (41);

        if ( precision_flt( i_cmplx_float_complex_a[k]    ,  3.4028f )     !=  1 ) exit (42);
        if ( precision_flt( i_cmplx_float_complex_b[k]    , -3.4028f )     !=  1 ) exit (43);
        if ( precision_flt( i_cmplx_float_complex_c[k]    , -3.4028f )     !=  1 ) exit (44);
        if ( precision_flt( i_cmplx_float_complex_d[k]    , -1.1754f )     !=  1 ) exit (45);

        if ( precision_dbl( i_cmplx_double_complex_a[k]   ,  1.797693 )    !=  1 ) exit (46);
        if ( precision_dbl( i_cmplx_double_complex_b[k]   , -1.797693 )    !=  1 ) exit (47);
        if ( precision_dbl( i_cmplx_double_complex_c[k]   , -1.797693 )    !=  1 ) exit (48);
        if ( precision_dbl( i_cmplx_double_complex_d[k]   , -2.225073 )    !=  1 ) exit (49);

      }


/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/


     for ( k = 0; k < 3; k++ ) {

        // Modify REAL part of COMPLEX 
        r_cmplx_8a[k]  =  300.119f;
        r_cmplx_8b[k]  = -300.119f;
        r_cmplx_8c[k]  =  1000.009f;
        r_cmplx_8d[k]  = -1000.009f;

        r_cmplx_16a[k]  =  1234300.11911;
        r_cmplx_16b[k]  = -1234300.11911;
        r_cmplx_16c[k]  =  12341000.00911;
        r_cmplx_16d[k]  = -12341000.00911;

        r_cmplx_long_double_complex_a[k]  =  987654321300.11998l;
        r_cmplx_long_double_complex_b[k]  = -987654321300.11998l;
        r_cmplx_long_double_complex_c[k]  =  9876543211000.00998l;
        r_cmplx_long_double_complex_d[k]  = -9876543211000.00998l;

        r_cmplx_float_complex_a[k]  =  300.119f;
        r_cmplx_float_complex_b[k]  = -300.119f;
        r_cmplx_float_complex_c[k]  =  1000.009f;
        r_cmplx_float_complex_d[k]  = -1000.009f;

        r_cmplx_double_complex_a[k]  =  1234300.11911;
        r_cmplx_double_complex_b[k]  = -1234300.11911;
        r_cmplx_double_complex_c[k]  =  12341000.00911;
        r_cmplx_double_complex_d[k]  = -12341000.00911;


        // Modify IMAGINARY part of COMPLEX
        i_cmplx_8a[k]  =  300.119f;
        i_cmplx_8b[k]  = -300.119f;
        i_cmplx_8c[k]  = -1000.009f;
        i_cmplx_8d[k]  =  1000.009f;

        i_cmplx_16a[k]  =  1234300.11911;
        i_cmplx_16b[k]  = -1234300.11911;
        i_cmplx_16c[k]  = -12341000.00911;
        i_cmplx_16d[k]  =  12341000.00911;

        i_cmplx_long_double_complex_a[k]  =  987654321300.11998l;
        i_cmplx_long_double_complex_b[k]  = -987654321300.11998l;
        i_cmplx_long_double_complex_c[k]  = -9876543211000.00998l;
        i_cmplx_long_double_complex_d[k]  =  9876543211000.00998l;

        i_cmplx_float_complex_a[k]  =  300.119f;
        i_cmplx_float_complex_b[k]  = -300.119f;
        i_cmplx_float_complex_c[k]  = -1000.009f;
        i_cmplx_float_complex_d[k]  =  1000.009f;

        i_cmplx_double_complex_a[k]  =  1234300.11911;
        i_cmplx_double_complex_b[k]  = -1234300.11911;
        i_cmplx_double_complex_c[k]  = -12341000.00911;
        i_cmplx_double_complex_d[k]  =  12341000.00911;

      }


     for ( k = 0; k < 3; k++ ) {

	// Create COMPLEX from REAL and IMAGINARY part
        blk_cmplx.cmplx_8a[k]  = createcomplexf( r_cmplx_8a[k], i_cmplx_8a[k] );
        blk_cmplx.cmplx_8b[k]  = createcomplexf( r_cmplx_8b[k], i_cmplx_8b[k] );
        blk_cmplx.cmplx_8c[k]  = createcomplexf( r_cmplx_8c[k], i_cmplx_8c[k] );
        blk_cmplx.cmplx_8d[k]  = createcomplexf( r_cmplx_8d[k], i_cmplx_8d[k] );
     
        blk_cmplx.cmplx_16a[k]  = createcomplex( r_cmplx_16a[k], i_cmplx_16a[k] );
        blk_cmplx.cmplx_16b[k]  = createcomplex( r_cmplx_16b[k], i_cmplx_16b[k] );
        blk_cmplx.cmplx_16c[k]  = createcomplex( r_cmplx_16c[k], i_cmplx_16c[k] );
        blk_cmplx.cmplx_16d[k]  = createcomplex( r_cmplx_16d[k], i_cmplx_16d[k] );
      
        blk_cmplx.cmplx_long_double_complex_a[k]  = createcomplexl( r_cmplx_long_double_complex_a[k], i_cmplx_long_double_complex_a[k] );
        blk_cmplx.cmplx_long_double_complex_b[k]  = createcomplexl( r_cmplx_long_double_complex_b[k], i_cmplx_long_double_complex_b[k] );
        blk_cmplx.cmplx_long_double_complex_c[k]  = createcomplexl( r_cmplx_long_double_complex_c[k], i_cmplx_long_double_complex_c[k] );
        blk_cmplx.cmplx_long_double_complex_d[k]  = createcomplexl( r_cmplx_long_double_complex_d[k], i_cmplx_long_double_complex_d[k] );
     
        blk_cmplx.cmplx_float_complex_a[k]  = createcomplexf( r_cmplx_float_complex_a[k], i_cmplx_float_complex_a[k] );
        blk_cmplx.cmplx_float_complex_b[k]  = createcomplexf( r_cmplx_float_complex_b[k], i_cmplx_float_complex_b[k] );
        blk_cmplx.cmplx_float_complex_c[k]  = createcomplexf( r_cmplx_float_complex_c[k], i_cmplx_float_complex_c[k] );
        blk_cmplx.cmplx_float_complex_d[k]  = createcomplexf( r_cmplx_float_complex_d[k], i_cmplx_float_complex_d[k] );
     
        blk_cmplx.cmplx_double_complex_a[k]  = createcomplex( r_cmplx_double_complex_a[k], i_cmplx_double_complex_a[k] );
        blk_cmplx.cmplx_double_complex_b[k]  = createcomplex( r_cmplx_double_complex_b[k], i_cmplx_double_complex_b[k] );
        blk_cmplx.cmplx_double_complex_c[k]  = createcomplex( r_cmplx_double_complex_c[k], i_cmplx_double_complex_c[k] );
        blk_cmplx.cmplx_double_complex_d[k]  = createcomplex( r_cmplx_double_complex_d[k], i_cmplx_double_complex_d[k] );

      }
     
//        blk_cmplx.cmplx_long_double_complex_a[k]  = createcomplexl( r_cmplx_long_double_complex_a[k],i_cmplx_long_double_complex_a[k] );
//        blk_cmplx.cmplx_long_double_complex_b[k]  = createcomplexl( r_cmplx_long_double_complex_b[k],i_cmplx_long_double_complex_b[k] );
//        blk_cmplx.cmplx_long_double_complex_c[k]  = createcomplexl( r_cmplx_long_double_complex_c[k],i_cmplx_long_double_complex_c[k] );
//        blk_cmplx.cmplx_long_double_complex_d[k]  = createcomplexl( r_cmplx_long_double_complex_d[k],i_cmplx_long_double_complex_d[k] );


/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/

     for ( k = 0; k < 3; k++ ) {

        // Verify REAL part of COMPLEX
        if ( precision_flt( r_cmplx_8a[k],     300.119f )      			!=  1 ) exit (50);
        if ( precision_flt( r_cmplx_8b[k],    -300.119f )      			!=  1 ) exit (50);
        if ( precision_flt( r_cmplx_8c[k],     1000.009f )      		!=  1 ) exit (50);
        if ( precision_flt( r_cmplx_8d[k],    -1000.009f )      		!=  1 ) exit (50);
   
        if ( precision_dbl( r_cmplx_16a[k],    1234300.11911 )    		!=  1 ) exit (51);
        if ( precision_dbl( r_cmplx_16b[k],   -1234300.11911 )      		!=  1 ) exit (51);
        if ( precision_dbl( r_cmplx_16c[k],    12341000.00911 )   		!=  1 ) exit (51);
        if ( precision_dbl( r_cmplx_16d[k],   -12341000.00911 )      		!=  1 ) exit (51);
    
        if ( precision_ldbl( r_cmplx_long_double_complex_a[k],     987654321300.11998l  )    !=  1 ) exit (52);
        if ( precision_ldbl( r_cmplx_long_double_complex_b[k],    -987654321300.11998l  )    !=  1 ) exit (52);
        if ( precision_ldbl( r_cmplx_long_double_complex_c[k],     9876543211000.00998l )    !=  1 ) exit (52);
        if ( precision_ldbl( r_cmplx_long_double_complex_d[k],    -9876543211000.00998l )    !=  1 ) exit (52);
    
        if ( precision_flt( r_cmplx_float_complex_a[k],     300.119f )     !=  1 ) exit (53);
        if ( precision_flt( r_cmplx_float_complex_b[k],    -300.119f )     !=  1 ) exit (53);
        if ( precision_flt( r_cmplx_float_complex_c[k],     1000.009f )    !=  1 ) exit (53);
        if ( precision_flt( r_cmplx_float_complex_d[k],    -1000.009f )    !=  1 ) exit (53);
    
        if ( precision_dbl( r_cmplx_double_complex_a[k],  1234300.11911l )  !=  1 ) exit (54);
        if ( precision_dbl( r_cmplx_double_complex_b[k], -1234300.11911l )  !=  1 ) exit (54);
        if ( precision_dbl( r_cmplx_double_complex_c[k],  12341000.00911l ) !=  1 ) exit (54);
        if ( precision_dbl( r_cmplx_double_complex_d[k], -12341000.00911l ) !=  1 ) exit (54);
    


        // Verify IMAGINARY part of COMPLEX
        if ( precision_flt( i_cmplx_8a[k],      300.119f )      		!=  1 ) exit (55);
        if ( precision_flt( i_cmplx_8b[k],     -300.119f )      		!=  1 ) exit (55);
        if ( precision_flt( i_cmplx_8c[k],     -1000.009f )      		!=  1 ) exit (55);
        if ( precision_flt( i_cmplx_8d[k],      1000.009f )      		!=  1 ) exit (55);
    
        if ( precision_dbl( i_cmplx_16a[k],    1234300.11911 )      		!=  1 ) exit (56);
        if ( precision_dbl( i_cmplx_16b[k],    -1234300.11911 )      		!=  1 ) exit (56);
        if ( precision_dbl( i_cmplx_16c[k],    -12341000.00911 )      		!=  1 ) exit (56);
        if ( precision_dbl( i_cmplx_16d[k],    12341000.00911 )      		!=  1 ) exit (56);
    
        if ( precision_ldbl( i_cmplx_long_double_complex_a[k],      987654321300.11998l )    !=  1 ) exit (57);
        if ( precision_ldbl( i_cmplx_long_double_complex_b[k],     -987654321300.11998l )    !=  1 ) exit (57);
        if ( precision_ldbl( i_cmplx_long_double_complex_c[k],     -9876543211000.00998l )   !=  1 ) exit (57);
        if ( precision_ldbl( i_cmplx_long_double_complex_d[k],      9876543211000.00998l )   !=  1 ) exit (57);
    
        if ( precision_flt( i_cmplx_float_complex_a[k],      300.119f )    !=  1 ) exit (58);
        if ( precision_flt( i_cmplx_float_complex_b[k],     -300.119f )    !=  1 ) exit (58);
        if ( precision_flt( i_cmplx_float_complex_c[k],     -1000.009f )   !=  1 ) exit (58);
        if ( precision_flt( i_cmplx_float_complex_d[k],      1000.009f )   !=  1 ) exit (58);
    
        if ( precision_dbl( i_cmplx_double_complex_a[k],    1234300.11911l )  !=  1 ) exit (59);
        if ( precision_dbl( i_cmplx_double_complex_b[k],    -1234300.11911l ) !=  1 ) exit (59);
        if ( precision_dbl( i_cmplx_double_complex_c[k],    -12341000.00911l ) !=  1 ) exit (59);
        if ( precision_dbl( i_cmplx_double_complex_d[k],    12341000.00911l )  !=  1 ) exit (59);
    

      }



}

