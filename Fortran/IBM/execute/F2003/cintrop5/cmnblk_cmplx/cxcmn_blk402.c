#include "cmplx.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"


float _Complex        blk_cmplx_8a;
float _Complex        blk_cmplx_8b;
float _Complex        blk_cmplx_8c;
float _Complex        blk_cmplx_8d;

double _Complex        blk_cmplx_16a;
double _Complex        blk_cmplx_16b;
double _Complex        blk_cmplx_16c;
double _Complex        blk_cmplx_16d;

        //  c-code must be compiled with xlc -qlongdouble for complex*32
long double _Complex        blk_cmplx_32a;
long double _Complex        blk_cmplx_32b;
long double _Complex        blk_cmplx_32c;
long double _Complex        blk_cmplx_32d;

float _Complex       blk_cmplx_float_complex_a;
float _Complex       blk_cmplx_float_complex_b;
float _Complex       blk_cmplx_float_complex_c;
float _Complex       blk_cmplx_float_complex_d;

double _Complex        blk_cmplx_double_complex_a;
double _Complex        blk_cmplx_double_complex_b;
double _Complex        blk_cmplx_double_complex_c;
double _Complex        blk_cmplx_double_complex_d;




void csub_cmplx(){

        // variables used to get the real part of complex values from fortran
        float    r_cmplx_8a;
        float    r_cmplx_8b;
        float    r_cmplx_8c;
        float    r_cmplx_8d;

        double    r_cmplx_16a;
        double    r_cmplx_16b;
        double    r_cmplx_16c;
        double    r_cmplx_16d;

        long double    r_cmplx_32a;
        long double    r_cmplx_32b;
        long double    r_cmplx_32c;
        long double    r_cmplx_32d;

        float   r_cmplx_float_complex_a;
        float   r_cmplx_float_complex_b;
        float   r_cmplx_float_complex_c;
        float   r_cmplx_float_complex_d;

        double    r_cmplx_double_complex_a;
        double    r_cmplx_double_complex_b;
        double    r_cmplx_double_complex_c;
        double    r_cmplx_double_complex_d;

        // variables used to get the imaginary part of complex values from fortran
        float    i_cmplx_8a;
        float    i_cmplx_8b;
        float    i_cmplx_8c;
        float    i_cmplx_8d;

        double    i_cmplx_16a;
        double    i_cmplx_16b;
        double    i_cmplx_16c;
        double    i_cmplx_16d;

        long double    i_cmplx_32a;
        long double    i_cmplx_32b;
        long double    i_cmplx_32c;
        long double    i_cmplx_32d;

        float   i_cmplx_float_complex_a;
        float   i_cmplx_float_complex_b;
        float   i_cmplx_float_complex_c;
        float   i_cmplx_float_complex_d;

        double    i_cmplx_double_complex_a;
        double    i_cmplx_double_complex_b;
        double    i_cmplx_double_complex_c;
        double    i_cmplx_double_complex_d;


/* --------------------------------------------------------------*
*       1) verify values from fortran code                       *
* --------------------------------------------------------------*/

        // get the real part of complex values from fortran
        r_cmplx_8a  =  crealf( blk_cmplx_8a);
        r_cmplx_8b  =  crealf( blk_cmplx_8b);
        r_cmplx_8c  =  crealf( blk_cmplx_8c);
        r_cmplx_8d  =  crealf( blk_cmplx_8d);
 
        r_cmplx_16a  =  creal( blk_cmplx_16a);
        r_cmplx_16b  =  creal( blk_cmplx_16b);
        r_cmplx_16c  =  creal( blk_cmplx_16c);
        r_cmplx_16d  =  creal( blk_cmplx_16d);
 
        r_cmplx_32a  =  creall( blk_cmplx_32a);
        r_cmplx_32b  =  creall( blk_cmplx_32b);
        r_cmplx_32c  =  creall( blk_cmplx_32c);
        r_cmplx_32d  =  creall( blk_cmplx_32d);
 
        r_cmplx_float_complex_a  =  crealf( blk_cmplx_float_complex_a);
        r_cmplx_float_complex_b  =  crealf( blk_cmplx_float_complex_b);
        r_cmplx_float_complex_c  =  crealf( blk_cmplx_float_complex_c);
        r_cmplx_float_complex_d  =  crealf( blk_cmplx_float_complex_d);
 
        r_cmplx_double_complex_a  =  creal( blk_cmplx_double_complex_a);
        r_cmplx_double_complex_b  =  creal( blk_cmplx_double_complex_b);
        r_cmplx_double_complex_c  =  creal( blk_cmplx_double_complex_c);
        r_cmplx_double_complex_d  =  creal( blk_cmplx_double_complex_d);
 

        // get the imaginary part of complex values from fortran
        i_cmplx_8a  =  cimagf( blk_cmplx_8a);
        i_cmplx_8b  =  cimagf( blk_cmplx_8b);
        i_cmplx_8c  =  cimagf( blk_cmplx_8c);
        i_cmplx_8d  =  cimagf( blk_cmplx_8d);
 
        i_cmplx_16a  =  cimag( blk_cmplx_16a);
        i_cmplx_16b  =  cimag( blk_cmplx_16b);
        i_cmplx_16c  =  cimag( blk_cmplx_16c);
        i_cmplx_16d  =  cimag( blk_cmplx_16d);
 
        i_cmplx_32a  =  cimagl( blk_cmplx_32a);
        i_cmplx_32b  =  cimagl( blk_cmplx_32b);
        i_cmplx_32c  =  cimagl( blk_cmplx_32c);
        i_cmplx_32d  =  cimagl( blk_cmplx_32d);
 
        i_cmplx_float_complex_a  =  cimagf( blk_cmplx_float_complex_a);
        i_cmplx_float_complex_b  =  cimagf( blk_cmplx_float_complex_b);
        i_cmplx_float_complex_c  =  cimagf( blk_cmplx_float_complex_c);
        i_cmplx_float_complex_d  =  cimagf( blk_cmplx_float_complex_d);
 
        i_cmplx_double_complex_a  =  cimag( blk_cmplx_double_complex_a);
        i_cmplx_double_complex_b  =  cimag( blk_cmplx_double_complex_b);
        i_cmplx_double_complex_c  =  cimag( blk_cmplx_double_complex_c);
        i_cmplx_double_complex_d  =  cimag( blk_cmplx_double_complex_d);
 

	// Verify REAL part of COMPLEX
        if ( precision_flt( r_cmplx_8a                 ,  3.4f  )  	!=  1 ) exit (10);
        if ( precision_flt( r_cmplx_8b                 ,  3.4f  )  	!=  1 ) exit (11);
        if ( precision_flt( r_cmplx_8c                 , -1.17f )  	!=  1 ) exit (12);
        if ( precision_flt( r_cmplx_8d                 , -3.4f  )  	!=  1 ) exit (13);

        if ( precision_dbl( r_cmplx_16a                ,  1.79 )  	!=  1 ) exit (14);
        if ( precision_dbl( r_cmplx_16b                ,  1.79 )  	!=  1 ) exit (15);
        if ( precision_dbl( r_cmplx_16c                , -2.22 )  	!=  1 ) exit (16);
        if ( precision_dbl( r_cmplx_16d                ,  1.79 )  	!=  1 ) exit (17);

        if ( precision_ldbl( r_cmplx_32a               ,  1.797693l  )  !=  1 ) exit (18);
        if ( precision_ldbl( r_cmplx_32b               ,  1.797693l  )  !=  1 ) exit (19);
        if ( precision_ldbl( r_cmplx_32c               , -2.225073l  )  !=  1 ) exit (20);
        if ( precision_ldbl( r_cmplx_32d               ,  1.797693l  )  !=  1 ) exit (21);

        if ( precision_flt( r_cmplx_float_complex_a    ,  3.4028f )  	!=  1 ) exit (22);
        if ( precision_flt( r_cmplx_float_complex_b    ,  3.4028f )  	!=  1 ) exit (23);
        if ( precision_flt( r_cmplx_float_complex_c    , -1.1754f )  	!=  1 ) exit (24);
        if ( precision_flt( r_cmplx_float_complex_d    , -3.4028f )  	!=  1 ) exit (25);

        if ( precision_dbl( r_cmplx_double_complex_a   ,  1.797693 )  	!=  1 ) exit (26);
        if ( precision_dbl( r_cmplx_double_complex_b   ,  1.797693 )  	!=  1 ) exit (27);
        if ( precision_dbl( r_cmplx_double_complex_c   , -2.225073 )  	!=  1 ) exit (28);
        if ( precision_dbl( r_cmplx_double_complex_d   ,  1.797693 )  	!=  1 ) exit (29);


	// Verify IMAGINARY part of COMPLEX
        if ( precision_flt( i_cmplx_8a                 ,  3.4f   )      !=  1 ) exit (30);
        if ( precision_flt( i_cmplx_8b                 , -3.4f   )      !=  1 ) exit (31);
        if ( precision_flt( i_cmplx_8c                 , -3.4f   )      !=  1 ) exit (32);
        if ( precision_flt( i_cmplx_8d                 , -1.175f )      !=  1 ) exit (33);

        if ( precision_dbl( i_cmplx_16a                ,  1.79 )        !=  1 ) exit (34);
        if ( precision_dbl( i_cmplx_16b                , -1.79 )        !=  1 ) exit (35);
        if ( precision_dbl( i_cmplx_16c                , -1.79 )        !=  1 ) exit (36);
        if ( precision_dbl( i_cmplx_16d                , -2.22 )        !=  1 ) exit (37);

        if ( precision_ldbl( i_cmplx_32a               ,  1.797693l  )  !=  1 ) exit (38);
        if ( precision_ldbl( i_cmplx_32b               , -1.797693l  )  !=  1 ) exit (39);
        if ( precision_ldbl( i_cmplx_32c               , -1.797693l  )  !=  1 ) exit (40);
        if ( precision_ldbl( i_cmplx_32d               , -2.225073l  )  !=  1 ) exit (41);

        if ( precision_flt( i_cmplx_float_complex_a    ,  3.4028f )     !=  1 ) exit (42);
        if ( precision_flt( i_cmplx_float_complex_b    , -3.4028f )     !=  1 ) exit (43);
        if ( precision_flt( i_cmplx_float_complex_c    , -3.4028f )     !=  1 ) exit (44);
        if ( precision_flt( i_cmplx_float_complex_d    , -1.1754f )     !=  1 ) exit (45);

        if ( precision_dbl( i_cmplx_double_complex_a   ,  1.797693 )    !=  1 ) exit (46);
        if ( precision_dbl( i_cmplx_double_complex_b   , -1.797693 )    !=  1 ) exit (47);
        if ( precision_dbl( i_cmplx_double_complex_c   , -1.797693 )    !=  1 ) exit (48);
        if ( precision_dbl( i_cmplx_double_complex_d   , -2.225073 )    !=  1 ) exit (49);



/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

        // Modify REAL part of COMPLEX 
        r_cmplx_8a  =  300.119f;
        r_cmplx_8b  = -300.119f;
        r_cmplx_8c  =  1000.009f;
        r_cmplx_8d  = -1000.009f;

        r_cmplx_16a  =  1234300.11911;
        r_cmplx_16b  = -1234300.11911;
        r_cmplx_16c  =  12341000.00911;
        r_cmplx_16d  = -12341000.00911;

        r_cmplx_32a  =  987654321300.11998l;
        r_cmplx_32b  = -987654321300.11998l;
        r_cmplx_32c  =  9876543211000.00998l;
        r_cmplx_32d  = -9876543211000.00998l;

        r_cmplx_float_complex_a  =  300.119f;
        r_cmplx_float_complex_b  = -300.119f;
        r_cmplx_float_complex_c  =  1000.009f;
        r_cmplx_float_complex_d  = -1000.009f;

        r_cmplx_double_complex_a  =  1234300.11911;
        r_cmplx_double_complex_b  = -1234300.11911;
        r_cmplx_double_complex_c  =  12341000.00911;
        r_cmplx_double_complex_d  = -12341000.00911;


        // Modify IMAGINARY part of COMPLEX
        i_cmplx_8a  =  300.119f;
        i_cmplx_8b  = -300.119f;
        i_cmplx_8c  = -1000.009f;
        i_cmplx_8d  =  1000.009f;

        i_cmplx_16a  =  1234300.11911;
        i_cmplx_16b  = -1234300.11911;
        i_cmplx_16c  = -12341000.00911;
        i_cmplx_16d  =  12341000.00911;

        i_cmplx_32a  =  987654321300.11998l;
        i_cmplx_32b  = -987654321300.11998l;
        i_cmplx_32c  = -9876543211000.00998l;
        i_cmplx_32d  =  9876543211000.00998l;

        i_cmplx_float_complex_a  =  300.119f;
        i_cmplx_float_complex_b  = -300.119f;
        i_cmplx_float_complex_c  = -1000.009f;
        i_cmplx_float_complex_d  =  1000.009f;

        i_cmplx_double_complex_a  =  1234300.11911;
        i_cmplx_double_complex_b  = -1234300.11911;
        i_cmplx_double_complex_c  = -12341000.00911;
        i_cmplx_double_complex_d  =  12341000.00911;


	// Create COMPLEX from REAL and IMAGINARY part
        blk_cmplx_8a  = createcomplexf( r_cmplx_8a, i_cmplx_8a );
        blk_cmplx_8b  = createcomplexf( r_cmplx_8b, i_cmplx_8b );
        blk_cmplx_8c  = createcomplexf( r_cmplx_8c, i_cmplx_8c );
        blk_cmplx_8d  = createcomplexf( r_cmplx_8d, i_cmplx_8d );
     
        blk_cmplx_16a  = createcomplex( r_cmplx_16a, i_cmplx_16a );
        blk_cmplx_16b  = createcomplex( r_cmplx_16b, i_cmplx_16b );
        blk_cmplx_16c  = createcomplex( r_cmplx_16c, i_cmplx_16c );
        blk_cmplx_16d  = createcomplex( r_cmplx_16d, i_cmplx_16d );
      
        blk_cmplx_32a  = createcomplexl( r_cmplx_32a, i_cmplx_32a );
        blk_cmplx_32b  = createcomplexl( r_cmplx_32b, i_cmplx_32b );
        blk_cmplx_32c  = createcomplexl( r_cmplx_32c, i_cmplx_32c );
        blk_cmplx_32d  = createcomplexl( r_cmplx_32d, i_cmplx_32d );
     
        blk_cmplx_float_complex_a  = createcomplexf( r_cmplx_float_complex_a, i_cmplx_float_complex_a );
        blk_cmplx_float_complex_b  = createcomplexf( r_cmplx_float_complex_b, i_cmplx_float_complex_b );
        blk_cmplx_float_complex_c  = createcomplexf( r_cmplx_float_complex_c, i_cmplx_float_complex_c );
        blk_cmplx_float_complex_d  = createcomplexf( r_cmplx_float_complex_d, i_cmplx_float_complex_d );
     
        blk_cmplx_double_complex_a  = createcomplex( r_cmplx_double_complex_a, i_cmplx_double_complex_a );
        blk_cmplx_double_complex_b  = createcomplex( r_cmplx_double_complex_b, i_cmplx_double_complex_b );
        blk_cmplx_double_complex_c  = createcomplex( r_cmplx_double_complex_c, i_cmplx_double_complex_c );
        blk_cmplx_double_complex_d  = createcomplex( r_cmplx_double_complex_d, i_cmplx_double_complex_d );
     
//        blk_cmplx_long_double_complex_a  = createcomplex( r_cmplx_long_double_complex_a,i_cmplx_long_double_complex_a );
//        blk_cmplx_long_double_complex_b  = createcomplex( r_cmplx_long_double_complex_b,i_cmplx_long_double_complex_b );
//        blk_cmplx_long_double_complex_c  = createcomplex( r_cmplx_long_double_complex_c,i_cmplx_long_double_complex_c );
//        blk_cmplx_long_double_complex_d  = createcomplex( r_cmplx_long_double_complex_d,i_cmplx_long_double_complex_d );


/* --------------------------------------------------------------*
*       3) verify values before returning to fortran             *
* --------------------------------------------------------------*/

        // Verify REAL part of COMPLEX
        if ( precision_flt( r_cmplx_8a,     300.119f )      		!=  1 ) exit (50);
        if ( precision_flt( r_cmplx_8b,    -300.119f )      		!=  1 ) exit (50);
        if ( precision_flt( r_cmplx_8c,     1000.009f )      		!=  1 ) exit (50);
        if ( precision_flt( r_cmplx_8d,    -1000.009f )      		!=  1 ) exit (50);
   
        if ( precision_dbl( r_cmplx_16a,    1234300.11911 )    		!=  1 ) exit (51);
        if ( precision_dbl( r_cmplx_16b,   -1234300.11911 )      	!=  1 ) exit (51);
        if ( precision_dbl( r_cmplx_16c,    12341000.00911 )   		!=  1 ) exit (51);
        if ( precision_dbl( r_cmplx_16d,   -12341000.00911 )      	!=  1 ) exit (51);
    
        if ( precision_ldbl( r_cmplx_32a,     987654321300.11998l  )    !=  1 ) exit (52);
        if ( precision_ldbl( r_cmplx_32b,    -987654321300.11998l  )    !=  1 ) exit (52);
        if ( precision_ldbl( r_cmplx_32c,     9876543211000.00998l )    !=  1 ) exit (52);
        if ( precision_ldbl( r_cmplx_32d,    -9876543211000.00998l )    !=  1 ) exit (52);
    
        if ( precision_flt( r_cmplx_float_complex_a,     300.119f )     !=  1 ) exit (53);
        if ( precision_flt( r_cmplx_float_complex_b,    -300.119f )     !=  1 ) exit (53);
        if ( precision_flt( r_cmplx_float_complex_c,     1000.009f )    !=  1 ) exit (53);
        if ( precision_flt( r_cmplx_float_complex_d,    -1000.009f )    !=  1 ) exit (53);
    
        if ( precision_dbl( r_cmplx_double_complex_a,  1234300.11911 )  !=  1 ) exit (54);
        if ( precision_dbl( r_cmplx_double_complex_b, -1234300.11911 )  !=  1 ) exit (54);
        if ( precision_dbl( r_cmplx_double_complex_c,  12341000.00911 ) !=  1 ) exit (54);
        if ( precision_dbl( r_cmplx_double_complex_d, -12341000.00911 ) !=  1 ) exit (54);
    
//        if ( precision_dbl( r_cmplx_long_double_complex_a,   1234300.119 )      !=  1 ) exit (70);
//        if ( precision_dbl( r_cmplx_long_double_complex_b,   -1234300.119 )      !=  1 ) exit (70);
//        if ( precision_dbl( r_cmplx_long_double_complex_c,   12341000.01 )      !=  1 ) exit (70);
//        if ( precision_dbl( r_cmplx_long_double_complex_d,   -12341000.01 )      !=  1 ) exit (70);


        // Verify IMAGINARY part of COMPLEX
        if ( precision_flt( i_cmplx_8a,      300.119f )      		!=  1 ) exit (55);
        if ( precision_flt( i_cmplx_8b,     -300.119f )      		!=  1 ) exit (55);
        if ( precision_flt( i_cmplx_8c,     -1000.009f )      		!=  1 ) exit (55);
        if ( precision_flt( i_cmplx_8d,      1000.009f )      		!=  1 ) exit (55);
    
        if ( precision_dbl( i_cmplx_16a,    1234300.11911 )      		!=  1 ) exit (56);
        if ( precision_dbl( i_cmplx_16b,    -1234300.11911 )      	!=  1 ) exit (56);
        if ( precision_dbl( i_cmplx_16c,    -12341000.00911 )      	!=  1 ) exit (56);
        if ( precision_dbl( i_cmplx_16d,    12341000.00911 )      		!=  1 ) exit (56);
    
        if ( precision_ldbl( i_cmplx_32a,      987654321300.11998l )    !=  1 ) exit (57);
        if ( precision_ldbl( i_cmplx_32b,     -987654321300.11998l )    !=  1 ) exit (57);
        if ( precision_ldbl( i_cmplx_32c,     -9876543211000.00998l )   !=  1 ) exit (57);
        if ( precision_ldbl( i_cmplx_32d,      9876543211000.00998l )   !=  1 ) exit (57);
    
        if ( precision_flt( i_cmplx_float_complex_a,      300.119f )    !=  1 ) exit (58);
        if ( precision_flt( i_cmplx_float_complex_b,     -300.119f )    !=  1 ) exit (58);
        if ( precision_flt( i_cmplx_float_complex_c,     -1000.009f )   !=  1 ) exit (58);
        if ( precision_flt( i_cmplx_float_complex_d,      1000.009f )   !=  1 ) exit (58);
    
        if ( precision_dbl( i_cmplx_double_complex_a,    1234300.11911 )  !=  1 ) exit (59);
        if ( precision_dbl( i_cmplx_double_complex_b,    -1234300.11911 ) !=  1 ) exit (59);
        if ( precision_dbl( i_cmplx_double_complex_c,    -12341000.00911 ) !=  1 ) exit (59);
        if ( precision_dbl( i_cmplx_double_complex_d,    12341000.00911 )  !=  1 ) exit (59);
    
//        if ( precision_dbl( r_cmplx_long_double_complex_a,   1234300.119 )      !=  1 ) exit (70);
//        if ( precision_dbl( r_cmplx_long_double_complex_b,   -1234300.119 )      !=  1 ) exit (70);
//        if ( precision_dbl( r_cmplx_long_double_complex_c,   -12341000.01 )      !=  1 ) exit (70);
 //       if ( precision_dbl( r_cmplx_long_double_complex_d,   12341000.01 )      !=  1 ) exit (70);




}

