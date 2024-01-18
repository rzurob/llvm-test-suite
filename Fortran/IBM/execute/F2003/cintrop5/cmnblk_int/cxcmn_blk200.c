#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#if (defined(_AIX) && ! defined(_AIX52))
  #define int_fast16_t short
#endif


int i;

struct {
        signed char 		int_s1a;
        int8_t                  int_s1b;

        short                   int_s2a;
        short                   int_s2b;

        int		  	int_s4a;
        int			int_s4b;

        long long 		int_s8a;
        long long 		int_s8b;

        signed char             int_c_signed_char;
        short                   int_c_short;
        int                     int_c_int;
        long                    int_c_long;
        long long               int_c_long_long;
        size_t                  int_c_size_t;
        intptr_t                int_c_intptr_t;
        intmax_t                int_c_intmax_t;
        int8_t                  int_c_int8_t;
        int16_t                 int_c_int16_t;
        int32_t                 int_c_int32_t;
        int64_t                 int_c_int64_t;
        int_least8_t            int_c_int_least8_t;
        int_least16_t           int_c_int_least16_t;
        int_least32_t           int_c_int_least32_t;
        int_least64_t           int_c_int_least64_t;
        int_fast8_t             int_c_int_fast8_t;
        int_fast16_t            int_c_int_fast16_t;
        int_fast32_t            int_c_int_fast32_t;
        int_fast64_t            int_c_int_fast64_t;

} blk_all;



void csub_all(){


/* --------------------------------------------------------------*
*       1) Verify values from Fortran code                       *
* --------------------------------------------------------------*/


	// check integer values from fortran
     	if ( blk_all.int_s1a                    !=  127  		  )    exit (120);
        if ( blk_all.int_s1b                    != -128 		  )    exit (121);
        if ( blk_all.int_s2a                    !=  32767  		  )    exit (122);
        if ( blk_all.int_s2b                    != -32768 		  )    exit (123);
        if ( blk_all.int_s4a                    !=  2147483647 		  )    exit (124);
        if ( blk_all.int_s4b                    != -2147483648 		  )    exit (125);
        if ( blk_all.int_s8a                    !=  9223372036854775807ll )    exit (126);
        if ( blk_all.int_s8b                    != -9223372036854775807LL )    exit (127);

        if ( blk_all.int_c_signed_char          !=  25 			  )    exit (128);
        if ( blk_all.int_c_short                != -31000 		  )    exit (129);
        if ( blk_all.int_c_int                  != -2147483648 		  )    exit (130);
        if ( blk_all.int_c_long                 !=  2147483647    	  )    exit (131);        
        if ( blk_all.int_c_long_long            != -9223372036854775807LL )    exit (132);
        if ( blk_all.int_c_size_t               != -2000000000   	  )    exit (133);       
        if ( blk_all.int_c_intptr_t             !=  1000000000     	  )    exit (134);     
        if ( blk_all.int_c_intmax_t             !=  9223372036854775807LL )    exit (135);
        if ( blk_all.int_c_int8_t               !=  111 	  	  )    exit (136);
        if ( blk_all.int_c_int16_t              != -32768 	  	  )    exit (137);
        if ( blk_all.int_c_int32_t              !=  2147483647 	 	  )    exit (138);
        if ( blk_all.int_c_int64_t              !=  9223372036850123456ll )    exit (139);
        if ( blk_all.int_c_int_least8_t         != -128 		  )    exit (140);
        if ( blk_all.int_c_int_least16_t        !=  10000  		  )    exit (141);     
        if ( blk_all.int_c_int_least32_t        !=  0 		  	  )    exit (142);
        if ( blk_all.int_c_int_least64_t        !=  1111111111111111111LL )    exit (143);
        if ( blk_all.int_c_int_fast8_t          !=  1  			  )    exit (144);
        if ( blk_all.int_c_int_fast16_t         !=  64  		  )    exit (145);
        if ( blk_all.int_c_int_fast32_t         !=  1111111119 		  )    exit (146);
        if ( blk_all.int_c_int_fast64_t         !=  9223372036854775807ll )    exit (147);


/* --------------------------------------------------------------*
*      2) Modify the values and pass to Fortran                  *
* --------------------------------------------------------------*/

        blk_all.int_s1a            = -128;
        blk_all.int_s1b            =  127;
        blk_all.int_s2a            = -32768;
        blk_all.int_s2b            =  32767;
        blk_all.int_s4a            = -2147483647;
        blk_all.int_s4b            =  2147483647;
        blk_all.int_s8a            = -9223372036854775807ll;
        blk_all.int_s8b            = +9223372036854775807LL;

        blk_all.int_c_signed_char  =  127;
        blk_all.int_c_short        = +32767;
        blk_all.int_c_int          = +2147483647;
        blk_all.int_c_long         = -2147483647;
        blk_all.int_c_long_long    =  9223372036854775807LL;
        blk_all.int_c_size_t       = -2147483647;  
        blk_all.int_c_intptr_t     = -2147483647; 
        blk_all.int_c_intmax_t     =  9223372036854775807LL;
        blk_all.int_c_int8_t       = -127;          
        blk_all.int_c_int16_t      = +32767;       
        blk_all.int_c_int32_t      = -2147483647;  
        blk_all.int_c_int64_t      = -9223372036850123456ll;
        blk_all.int_c_int_least8_t       =  127;         
        blk_all.int_c_int_least16_t      = -32767;      
        blk_all.int_c_int_least32_t      =  2147483647;
        blk_all.int_c_int_least64_t      =  9223372036854775807LL;
        blk_all.int_c_int_fast8_t        = -127;      
        blk_all.int_c_int_fast16_t       =  6;      
        blk_all.int_c_int_fast32_t       = -2147483647;
        blk_all.int_c_int_fast64_t       = -9223372036854775807ll;


/* --------------------------------------------------------------*
*       3) Verify values before returning to Fortran             *
* --------------------------------------------------------------*/

        if ( blk_all.int_s1a            != -128                   )    exit (150);
        if ( blk_all.int_s1b            !=  127                   )    exit (151);
        if ( blk_all.int_s2a            != -32768                 )    exit (152);
        if ( blk_all.int_s2b            !=  32767                 )    exit (153);
        if ( blk_all.int_s4a            != -2147483647            )    exit (154);
        if ( blk_all.int_s4b            !=  2147483647            )    exit (155);
        if ( blk_all.int_s8a            != -9223372036854775807ll )    exit (156);
        if ( blk_all.int_s8b            != +9223372036854775807LL )    exit (157);

        if ( blk_all.int_c_signed_char  !=  127                   )    exit (158);
        if ( blk_all.int_c_short        != +32767                 )    exit (159);
        if ( blk_all.int_c_int          != +2147483647            )    exit (160);
        if ( blk_all.int_c_long         != -2147483647            )    exit (161);
        if ( blk_all.int_c_long_long    !=  9223372036854775807LL )    exit (162);
        if ( blk_all.int_c_size_t       != -2147483647            )    exit (163);
        if ( blk_all.int_c_intptr_t     != -2147483647            )    exit (164);
        if ( blk_all.int_c_intmax_t     !=  9223372036854775807LL )    exit (165);
        if ( blk_all.int_c_int8_t       != -127                   )    exit (166);
        if ( blk_all.int_c_int16_t      != +32767                 )    exit (167);
        if ( blk_all.int_c_int32_t      != -2147483647            )    exit (168);
        if ( blk_all.int_c_int64_t      != -9223372036850123456ll )    exit (169);
        if ( blk_all.int_c_int_least8_t       !=  127                   )    exit (170);
        if ( blk_all.int_c_int_least16_t      != -32767                 )    exit (171);
        if ( blk_all.int_c_int_least32_t      !=  2147483647            )    exit (172);
        if ( blk_all.int_c_int_least64_t      !=  9223372036854775807LL )    exit (173);
        if ( blk_all.int_c_int_fast8_t        != -127                   )    exit (174);
        if ( blk_all.int_c_int_fast16_t       !=  6                     )    exit (175);
        if ( blk_all.int_c_int_fast32_t       != -2147483647            )    exit (176);
        if ( blk_all.int_c_int_fast64_t       != -9223372036854775807ll )    exit (177);

}

