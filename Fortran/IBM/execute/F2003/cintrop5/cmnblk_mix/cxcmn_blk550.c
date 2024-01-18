#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include "precision_r.inc"

#if (defined(_AIX) && ! defined(_AIX52))
  #define int_fast16_t short
#endif


// --------------------------------------------------------------------- //
// 			Mixed data types in one struct  	          //	
// --------------------------------------------------------------------- //

struct foo {
         int_least32_t			int_c_int_least32_t[2][2][2] ;
         double                		r_c_double_s8a[2][2][2]      ;
         short               		int_s2b[2][2][2]            ;
         int_fast32_t               	int_c_int_fast32_t[2][2][2];
         int               		int_s4a[2][2][2]           ;
         int_fast64_t               	int_c_int_fast64_t[2][2][2];
         long double               	real_s16c[2][2][2]        ;
         int               		int_s4b[2][2][2]        ;
         long long               	int_s8b[2][2][2]       ;
         signed char               	int_c_signed_char[2][2][2]      ;
         short               		int_c_short[2][2][2]           ;
         double               		real_s8b[2][2][2]             ;
         int               		int_c_int[2][2][2]           ;
         long long               	int_c_long_long[2][2][2]    ;
         long               		int_c_long[2][2][2]        ;
         char              		char_c_char[2][2][2]      ;
         size_t               		int_c_size_t[2][2][2]    ;
         intptr_t               	int_c_intptr_t[2][2][2]        ;
         float               		real_s4c[2][2][2]              ;
         intmax_t               	int_c_intmax_t[2][2][2]         ;
         int8_t               		int_c_int8_t[2][2][2]         ;
         int16_t               		int_c_int16_t[2][2][2]       ;
         int32_t               		int_c_int32_t[2][2][2]      ;
         float                		r_c_float_s4a[2][2][2]     ;
         int64_t               		int_c_int64_t[2][2][2]    ;
         int_least8_t               	int_c_int_least8_t[2][2][2]      ;
         int_least16_t               	int_c_int_least16_t[2][2][2]    ;
         char               		char1[2][2][2]                 ;
         int_least64_t               	int_c_int_least64_t[2][2][2]  ;
         _Bool               		log1[2][2][2]                ;
         int_fast8_t               	int_c_int_fast8_t[2][2][2]  ;
         signed char               	int_s1b[2][2][2]           ;
         int_fast16_t               	int_c_int_fast16_t[2][2][2] ;
         float                		real_s4b[2][2][2];
} blk_all;


void csub_all(){

        int i,j,k;

/* --------------------------------------------------------------*
*  	1) verify values from fortran code                       *
* --------------------------------------------------------------*/

     for ( i = 0; i < 2; i++ ) {
       for ( j = 0; j < 2; j++ ) {
         for ( k = 0; k < 2; k++ ) {


           if ( blk_all.int_c_int_least32_t[k][j][i]    != 50 )   exit (85);
           if ( blk_all.r_c_double_s8a[k][j][i]       != 51.0 )   exit (86);
           if ( blk_all.int_s2b[k][j][i]                != 52 )   exit (87);
           if ( blk_all.int_c_int_fast32_t[k][j][i]     != 53 )   exit (88);
           if ( blk_all.int_s4a[k][j][i]                != 54 )   exit (89);
           if ( blk_all.int_c_int_fast64_t[k][j][i]     != 55 )   exit (90);
           if ( blk_all.real_s16c[k][j][i]           != 56.0l )   exit (91);
           if ( blk_all.int_s4b[k][j][i]                != 57 )   exit (92);
           if ( blk_all.int_s8b[k][j][i]                != 58 )   exit (93);
           if ( blk_all.int_c_signed_char[k][j][i]      != 59 )   exit (94);
           if ( blk_all.int_c_short[k][j][i]            != 60 )   exit (95);
           if ( blk_all.real_s8b[k][j][i]             != 61.0 )   exit (96);
           if ( blk_all.int_c_int[k][j][i]              != 62 )   exit (97);
           if ( blk_all.int_c_long_long[k][j][i]        != 63 )   exit (98);
           if ( blk_all.int_c_long[k][j][i]             != 64 )   exit (99);
           if ( blk_all.char_c_char[k][j][i]            != 'A')   exit (100);
           if ( blk_all.int_c_size_t[k][j][i]           != 65 )   exit (101);
           if ( blk_all.int_c_intptr_t[k][j][i]         != 66 )   exit (102);
           if ( blk_all.real_s4c[k][j][i]            != 67.0f )   exit (103);
           if ( blk_all.int_c_intmax_t[k][j][i]         != 68 )   exit (104);
           if ( blk_all.int_c_int8_t[k][j][i]           != 69 )   exit (105);
           if ( blk_all.int_c_int16_t[k][j][i]          != 70 )   exit (106);
           if ( blk_all.int_c_int32_t[k][j][i]          != 71 )   exit (107);
           if ( blk_all.r_c_float_s4a[k][j][i]          != 72 )   exit (108);
           if ( blk_all.int_c_int64_t[k][j][i]          != 73 )   exit (109);
           if ( blk_all.int_c_int_least8_t[k][j][i]     != 74 )   exit (110);
           if ( blk_all.int_c_int_least16_t[k][j][i]    != 75 )   exit (111);
           if ( blk_all.char1[k][j][i]                  != 'A')   exit (112);
           if ( blk_all.int_c_int_least64_t[k][j][i]    != 76 )   exit (113);
           if ( blk_all.log1[k][j][i]                   != 1  )   exit (114);
           if ( blk_all.int_c_int_fast8_t[k][j][i]      != 77 )   exit (115);
           if ( blk_all.int_s1b[k][j][i]                != 78 )   exit (146);
           if ( blk_all.int_c_int_fast16_t[k][j][i]     != 79 )   exit (117);
           if ( blk_all.real_s4b[k][j][i]            != 80.0f )   exit (118);

         }
       }
     }



/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

     for ( i = 0; i < 2; i++ ) {
       for ( j = 0; j < 2; j++ ) {
         for ( k = 0; k < 2; k++ ) {


         blk_all.int_c_int_least32_t[k][j][i]    = 80 ;
         blk_all.r_c_double_s8a[k][j][i]         = 79 ;
         blk_all.int_s2b[k][j][i]                = 78 ;
         blk_all.int_c_int_fast32_t[k][j][i]     = 77 ;
         blk_all.int_s4a[k][j][i]               = 76 ;
         blk_all.int_c_int_fast64_t[k][j][i]     = 75 ;
         blk_all.real_s16c[k][j][i]              = 74 ;
         blk_all.int_s4b[k][j][i]                = 73 ;
         blk_all.int_s8b[k][j][i]                = 72 ;
         blk_all.int_c_signed_char[k][j][i]      = 71 ;
         blk_all.int_c_short[k][j][i]            = 70 ;
         blk_all.real_s8b[k][j][i]               = 69 ;
         blk_all.int_c_int[k][j][i]              = 68 ;
         blk_all.int_c_long_long[k][j][i]        = 67 ;
         blk_all.int_c_long[k][j][i]             = 66 ;
         blk_all.char_c_char[k][j][i]            = 'Z';
         blk_all.int_c_size_t[k][j][i]           = 65 ;
         blk_all.int_c_intptr_t[k][j][i]         = 64 ;
         blk_all.real_s4c[k][j][i]               = 63 ;
         blk_all.int_c_intmax_t[k][j][i]         = 62 ;
         blk_all.int_c_int8_t[k][j][i]           = 61 ;
         blk_all.int_c_int16_t[k][j][i]          = 60 ;
         blk_all.int_c_int32_t[k][j][i]          = 59 ;
         blk_all.r_c_float_s4a[k][j][i]          = 58 ;
         blk_all.int_c_int64_t[k][j][i]          = 57 ;
         blk_all.int_c_int_least8_t[k][j][i]     = 56 ;
         blk_all.int_c_int_least16_t[k][j][i]    = 55 ;
         blk_all.char1[k][j][i]                  = 'Z';
         blk_all.int_c_int_least64_t[k][j][i]    = 54 ;
         blk_all.log1[k][j][i]                   =  0 ;
         blk_all.int_c_int_fast8_t[k][j][i]      = 53 ;
         blk_all.int_s1b[k][j][i]                = 52 ;
         blk_all.int_c_int_fast16_t[k][j][i]     = 51 ;
         blk_all.real_s4b[k][j][i]               = 50 ;

         }
       }
     }



/* --------------------------------------------------------------*
*  	1) verify values before returning to fortran             *
* --------------------------------------------------------------*/
 
     for ( i = 0; i < 2; i++ ) {
       for ( j = 0; j < 2; j++ ) {
         for ( k = 0; k < 2; k++ ) {

        if (blk_all.int_c_int_least32_t[k][j][i]    != 80 )     exit (135);
        if (blk_all.r_c_double_s8a[k][j][i]         != 79 )     exit (136);
        if (blk_all.int_s2b[k][j][i]                != 78 )     exit (137);
        if (blk_all.int_c_int_fast32_t[k][j][i]     != 77 )     exit (138);
        if (blk_all.int_s4a[k][j][i]                != 76 )     exit (139);
        if (blk_all.int_c_int_fast64_t[k][j][i]     != 75 )     exit (140);
        if (blk_all.real_s16c[k][j][i]              != 74 )     exit (141);
        if (blk_all.int_s4b[k][j][i]                != 73 )     exit (142);
        if (blk_all.int_s8b[k][j][i]                != 72 )     exit (143);
        if (blk_all.int_c_signed_char[k][j][i]      != 71 )     exit (144);
        if (blk_all.int_c_short[k][j][i]            != 70 )     exit (145);
        if (blk_all.real_s8b[k][j][i]               != 69 )     exit (146);
        if (blk_all.int_c_int[k][j][i]              != 68 )     exit (147);
        if (blk_all.int_c_long_long[k][j][i]        != 67 )     exit (148);
        if (blk_all.int_c_long[k][j][i]             != 66 )     exit (149);
        if (blk_all.char_c_char[k][j][i]            != 'Z' )    exit (150);
        if (blk_all.int_c_size_t[k][j][i]           != 65 )     exit (151);
        if (blk_all.int_c_intptr_t[k][j][i]         != 64 )     exit (152);
        if (blk_all.real_s4c[k][j][i]               != 63 )     exit (153);
        if (blk_all.int_c_intmax_t[k][j][i]         != 62 )     exit (153);
        if (blk_all.int_c_int8_t[k][j][i]           != 61 )     exit (154);
        if (blk_all.int_c_int16_t[k][j][i]          != 60 )     exit (155);
        if (blk_all.int_c_int32_t[k][j][i]          != 59 )     exit (156);
        if (blk_all.r_c_float_s4a[k][j][i]          != 58 )     exit (157);
        if (blk_all.int_c_int64_t[k][j][i]          != 57 )     exit (158);
        if (blk_all.int_c_int_least8_t[k][j][i]     != 56 )     exit (159);
        if (blk_all.int_c_int_least16_t[k][j][i]    != 55 )     exit (160);
        if (blk_all.char1[k][j][i]                  != 'Z' )    exit (161);
        if (blk_all.int_c_int_least64_t[k][j][i]    != 54 )     exit (162);
        if (blk_all.log1[k][j][i]                   !=  0 )     exit (163);
        if (blk_all.int_c_int_fast8_t[k][j][i]      != 53 )     exit (164);
        if (blk_all.int_s1b[k][j][i]                != 52 )     exit (165);
        if (blk_all.int_c_int_fast16_t[k][j][i]     != 51 )     exit (166);
        if (blk_all.real_s4b[k][j][i]               != 50 )     exit (167);
	

         }
       }
     }

}

