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

extern struct foo {
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

extern int fxcmn_blk700a();


int main(){

        int i,j,k;

/* --------------------------------------------------------------*
*      1) Initiaze values and pass to fortran                    *
* --------------------------------------------------------------*/

     for ( i = 0; i < 2; i++ ) {
       for ( j = 0; j < 2; j++ ) {
         for ( k = 0; k < 2; k++ ) {


         blk_all.int_c_int_least32_t[k][j][i]    = 80 ;
         blk_all.r_c_double_s8a[k][j][i]         = 79 ;
         blk_all.int_s2b[k][j][i]                = 78 ;
         blk_all.int_c_int_fast32_t[k][j][i]     = 77 ;
         blk_all.int_s4a[k][j][i]                = 76 ;
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
*  	2) verify values before passing  to fortran              *
* --------------------------------------------------------------*/
 
     for ( i = 0; i < 2; i++ ) {
       for ( j = 0; j < 2; j++ ) {
         for ( k = 0; k < 2; k++ ) {

        if (blk_all.int_c_int_least32_t[k][j][i]    != 80 )     exit (15);
        if (blk_all.r_c_double_s8a[k][j][i]         != 79 )     exit (16);
        if (blk_all.int_s2b[k][j][i]                != 78 )     exit (17);
        if (blk_all.int_c_int_fast32_t[k][j][i]     != 77 )     exit (18);
        if (blk_all.int_s4a[k][j][i]                != 76 )     exit (19);
        if (blk_all.int_c_int_fast64_t[k][j][i]     != 75 )     exit (20);
        if (blk_all.real_s16c[k][j][i]              != 74 )     exit (21);
        if (blk_all.int_s4b[k][j][i]                != 73 )     exit (22);
        if (blk_all.int_s8b[k][j][i]                != 72 )     exit (23);
        if (blk_all.int_c_signed_char[k][j][i]      != 71 )     exit (24);
        if (blk_all.int_c_short[k][j][i]            != 70 )     exit (25);
        if (blk_all.real_s8b[k][j][i]               != 69 )     exit (26);
        if (blk_all.int_c_int[k][j][i]              != 68 )     exit (27);
        if (blk_all.int_c_long_long[k][j][i]        != 67 )     exit (28);
        if (blk_all.int_c_long[k][j][i]             != 66 )     exit (29);
        if (blk_all.char_c_char[k][j][i]            != 'Z' )    exit (30);
        if (blk_all.int_c_size_t[k][j][i]           != 65 )     exit (31);
        if (blk_all.int_c_intptr_t[k][j][i]         != 64 )     exit (32);
        if (blk_all.real_s4c[k][j][i]               != 63 )     exit (33);
        if (blk_all.int_c_intmax_t[k][j][i]         != 62 )     exit (33);
        if (blk_all.int_c_int8_t[k][j][i]           != 61 )     exit (34);
        if (blk_all.int_c_int16_t[k][j][i]          != 60 )     exit (35);
        if (blk_all.int_c_int32_t[k][j][i]          != 59 )     exit (36);
        if (blk_all.r_c_float_s4a[k][j][i]          != 58 )     exit (37);
        if (blk_all.int_c_int64_t[k][j][i]          != 57 )     exit (38);
        if (blk_all.int_c_int_least8_t[k][j][i]     != 56 )     exit (39);
        if (blk_all.int_c_int_least16_t[k][j][i]    != 55 )     exit (40);
        if (blk_all.char1[k][j][i]                  != 'Z' )    exit (41);
        if (blk_all.int_c_int_least64_t[k][j][i]    != 54 )     exit (42);
        if (blk_all.log1[k][j][i]                   !=  0 )     exit (43);
        if (blk_all.int_c_int_fast8_t[k][j][i]      != 53 )     exit (44);
        if (blk_all.int_s1b[k][j][i]                != 52 )     exit (45);
        if (blk_all.int_c_int_fast16_t[k][j][i]     != 51 )     exit (46);
        if (blk_all.real_s4b[k][j][i]               != 50 )     exit (47);
	
         }
       }
     }


/* --------------------------------------------------------------*
*       3) call Fortran subroutine                               *
* --------------------------------------------------------------*/

     fxcmn_blk700a();


/* --------------------------------------------------------------*
*       4) verify values passed back from fortran                *
* --------------------------------------------------------------*/

     for ( i = 0; i < 2; i++ ) {
       for ( j = 0; j < 2; j++ ) {
         for ( k = 0; k < 2; k++ ) {

        if (blk_all.int_c_int_least32_t[k][j][i]    != 40 )     exit (85);
        if (blk_all.r_c_double_s8a[k][j][i]         != 39 )     exit (86);
        if (blk_all.int_s2b[k][j][i]                != 38 )     exit (87);
        if (blk_all.int_c_int_fast32_t[k][j][i]     != 37 )     exit (88);
        if (blk_all.int_s4a[k][j][i]                != 36 )     exit (89);
        if (blk_all.int_c_int_fast64_t[k][j][i]     != 35 )     exit (90);
        if (blk_all.real_s16c[k][j][i]              != 34 )     exit (91);
        if (blk_all.int_s4b[k][j][i]                != 33 )     exit (92);
        if (blk_all.int_s8b[k][j][i]                != 32 )     exit (93);
        if (blk_all.int_c_signed_char[k][j][i]      != 31 )     exit (94);
        if (blk_all.int_c_short[k][j][i]            != 30 )     exit (95);
        if (blk_all.real_s8b[k][j][i]               != 29 )     exit (96);
        if (blk_all.int_c_int[k][j][i]              != 28 )     exit (97);
        if (blk_all.int_c_long_long[k][j][i]        != 27 )     exit (98);
        if (blk_all.int_c_long[k][j][i]             != 26 )     exit (99);
        if (blk_all.char_c_char[k][j][i]            != 'a' )    exit (100);
        if (blk_all.int_c_size_t[k][j][i]           != 25 )     exit (101);
        if (blk_all.int_c_intptr_t[k][j][i]         != 24 )     exit (102);
        if (blk_all.real_s4c[k][j][i]               != 23 )     exit (103);
        if (blk_all.int_c_intmax_t[k][j][i]         != 22 )     exit (103);
        if (blk_all.int_c_int8_t[k][j][i]           != 21 )     exit (104);
        if (blk_all.int_c_int16_t[k][j][i]          != 20 )     exit (105);
        if (blk_all.int_c_int32_t[k][j][i]          != 19 )     exit (106);
        if (blk_all.r_c_float_s4a[k][j][i]          != 18 )     exit (107);
        if (blk_all.int_c_int64_t[k][j][i]          != 17 )     exit (108);
        if (blk_all.int_c_int_least8_t[k][j][i]     != 16 )     exit (109);
        if (blk_all.int_c_int_least16_t[k][j][i]    != 15 )     exit (110);
        if (blk_all.char1[k][j][i]                  != 'b' )    exit (111);
        if (blk_all.int_c_int_least64_t[k][j][i]    != 14 )     exit (112);
        if (blk_all.log1[k][j][i]                   != 1 )      exit (113);
        if (blk_all.int_c_int_fast8_t[k][j][i]      != 13 )     exit (114);
        if (blk_all.int_s1b[k][j][i]                != 12 )     exit (115);
        if (blk_all.int_c_int_fast16_t[k][j][i]     != 11 )     exit (116);
        if (blk_all.real_s4b[k][j][i]               != 10. )    exit (117);

         }
       }
     }

     return 0;

}

