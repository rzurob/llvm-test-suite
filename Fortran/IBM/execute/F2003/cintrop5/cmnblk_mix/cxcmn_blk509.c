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
//                      Mixed data types without struct                  //
// --------------------------------------------------------------------- //

int_least32_t			blk_int_c_int_least32_t[3] ;
double                		blk_r_c_double_s8a[3]      ;
short               		blk_int_s2b[3]            ;
int_fast32_t               	blk_int_c_int_fast32_t[3];
int               		blk_int_s4a[3]           ;
int_fast64_t               	blk_int_c_int_fast64_t[3];
long double               	blk_real_s16c[3]        ;
int               		blk_int_s4b[3]        ;
long long               	blk_int_s8b[3]       ;
signed char               	blk_int_c_signed_char[3]      ;
short               		blk_int_c_short[3]           ;
double               		blk_real_s8b[3]             ;
int               		blk_int_c_int[3]           ;
long long               	blk_int_c_long_long[3]    ;
long               		blk_int_c_long[3]        ;
char              		blk_char_c_char[3]      ;
size_t               		blk_int_c_size_t[3]    ;
intptr_t           	    	blk_int_c_intptr_t[3]        ;
float               		blk_real_s4c[3]              ;
intmax_t          	     	blk_int_c_intmax_t[3]         ;
int8_t               		blk_int_c_int8_t[3]         ;
int16_t               		blk_int_c_int16_t[3]       ;
int32_t               		blk_int_c_int32_t[3]      ;
float                		blk_r_c_float_s4a[3]     ;
int64_t               		blk_int_c_int64_t[3]    ;
int_least8_t               	blk_int_c_int_least8_t[3]      ;
int_least16_t               	blk_int_c_int_least16_t[3]    ;
char               		blk_char1[3]                 ;
int_least64_t               	blk_int_c_int_least64_t[3]  ;
_Bool               		blk_log1[3]                ;
int_fast8_t               	blk_int_c_int_fast8_t[3]  ;
signed char               	blk_int_s1b[3]           ;
int_fast16_t               	blk_int_c_int_fast16_t[3] ;
float                		blk_real_s4b[3];


void csub_all(){

     int i;

/* --------------------------------------------------------------*
*  	1) verify values from fortran code                       *
* --------------------------------------------------------------*/

     for ( i = 0; i < 3; i++ ) {

           if ( blk_int_c_int_least32_t[i]    != 50 )   exit (85);
           if ( blk_r_c_double_s8a[i]       != 51.0 )   exit (86);
           if ( blk_int_s2b[i]                != 52 )   exit (87);
           if ( blk_int_c_int_fast32_t[i]     != 53 )   exit (88);
           if ( blk_int_s4a[i]                != 54 )   exit (89);
           if ( blk_int_c_int_fast64_t[i]     != 55 )   exit (90);
           if ( blk_real_s16c[i]           != 56.0l )   exit (91);
           if ( blk_int_s4b[i]                != 57 )   exit (92);
           if ( blk_int_s8b[i]                != 58 )   exit (93);
           if ( blk_int_c_signed_char[i]      != 59 )   exit (94);
           if ( blk_int_c_short[i]            != 60 )   exit (95);
           if ( blk_real_s8b[i]             != 61.0 )   exit (96);
           if ( blk_int_c_int[i]              != 62 )   exit (97);
           if ( blk_int_c_long_long[i]        != 63 )   exit (98);
           if ( blk_int_c_long[i]             != 64 )   exit (99);
           if ( blk_char_c_char[i]            != 'A')   exit (100);
           if ( blk_int_c_size_t[i]           != 65 )   exit (101);
           if ( blk_int_c_intptr_t[i]         != 66 )   exit (102);
           if ( blk_real_s4c[i]            != 67.0f )   exit (103);
           if ( blk_int_c_intmax_t[i]         != 68 )   exit (104);
           if ( blk_int_c_int8_t[i]           != 69 )   exit (105);
           if ( blk_int_c_int16_t[i]          != 70 )   exit (106);
           if ( blk_int_c_int32_t[i]          != 71 )   exit (107);
           if ( blk_r_c_float_s4a[i]          != 72 )   exit (108);
           if ( blk_int_c_int64_t[i]          != 73 )   exit (109);
           if ( blk_int_c_int_least8_t[i]     != 74 )   exit (110);
           if ( blk_int_c_int_least16_t[i]    != 75 )   exit (111);
           if ( blk_char1[i]                  != 'A')   exit (112);
           if ( blk_int_c_int_least64_t[i]    != 76 )   exit (113);
           if ( blk_log1[i]                   != 1  )   exit (114);
           if ( blk_int_c_int_fast8_t[i]      != 77 )   exit (115);
           if ( blk_int_s1b[i]                != 78 )   exit (146);
           if ( blk_int_c_int_fast16_t[i]     != 79 )   exit (117);
           if ( blk_real_s4b[i]            != 80.0f )   exit (118);

	}


/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

     for ( i = 0; i < 3; i++ ) {

         blk_int_c_int_least32_t[i]    = 80 ;
         blk_r_c_double_s8a[i]         = 79 ;
         blk_int_s2b[i]                = 78 ;
         blk_int_c_int_fast32_t[i]     = 77 ;
         blk_int_s4a[i]               = 76 ;
         blk_int_c_int_fast64_t[i]     = 75 ;
         blk_real_s16c[i]              = 74 ;
         blk_int_s4b[i]                = 73 ;
         blk_int_s8b[i]                = 72 ;
         blk_int_c_signed_char[i]      = 71 ;
         blk_int_c_short[i]            = 70 ;
         blk_real_s8b[i]               = 69 ;
         blk_int_c_int[i]              = 68 ;
         blk_int_c_long_long[i]        = 67 ;
         blk_int_c_long[i]             = 66 ;
         blk_char_c_char[i]            = 'Z';
         blk_int_c_size_t[i]           = 65 ;
         blk_int_c_intptr_t[i]         = 64 ;
         blk_real_s4c[i]               = 63 ;
         blk_int_c_intmax_t[i]         = 62 ;
         blk_int_c_int8_t[i]           = 61 ;
         blk_int_c_int16_t[i]          = 60 ;
         blk_int_c_int32_t[i]          = 59 ;
         blk_r_c_float_s4a[i]          = 58 ;
         blk_int_c_int64_t[i]          = 57 ;
         blk_int_c_int_least8_t[i]     = 56 ;
         blk_int_c_int_least16_t[i]    = 55 ;
         blk_char1[i]                  = 'Z';
         blk_int_c_int_least64_t[i]    = 54 ;
         blk_log1[i]                   =  0 ;
         blk_int_c_int_fast8_t[i]      = 53 ;
         blk_int_s1b[i]                = 52 ;
         blk_int_c_int_fast16_t[i]     = 51 ;
         blk_real_s4b[i]               = 50 ;
     }


/* --------------------------------------------------------------*
*  	1) verify values before returning to fortran             *
* --------------------------------------------------------------*/
 
     for ( i = 0; i < 3; i++ ) {

        if (blk_int_c_int_least32_t[i]    != 80 )     exit (135);
        if (blk_r_c_double_s8a[i]         != 79 )     exit (136);
        if (blk_int_s2b[i]                != 78 )     exit (137);
        if (blk_int_c_int_fast32_t[i]     != 77 )     exit (138);
        if (blk_int_s4a[i]                != 76 )     exit (139);
        if (blk_int_c_int_fast64_t[i]     != 75 )     exit (140);
        if (blk_real_s16c[i]              != 74 )     exit (141);
        if (blk_int_s4b[i]                != 73 )     exit (142);
        if (blk_int_s8b[i]                != 72 )     exit (143);
        if (blk_int_c_signed_char[i]      != 71 )     exit (144);
        if (blk_int_c_short[i]            != 70 )     exit (145);
        if (blk_real_s8b[i]               != 69 )     exit (146);
        if (blk_int_c_int[i]              != 68 )     exit (147);
        if (blk_int_c_long_long[i]        != 67 )     exit (148);
        if (blk_int_c_long[i]             != 66 )     exit (149);
        if (blk_char_c_char[i]            != 'Z' )    exit (150);
        if (blk_int_c_size_t[i]           != 65 )     exit (151);
        if (blk_int_c_intptr_t[i]         != 64 )     exit (152);
        if (blk_real_s4c[i]               != 63 )     exit (153);
        if (blk_int_c_intmax_t[i]         != 62 )     exit (153);
        if (blk_int_c_int8_t[i]           != 61 )     exit (154);
        if (blk_int_c_int16_t[i]          != 60 )     exit (155);
        if (blk_int_c_int32_t[i]          != 59 )     exit (156);
        if (blk_r_c_float_s4a[i]          != 58 )     exit (157);
        if (blk_int_c_int64_t[i]          != 57 )     exit (158);
        if (blk_int_c_int_least8_t[i]     != 56 )     exit (159);
        if (blk_int_c_int_least16_t[i]    != 55 )     exit (160);
        if (blk_char1[i]                  != 'Z' )    exit (161);
        if (blk_int_c_int_least64_t[i]    != 54 )     exit (162);
        if (blk_log1[i]                   !=  0 )     exit (163);
        if (blk_int_c_int_fast8_t[i]      != 53 )     exit (164);
        if (blk_int_s1b[i]                != 52 )     exit (165);
        if (blk_int_c_int_fast16_t[i]     != 51 )     exit (166);
        if (blk_real_s4b[i]               != 50 )     exit (167);
	
     }

}
