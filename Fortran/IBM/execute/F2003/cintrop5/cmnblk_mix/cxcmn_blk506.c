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
// 			Mixed data types without struct  	         //	
// --------------------------------------------------------------------- //

int_least32_t			blk_int_c_int_least32_t ;
double                		blk_r_c_double_s8a      ;
short               		blk_int_s2b            ;
int_fast32_t               	blk_int_c_int_fast32_t;
int               		blk_int_s4a           ;
int_fast64_t               	blk_int_c_int_fast64_t;
long double               	blk_real_s16c        ;
int               		blk_int_s4b        ;
long long               	blk_int_s8b       ;
signed char               	blk_int_c_signed_char      ;
short               		blk_int_c_short           ;
double               		blk_real_s8b             ;
int               		blk_int_c_int           ;
long long               	blk_int_c_long_long    ;
long               		blk_int_c_long        ;
char              		blk_char_c_char      ;
size_t               		blk_int_c_size_t    ;
intptr_t        	       	blk_int_c_intptr_t        ;
float               		blk_real_s4c              ;
intmax_t               		blk_int_c_intmax_t         ;
int8_t               		blk_int_c_int8_t         ;
int16_t               		blk_int_c_int16_t       ;
int32_t               		blk_int_c_int32_t      ;
float                		blk_r_c_float_s4a     ;
int64_t               		blk_int_c_int64_t    ;
int_least8_t               	blk_int_c_int_least8_t      ;
int_least16_t               	blk_int_c_int_least16_t    ;
char               		blk_char1                 ;
int_least64_t               	blk_int_c_int_least64_t  ;
_Bool               		blk_log1                ;
int_fast8_t               	blk_int_c_int_fast8_t  ;
signed char               	blk_int_s1b           ;
int_fast16_t               	blk_int_c_int_fast16_t ;
float                		blk_real_s4b;


void csub_all(){

/* --------------------------------------------------------------*
*  	1) verify values from fortran code                       *
* --------------------------------------------------------------*/

           if ( blk_int_c_int_least32_t    != 50 )   exit (85);
           if ( blk_r_c_double_s8a       != 51.0 )   exit (86);
           if ( blk_int_s2b                != 52 )   exit (87);
           if ( blk_int_c_int_fast32_t     != 53 )   exit (88);
           if ( blk_int_s4a                != 54 )   exit (89);
           if ( blk_int_c_int_fast64_t     != 55 )   exit (90);
           if ( blk_real_s16c           != 56.0l )   exit (91);
           if ( blk_int_s4b                != 57 )   exit (92);
           if ( blk_int_s8b                != 58 )   exit (93);
           if ( blk_int_c_signed_char      != 59 )   exit (94);
           if ( blk_int_c_short            != 60 )   exit (95);
           if ( blk_real_s8b             != 61.0 )   exit (96);
           if ( blk_int_c_int              != 62 )   exit (97);
           if ( blk_int_c_long_long        != 63 )   exit (98);
           if ( blk_int_c_long             != 64 )   exit (99);
           if ( blk_char_c_char            != 'A')   exit (100);
           if ( blk_int_c_size_t           != 65 )   exit (101);
           if ( blk_int_c_intptr_t         != 66 )   exit (102);
           if ( blk_real_s4c            != 67.0f )   exit (103);
           if ( blk_int_c_intmax_t         != 68 )   exit (104);
           if ( blk_int_c_int8_t           != 69 )   exit (105);
           if ( blk_int_c_int16_t          != 70 )   exit (106);
           if ( blk_int_c_int32_t          != 71 )   exit (107);
           if ( blk_r_c_float_s4a          != 72 )   exit (108);
           if ( blk_int_c_int64_t          != 73 )   exit (109);
           if ( blk_int_c_int_least8_t     != 74 )   exit (110);
           if ( blk_int_c_int_least16_t    != 75 )   exit (111);
           if ( blk_char1                  != 'A')   exit (112);
           if ( blk_int_c_int_least64_t    != 76 )   exit (113);
           if ( blk_log1                   != 1  )   exit (114);
           if ( blk_int_c_int_fast8_t      != 77 )   exit (115);
           if ( blk_int_s1b                != 78 )   exit (146);
           if ( blk_int_c_int_fast16_t     != 79 )   exit (117);
           if ( blk_real_s4b            != 80.0f )   exit (118);



/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

         blk_int_c_int_least32_t    = 80 ;
         blk_r_c_double_s8a         = 79 ;
         blk_int_s2b                = 78 ;
         blk_int_c_int_fast32_t     = 77 ;
         blk_int_s4a                = 76 ;
         blk_int_c_int_fast64_t     = 75 ;
         blk_real_s16c              = 74 ;
         blk_int_s4b                = 73 ;
         blk_int_s8b                = 72 ;
         blk_int_c_signed_char      = 71 ;
         blk_int_c_short            = 70 ;
         blk_real_s8b               = 69 ;
         blk_int_c_int              = 68 ;
         blk_int_c_long_long        = 67 ;
         blk_int_c_long             = 66 ;
         blk_char_c_char            = 'z';
         blk_int_c_size_t           = 65 ;
         blk_int_c_intptr_t         = 64 ;
         blk_real_s4c               = 63 ;
         blk_int_c_intmax_t         = 62 ;
         blk_int_c_int8_t           = 61 ;
         blk_int_c_int16_t          = 60 ;
         blk_int_c_int32_t          = 59 ;
         blk_r_c_float_s4a          = 58 ;
         blk_int_c_int64_t          = 57 ;
         blk_int_c_int_least8_t     = 56 ;
         blk_int_c_int_least16_t    = 55 ;
         blk_char1                  = 'Z';
         blk_int_c_int_least64_t    = 54 ;
         blk_log1                   =  0 ;
         blk_int_c_int_fast8_t      = 53 ;
         blk_int_s1b                = 52 ;
         blk_int_c_int_fast16_t     = 51 ;
         blk_real_s4b               = 50 ;



/* --------------------------------------------------------------*
*  	1) verify values before returning to fortran             *
* --------------------------------------------------------------*/

        if (blk_int_c_int_least32_t    != 80 )     exit (135);
        if (blk_r_c_double_s8a         != 79 )     exit (136);
        if (blk_int_s2b                != 78 )     exit (137);
        if (blk_int_c_int_fast32_t     != 77 )     exit (138);
        if (blk_int_s4a                != 76 )     exit (139);
        if (blk_int_c_int_fast64_t     != 75 )     exit (140);
        if (blk_real_s16c              != 74 )     exit (141);
        if (blk_int_s4b                != 73 )     exit (142);
        if (blk_int_s8b                != 72 )     exit (143);
        if (blk_int_c_signed_char      != 71 )     exit (144);
        if (blk_int_c_short            != 70 )     exit (145);
        if (blk_real_s8b               != 69 )     exit (146);
        if (blk_int_c_int              != 68 )     exit (147);
        if (blk_int_c_long_long        != 67 )     exit (148);
        if (blk_int_c_long             != 66 )     exit (149);
        if (blk_char_c_char            != 'z' )    exit (150);
        if (blk_int_c_size_t           != 65 )     exit (151);
        if (blk_int_c_intptr_t         != 64 )     exit (152);
        if (blk_real_s4c               != 63 )     exit (153);
        if (blk_int_c_intmax_t         != 62 )     exit (153);
        if (blk_int_c_int8_t           != 61 )     exit (154);
        if (blk_int_c_int16_t          != 60 )     exit (155);
        if (blk_int_c_int32_t          != 59 )     exit (156);
        if (blk_r_c_float_s4a          != 58 )     exit (157);
        if (blk_int_c_int64_t          != 57 )     exit (158);
        if (blk_int_c_int_least8_t     != 56 )     exit (159);
        if (blk_int_c_int_least16_t    != 55 )     exit (160);
        if (blk_char1                  != 'Z' )    exit (161);
        if (blk_int_c_int_least64_t    != 54 )     exit (162);
        if (blk_log1                   !=  0 )     exit (163);
        if (blk_int_c_int_fast8_t      != 53 )     exit (164);
        if (blk_int_s1b                != 52 )     exit (165);
        if (blk_int_c_int_fast16_t     != 51 )     exit (166);
        if (blk_real_s4b               != 50 )     exit (167);

}
