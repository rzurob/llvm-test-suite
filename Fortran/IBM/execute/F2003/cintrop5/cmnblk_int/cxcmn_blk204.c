#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>


signed char 		blk_int_s1a[5];
int8_t                  blk_int_s1b[5];

short                   blk_int_s2a[5];
short                   blk_int_s2b[5];

int		  	blk_int_s4a[5];
int			blk_int_s4b[5];

long long 		blk_int_s8a[5];
long long 		blk_int_s8b[5];

signed char             blk_int_c_signed_char[5];
short                   blk_int_c_short[5];
int                     blk_int_c_int[5];
long                    blk_int_c_long[5];
long long               blk_int_c_long_long[5];
size_t                  blk_int_c_size_t[5];
intptr_t                blk_int_c_intptr_t[5];
intmax_t                blk_int_c_intmax_t[5];
int8_t                  blk_int_c_int8_t[5];
int16_t                 blk_int_c_int16_t[5];
int32_t                 blk_int_c_int32_t[5];
int64_t                 blk_int_c_int64_t[5];
int_least8_t            blk_int_c_int_least8_t[5];
int_least16_t           blk_int_c_int_least16_t[5];
int_least32_t           blk_int_c_int_least32_t[5];
int_least64_t           blk_int_c_int_least64_t[5];
int_fast8_t             blk_int_c_int_fast8_t[5];
// 	int_fast16_t            blk_int_c_int_fast16_t[5];
int_fast32_t            blk_int_c_int_fast32_t[5];
int_fast64_t            blk_int_c_int_fast64_t[5];

int i;

void csub_all(){


/* --------------------------------------------------------------*
*       1) Verify values from Fortran code                       *
* --------------------------------------------------------------*/

        if (blk_int_s1a[0]         !=  127                    )    exit (210);
        if (blk_int_s1a[1]         !=  0                      )    exit (210);
        if (blk_int_s1a[2]         !=  -128                   )    exit (210);
        if (blk_int_s1a[3]         !=  127                    )    exit (210);
        if (blk_int_s1a[4]         !=  127                    )    exit (210);

        for ( i = 0; i < 5; i++ ) {
          if ( blk_int_s1b[i]      != -128                    )    exit (211);
        }

        if (blk_int_s2a[0]         !=  32767                  )    exit (212);
        if (blk_int_s2a[1]         !=  0                      )    exit (212);
        if (blk_int_s2a[2]         !=  -32768                 )    exit (212);
        if (blk_int_s2a[3]         !=  32767                  )    exit (212);
        if (blk_int_s2a[4]         !=  127                    )    exit (212);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_s2b[i]     != -32768                  )    exit (213);
        }

        if (blk_int_s4a[0]         !=  2147483647             )    exit (214);
        if (blk_int_s4a[1]         !=  127                    )    exit (214);
        if (blk_int_s4a[2]         != -2147483648             )    exit (214);
        if (blk_int_s4a[3]         !=  0                      )    exit (214);
        if (blk_int_s4a[4]         !=  1000000                )    exit (214);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_s4b[i]   != -2147483648             )    exit (215);
	}

        if (blk_int_s8a[0]         !=  9223372036854775807LL  )    exit (216);
        if (blk_int_s8a[1]         !=  0                      )    exit (216);
        if (blk_int_s8a[2]         != -9223372036854775807LL  )    exit (216);
        if (blk_int_s8a[3]         !=  1000000                )    exit (216);
        if (blk_int_s8a[4]         != -2147483648LL           )    exit (216);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_s8b[i]   != -9223372036854775807LL  )    exit (213);
	}

        if (blk_int_c_signed_char[0] !=  127                    )    exit (217);
        if (blk_int_c_signed_char[1] !=  0                      )    exit (217);
        if (blk_int_c_signed_char[2] != -128                    )    exit (217);
        if (blk_int_c_signed_char[3] !=  127                    )    exit (217);
        if (blk_int_c_signed_char[4] !=  127                    )    exit (217);

        if (blk_int_c_short[0]     !=  32767                  )    exit (218);
        if (blk_int_c_short[1]     !=  0                      )    exit (218);
        if (blk_int_c_short[2]     != -32768                  )    exit (218);
        if (blk_int_c_short[3]     !=  32767                  )    exit (218);
        if (blk_int_c_short[4]     !=  127            	  )    exit (218);

        if (blk_int_c_int[0]       !=  2147483647             )    exit (219);
        if (blk_int_c_int[1]       !=  127            	  )    exit (219);
        if (blk_int_c_int[2]       != -2147483648             )    exit (219);
        if (blk_int_c_int[3]       !=  0                      )    exit (219);
        if (blk_int_c_int[4]       !=  1000000                )    exit (219);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_long[i]      !=  2147483647         )   exit (229);
	}

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_long_long[i] != -9223372036854775807LL )   exit (220);
	}

printf(" %d \n", blk_int_c_size_t[2]);
        if (blk_int_c_size_t[0]            !=  2147483647     )    exit (221);
        if (blk_int_c_size_t[1]            !=  127            )    exit (221);
//        if (blk_int_c_size_t[2]            != -2147483648     )    exit (221);
        if (blk_int_c_size_t[3]            !=  0              )    exit (221);
        if (blk_int_c_size_t[4]            !=  1000000        )    exit (221);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_intptr_t[i]  !=  1000000000                )   exit (222);
	}

        if (blk_int_c_intmax_t[0]          !=  9223372036854775807LL )    exit (223);
        if (blk_int_c_intmax_t[1]          !=  0          		 )    exit (223);
        if (blk_int_c_intmax_t[2]          != -9223372036854775807LL )    exit (223);
        if (blk_int_c_intmax_t[3]          !=  1000000               )    exit (223);
        if (blk_int_c_intmax_t[4]          != -2147483648LL          )    exit (223);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int8_t[i]    != -128                       )   exit (224);
	}

        if (blk_int_c_int16_t[0]           !=  32767                 )    exit (225);
        if (blk_int_c_int16_t[1]           !=  0                     )    exit (225);
        if (blk_int_c_int16_t[2]           != -32768                 )    exit (225);
        if (blk_int_c_int16_t[3]           !=  32767                 )    exit (225);
        if (blk_int_c_int16_t[4]           !=  127                   )    exit (225);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int32_t[i]   !=  2147483647                )   exit (226);
	}

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int64_t[i]   !=  9223372036850123456LL     )   exit (227);
	}

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int_least8_t[i] != -128                    )   exit (228);
	}

        if (blk_int_c_int_least16_t[0]     != 32767                  )    exit (229);
        if (blk_int_c_int_least16_t[1]     != 0        	         )    exit (229); 
        if (blk_int_c_int_least16_t[2]     != -32768                 )    exit (229);
        if (blk_int_c_int_least16_t[3]     != 32767                  )    exit (229);
        if (blk_int_c_int_least16_t[4]     != 127                    )    exit (229); 

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int_least32_t[i] !=  0                     )    exit (230);
	}

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int_least64_t[i] != 1111111111111111111LL   ) exit (231);
	}

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int_fast8_t[i]   != 1              	  )    exit (232);
	}

        if (blk_int_c_int_fast32_t[0]      !=  2147483647             )    exit (233);
        if (blk_int_c_int_fast32_t[1]      !=  127              	  )    exit (233);
        if (blk_int_c_int_fast32_t[2]      != -2147483648             )    exit (233);
        if (blk_int_c_int_fast32_t[3]      !=  0                      )    exit (233);
        if (blk_int_c_int_fast32_t[4]      !=  1000000                )    exit (233);

        if (blk_int_c_int_fast64_t[0]      !=  9223372036854775807LL  )    exit (234);
        if (blk_int_c_int_fast64_t[1]      !=  0            	  )    exit (234);
        if (blk_int_c_int_fast64_t[2]      != -9223372036854775807LL  )    exit (234);
        if (blk_int_c_int_fast64_t[3]      !=  1000000                )    exit (234);
        if (blk_int_c_int_fast64_t[4]      != -2147483648LL           )    exit (234);


/* --------------------------------------------------------------*
*      2) Modify the values and pass to Fortran                  *
* --------------------------------------------------------------*/
        blk_int_s1a[4]         =  127;
        blk_int_s1a[3]         =  0;
        blk_int_s1a[2]         =  -128;
        blk_int_s1a[1]         =  127 ;
        blk_int_s1a[0]         =  127 ;

        for ( i = 0; i < 5; i++ ) {
           blk_int_s1b[i]      = 127 ;
        }

        blk_int_s2a[4]         =  32767 ;
        blk_int_s2a[3]         =  0    ;
        blk_int_s2a[2]         =  -32768;
        blk_int_s2a[1]         =  32767;
        blk_int_s2a[0]         =  127 ;

        for ( i = 0; i < 5; i++ ) {
            blk_int_s2b[i]     = 32767 ;
        }

        blk_int_s4a[4]         =  2147483647 ;
        blk_int_s4a[3]         =  127         ;
        blk_int_s4a[2]         = -2147483648 ;
        blk_int_s4a[1]         =  0         ;
        blk_int_s4a[0]         =  1000000  ;

        for ( i = 0; i < 5; i++ ) {
            blk_int_s4b[i]     = 2147483647;
        }

        blk_int_s8a[4]         =  9223372036854775807LL  ;
        blk_int_s8a[3]         =  0                     ;
        blk_int_s8a[2]         = -9223372036854775807LL;
        blk_int_s8a[1]         =  1000000             ;
        blk_int_s8a[0]         = -2147483648LL       ;

        for ( i = 0; i < 5; i++ ) {
            blk_int_s8b[i]     = 9223372036854775807LL ;
        }

        blk_int_c_signed_char[4] =  127 ;
        blk_int_c_signed_char[3] =  0  ;
        blk_int_c_signed_char[2] = -128;
        blk_int_c_signed_char[1] =  127;
        blk_int_c_signed_char[0] =  127;

        blk_int_c_short[4]     =  32767;
        blk_int_c_short[3]     =  0   ;
        blk_int_c_short[2]     = -32768;
        blk_int_c_short[1]     =  32767 ;
        blk_int_c_short[0]     =  127  ;

        blk_int_c_int[4]       =  2147483647 ;
        blk_int_c_int[3]       =  127       ;
        blk_int_c_int[2]       = -2147483648 ;
        blk_int_c_int[1]       =  0         ;
        blk_int_c_int[0]       =  1000000  ;

        for ( i = 0; i < 5; i++ ) {
            blk_int_c_long[i]      =  -2147483648 ;
        }

        for ( i = 0; i < 5; i++ ) {
            blk_int_c_long_long[i] = -9223372036854775807LL ;
        }

        blk_int_c_size_t[4]            =  2147483647 ;
        blk_int_c_size_t[3]            =  127        ;
        blk_int_c_size_t[2]            = -2147483648  ;
        blk_int_c_size_t[1]            =  0           ;
        blk_int_c_size_t[0]            =  1000000     ;

        for ( i = 0; i < 5; i++ ) {
            blk_int_c_intptr_t[i]  =  -1000000000 ;
        }

        blk_int_c_intmax_t[4]          =  9223372036854775807LL ;
        blk_int_c_intmax_t[3]          =  0                    ;
        blk_int_c_intmax_t[2]          = -9223372036854775807LL;
        blk_int_c_intmax_t[1]          =  1000000             ;
        blk_int_c_intmax_t[0]          = -2147483648LL       ;

        for ( i = 0; i < 5; i++ ) {
            blk_int_c_int8_t[i]    = 127 ;
        }

        blk_int_c_int16_t[4]           =  32767 ;
        blk_int_c_int16_t[3]           =  0      ;
        blk_int_c_int16_t[2]           = -32768   ;
        blk_int_c_int16_t[1]           =  32767  ;
        blk_int_c_int16_t[0]           =  127   ;

        for ( i = 0; i < 5; i++ ) {
            blk_int_c_int32_t[i]   =  -2147483648 ;
        }

        for ( i = 0; i < 5; i++ ) {
            blk_int_c_int64_t[i]   =  -9223372036850123456LL ;
        }

        for ( i = 0; i < 5; i++ ) {
            blk_int_c_int_least8_t[i] =  127 ;
        }

        blk_int_c_int_least16_t[4]     = 32767 ;
        blk_int_c_int_least16_t[3]     = 0     ;
        blk_int_c_int_least16_t[2]     = -32768 ;
        blk_int_c_int_least16_t[1]     = 32767 ;
        blk_int_c_int_least16_t[0]     = 127  ;

        for ( i = 0; i < 5; i++ ) {
            blk_int_c_int_least32_t[i] =  2147483647 ;
        }

        for ( i = 0; i < 5; i++ ) {
            blk_int_c_int_least64_t[i] = -111111111111111111LL ;
        }

        for ( i = 0; i < 5; i++ ) {
            blk_int_c_int_fast8_t[i]   = -127 ;
        }

        blk_int_c_int_fast32_t[4]      =  2147483647 ;
        blk_int_c_int_fast32_t[3]      =  127        ;
        blk_int_c_int_fast32_t[2]      = -2147483648 ;
        blk_int_c_int_fast32_t[1]      =  0          ;
        blk_int_c_int_fast32_t[0]      =  1000000    ;

        blk_int_c_int_fast64_t[4]      =  9223372036854775807LL ;
        blk_int_c_int_fast64_t[3]      =  0                   ;
        blk_int_c_int_fast64_t[2]      = -9223372036854775807LL;
        blk_int_c_int_fast64_t[1]      =  1000000            ;
        blk_int_c_int_fast64_t[0]      = -2147483648LL       ;


/* --------------------------------------------------------------*
*       3) Verify values before returning to Fortran             *
* --------------------------------------------------------------*/

        if (blk_int_s1a[4]         !=  127                    )    exit (240);
        if (blk_int_s1a[3]         !=  0                      )    exit (240);
        if (blk_int_s1a[2]         !=  -128                   )    exit (240);
        if (blk_int_s1a[1]         !=  127                    )    exit (240);
        if (blk_int_s1a[0]         !=  127                    )    exit (240);

        for ( i = 0; i < 5; i++ ) {
          if ( blk_int_s1b[i]      != 127                     )    exit (241);
        }

        if (blk_int_s2a[4]         !=  32767                  )    exit (242);
        if (blk_int_s2a[3]         !=  0                      )    exit (242);
        if (blk_int_s2a[2]         !=  -32768                 )    exit (242);
        if (blk_int_s2a[1]         !=  32767                  )    exit (242);
        if (blk_int_s2a[0]         !=  127                    )    exit (242);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_s2b[i]     != 32767                  )    exit (243);
        }

        if (blk_int_s4a[4]         !=  2147483647             )    exit (244);
        if (blk_int_s4a[3]         !=  127                    )    exit (244);
        if (blk_int_s4a[2]         != -2147483648             )    exit (244);
        if (blk_int_s4a[1]         !=  0                      )    exit (244);
        if (blk_int_s4a[0]         !=  1000000                )    exit (244);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_s4b[i]     != 2147483647             )    exit (245);
        }

        if (blk_int_s8a[4]         !=  9223372036854775807LL  )    exit (246);
        if (blk_int_s8a[3]         !=  0                      )    exit (246);
        if (blk_int_s8a[2]         != -9223372036854775807LL  )    exit (246);
        if (blk_int_s8a[1]         !=  1000000                )    exit (246);
        if (blk_int_s8a[0]         != -2147483648LL           )    exit (246);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_s8b[i]     != 9223372036854775807LL  )    exit (247);
        }

        if (blk_int_c_signed_char[4] !=  127                    )    exit (248);
        if (blk_int_c_signed_char[3] !=  0                      )    exit (248);
        if (blk_int_c_signed_char[2] != -128                    )    exit (248);
        if (blk_int_c_signed_char[1] !=  127                    )    exit (248);
        if (blk_int_c_signed_char[0] !=  127                    )    exit (248);

        if (blk_int_c_short[4]     !=  32767                  )    exit (249);
        if (blk_int_c_short[3]     !=  0                      )    exit (249);
        if (blk_int_c_short[2]     != -32768                  )    exit (249);
        if (blk_int_c_short[1]     !=  32767                  )    exit (249);
        if (blk_int_c_short[0]     !=  127                    )    exit (249);

        if (blk_int_c_int[4]       !=  2147483647             )    exit (250);
        if (blk_int_c_int[3]       !=  127                    )    exit (250);
        if (blk_int_c_int[2]       != -2147483648             )    exit (250);
        if (blk_int_c_int[1]       !=  0                      )    exit (250);
        if (blk_int_c_int[0]       !=  1000000                )    exit (250);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_long[i]      !=  -2147483648         )   exit (251);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_long_long[i] != -9223372036854775807LL )   exit (252);
        }

printf(" %d \n", blk_int_c_size_t[2]);
        if (blk_int_c_size_t[4]            !=  2147483647     )    exit (253);
        if (blk_int_c_size_t[3]            !=  127            )    exit (253);
//        if (blk_int_c_size_t[2]            != -2147483648     )    exit (253);
        if (blk_int_c_size_t[1]            !=  0              )    exit (253);
        if (blk_int_c_size_t[0]            !=  1000000        )    exit (253);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_intptr_t[i]  !=  -1000000000               )   exit (254);
        }

        if (blk_int_c_intmax_t[4]          !=  9223372036854775807LL )    exit (255);
        if (blk_int_c_intmax_t[3]          !=  0                     )    exit (255);
        if (blk_int_c_intmax_t[2]          != -9223372036854775807LL )    exit (255);
        if (blk_int_c_intmax_t[1]          !=  1000000               )    exit (265);
        if (blk_int_c_intmax_t[0]          != -2147483648LL          )    exit (265);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int8_t[i]    != 127                       )   exit (256);
        }

        if (blk_int_c_int16_t[4]           !=  32767                 )    exit (257);
        if (blk_int_c_int16_t[3]           !=  0                     )    exit (257);
        if (blk_int_c_int16_t[2]           != -32768                 )    exit (257);
        if (blk_int_c_int16_t[1]           !=  32767                 )    exit (257);
        if (blk_int_c_int16_t[0]           !=  127                   )    exit (257);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int32_t[i]   !=  -2147483648                )   exit (258);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int64_t[i]   !=  -9223372036850123456LL     )   exit (259);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int_least8_t[i] !=  127                    )   exit (260);
        }

        if (blk_int_c_int_least16_t[4]     != 32767                  )    exit (261);
        if (blk_int_c_int_least16_t[3]     != 0                      )    exit (261);
        if (blk_int_c_int_least16_t[2]     != -32768                 )    exit (261);
        if (blk_int_c_int_least16_t[1]     != 32767                  )    exit (261);
        if (blk_int_c_int_least16_t[0]     != 127                    )    exit (261);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int_least32_t[i] !=  2147483647            )    exit (262);
        }


        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int_least64_t[i] != -111111111111111111LL   ) exit (263);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( blk_int_c_int_fast8_t[i]   != -127                    )    exit (264);
        }

        if (blk_int_c_int_fast32_t[4]      !=  2147483647             )    exit (265);
        if (blk_int_c_int_fast32_t[3]      !=  127                    )    exit (265);
        if (blk_int_c_int_fast32_t[2]      != -2147483648             )    exit (265);
        if (blk_int_c_int_fast32_t[1]      !=  0                      )    exit (265);
        if (blk_int_c_int_fast32_t[0]      !=  1000000                )    exit (265);

        if (blk_int_c_int_fast64_t[4]      !=  9223372036854775807LL  )    exit (266);
        if (blk_int_c_int_fast64_t[3]      !=  0                      )    exit (266);
        if (blk_int_c_int_fast64_t[2]      != -9223372036854775807LL  )    exit (266);
        if (blk_int_c_int_fast64_t[1]      !=  1000000                )    exit (266);
        if (blk_int_c_int_fast64_t[0]      != -2147483648LL           )    exit (266);

}

