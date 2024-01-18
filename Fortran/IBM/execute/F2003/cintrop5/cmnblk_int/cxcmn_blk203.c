#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#if (defined(_AIX) && ! defined(_AIX52))
  #define int_fast16_t short
#endif



struct {
        signed char 		int_s1a[5];
        int8_t                  int_s1b[5];

        short                   int_s2a[5];
        short                   int_s2b[5];

        int		  	int_s4a[5];
        int			int_s4b[5];

        long long 		int_s8a[5];
        long long 		int_s8b[5];

        signed char             int_c_signed_char[5];
        short                   int_c_short[5];
        int                     int_c_int[5];
        long                    int_c_long[5];
        long long               int_c_long_long[5];
        size_t                  int_c_size_t[5];
        intptr_t                int_c_intptr_t[5];
        intmax_t                int_c_intmax_t[5];
        int8_t                  int_c_int8_t[5];
        int16_t                 int_c_int16_t[5];
        int32_t                 int_c_int32_t[5];
        int64_t                 int_c_int64_t[5];
        int_least8_t            int_c_int_least8_t[5];
        int_least16_t           int_c_int_least16_t[5];
        int_least32_t           int_c_int_least32_t[5];
        int_least64_t           int_c_int_least64_t[5];
        int_fast8_t             int_c_int_fast8_t[5];
        int_fast16_t            int_c_int_fast16_t[5];
        int_fast32_t            int_c_int_fast32_t[5];
        int_fast64_t            int_c_int_fast64_t[5];

} blk_all;


void csub_all(){

	int i;
	long         tmp_int_c_size_t; 

/* --------------------------------------------------------------*
*       1) Verify values from Fortran code                       *
* --------------------------------------------------------------*/

        if (blk_all.int_s1a[0]         !=  127                    )    exit (60);
        if (blk_all.int_s1a[1]         !=  0                      )    exit (60);
        if (blk_all.int_s1a[2]         !=  -128                   )    exit (60);
        if (blk_all.int_s1a[3]         !=  127                    )    exit (60);
        if (blk_all.int_s1a[4]         !=  127                    )    exit (60);

        for ( i = 0; i < 5; i++ ) {
          if ( blk_all.int_s1b[i]      != -128                    )    exit (61);
        }

        if (blk_all.int_s2a[0]         !=  32767                  )    exit (62);
        if (blk_all.int_s2a[1]         !=  0                      )    exit (62);
        if (blk_all.int_s2a[2]         !=  -32768                 )    exit (62);
        if (blk_all.int_s2a[3]         !=  32767                  )    exit (62);
        if (blk_all.int_s2a[4]         !=  127                    )    exit (62);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_s2b[i]     != -32768                  )    exit (63);
        }

        if (blk_all.int_s4a[0]         !=  2147483647             )    exit (64);
        if (blk_all.int_s4a[1]         !=  127                    )    exit (64);
        if (blk_all.int_s4a[2]         != -2147483648             )    exit (64);
        if (blk_all.int_s4a[3]         !=  0                      )    exit (64);
        if (blk_all.int_s4a[4]         !=  1000000                )    exit (64);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_s4b[i]   != -2147483648             )    exit (65);
	}

        if (blk_all.int_s8a[0]         !=  9223372036854775807LL  )    exit (66);
        if (blk_all.int_s8a[1]         !=  0                      )    exit (66);
        if (blk_all.int_s8a[2]         != -9223372036854775807LL  )    exit (66);
        if (blk_all.int_s8a[3]         !=  1000000                )    exit (66);
        if (blk_all.int_s8a[4]         != -2147483648LL           )    exit (66);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_s8b[i]   != -9223372036854775807LL  )    exit (67);
	}

        if (blk_all.int_c_signed_char[0] !=  127                    )    exit (68);
        if (blk_all.int_c_signed_char[1] !=  0                      )    exit (68);
        if (blk_all.int_c_signed_char[2] != -128                    )    exit (68);
        if (blk_all.int_c_signed_char[3] !=  127                    )    exit (68);
        if (blk_all.int_c_signed_char[4] !=  127                    )    exit (68);

        if (blk_all.int_c_short[0]     !=  32767                  )    exit (69);
        if (blk_all.int_c_short[1]     !=  0                      )    exit (69);
        if (blk_all.int_c_short[2]     != -32768                  )    exit (69);
        if (blk_all.int_c_short[3]     !=  32767                  )    exit (69);
        if (blk_all.int_c_short[4]     !=  127            	  )    exit (69);

        if (blk_all.int_c_int[0]       !=  2147483647             )    exit (70);
        if (blk_all.int_c_int[1]       !=  127            	  )    exit (70);
        if (blk_all.int_c_int[2]       != -2147483648             )    exit (70);
        if (blk_all.int_c_int[3]       !=  0                      )    exit (70);
        if (blk_all.int_c_int[4]       !=  1000000                )    exit (70);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_long[i]      !=  2147483647         )   exit (71);
	}

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_long_long[i] != -9223372036854775807LL )   exit (72);
	}

	// Due to size_t being similar to unsigned long, to compare negative values, it needs to be type casted.
	tmp_int_c_size_t = blk_all.int_c_size_t[2];

        if (blk_all.int_c_size_t[0]            !=  2147483647     )    exit (73);
        if (blk_all.int_c_size_t[1]            !=  127            )    exit (73);
        if (tmp_int_c_size_t                   != -2147483648     )    exit (73);
        if (blk_all.int_c_size_t[3]            !=  0              )    exit (73);
        if (blk_all.int_c_size_t[4]            !=  1000000        )    exit (73);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_intptr_t[i]  !=  1000000000                )   exit (74);
	}

        if (blk_all.int_c_intmax_t[0]          !=  9223372036854775807LL )    exit (75);
        if (blk_all.int_c_intmax_t[1]          !=  0          		 )    exit (75);
        if (blk_all.int_c_intmax_t[2]          != -9223372036854775807LL )    exit (75);
        if (blk_all.int_c_intmax_t[3]          !=  1000000               )    exit (75);
        if (blk_all.int_c_intmax_t[4]          != -2147483648LL          )    exit (75);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int8_t[i]    != -128                       )   exit (76);
	}

        if (blk_all.int_c_int16_t[0]           !=  32767                 )    exit (77);
        if (blk_all.int_c_int16_t[1]           !=  0                     )    exit (77);
        if (blk_all.int_c_int16_t[2]           != -32768                 )    exit (77);
        if (blk_all.int_c_int16_t[3]           !=  32767                 )    exit (77);
        if (blk_all.int_c_int16_t[4]           !=  127                   )    exit (77);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int32_t[i]   !=  2147483647                )   exit (78);
	}

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int64_t[i]   !=  9223372036850123456LL     )   exit (79);
	}

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int_least8_t[i] != -128                    )   exit (80);
	}

        if (blk_all.int_c_int_least16_t[0]     != 32767                  )    exit (81);
        if (blk_all.int_c_int_least16_t[1]     != 0        	         )    exit (81); 
        if (blk_all.int_c_int_least16_t[2]     != -32768                 )    exit (81);
        if (blk_all.int_c_int_least16_t[3]     != 32767                  )    exit (81);
        if (blk_all.int_c_int_least16_t[4]     != 127                    )    exit (81); 

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int_least32_t[i] !=  0                     )    exit (82);
	}

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int_least64_t[i] != 1111111111111111111LL   )    exit (83);
	}

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int_fast8_t[i]   != 1              	  )    exit (84);
	}

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int_fast16_t[i]   != 64                       )    exit (170);
        }

        if (blk_all.int_c_int_fast32_t[0]      !=  2147483647             )    exit (85);
        if (blk_all.int_c_int_fast32_t[1]      !=  127              	  )    exit (85);
        if (blk_all.int_c_int_fast32_t[2]      != -2147483648             )    exit (85);
        if (blk_all.int_c_int_fast32_t[3]      !=  0                      )    exit (85);
        if (blk_all.int_c_int_fast32_t[4]      !=  1000000                )    exit (85);

        if (blk_all.int_c_int_fast64_t[0]      !=  9223372036854775807LL  )    exit (86);
        if (blk_all.int_c_int_fast64_t[1]      !=  0            	  )    exit (86);
        if (blk_all.int_c_int_fast64_t[2]      != -9223372036854775807LL  )    exit (86);
        if (blk_all.int_c_int_fast64_t[3]      !=  1000000                )    exit (86);
        if (blk_all.int_c_int_fast64_t[4]      != -2147483648LL           )    exit (86);


/* --------------------------------------------------------------*
*      2) Modify the values and pass to Fortran                  *
* --------------------------------------------------------------*/
        blk_all.int_s1a[4]         =  127;
        blk_all.int_s1a[3]         =  0;
        blk_all.int_s1a[2]         =  -128;
        blk_all.int_s1a[1]         =  127 ;
        blk_all.int_s1a[0]         =  127 ;

        for ( i = 0; i < 5; i++ ) {
           blk_all.int_s1b[i]      = 127 ;
        }

        blk_all.int_s2a[4]         =  32767 ;
        blk_all.int_s2a[3]         =  0    ;
        blk_all.int_s2a[2]         =  -32768;
        blk_all.int_s2a[1]         =  32767;
        blk_all.int_s2a[0]         =  127 ;

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_s2b[i]     = 32767 ;
        }

        blk_all.int_s4a[4]         =  2147483647 ;
        blk_all.int_s4a[3]         =  127         ;
        blk_all.int_s4a[2]         = -2147483648 ;
        blk_all.int_s4a[1]         =  0         ;
        blk_all.int_s4a[0]         =  1000000  ;

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_s4b[i]     = 2147483647;
        }

        blk_all.int_s8a[4]         =  9223372036854775807LL  ;
        blk_all.int_s8a[3]         =  0                     ;
        blk_all.int_s8a[2]         = -9223372036854775807LL;
        blk_all.int_s8a[1]         =  1000000             ;
        blk_all.int_s8a[0]         = -2147483648LL       ;

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_s8b[i]     = 9223372036854775807LL ;
        }

        blk_all.int_c_signed_char[4] =  127 ;
        blk_all.int_c_signed_char[3] =  0  ;
        blk_all.int_c_signed_char[2] = -128;
        blk_all.int_c_signed_char[1] =  127;
        blk_all.int_c_signed_char[0] =  127;

        blk_all.int_c_short[4]     =  32767;
        blk_all.int_c_short[3]     =  0   ;
        blk_all.int_c_short[2]     = -32768;
        blk_all.int_c_short[1]     =  32767 ;
        blk_all.int_c_short[0]     =  127  ;

        blk_all.int_c_int[4]       =  2147483647 ;
        blk_all.int_c_int[3]       =  127       ;
        blk_all.int_c_int[2]       = -2147483648 ;
        blk_all.int_c_int[1]       =  0         ;
        blk_all.int_c_int[0]       =  1000000  ;

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_c_long[i]      =  -2147483648 ;
        }

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_c_long_long[i] = -9223372036854775807LL ;
        }

        blk_all.int_c_size_t[4]            =  2147483647 ;
        blk_all.int_c_size_t[3]            =  127        ;
        blk_all.int_c_size_t[2]            =  2147483646 ;
        blk_all.int_c_size_t[1]            =  0          ;
        blk_all.int_c_size_t[0]            =  1000000    ;

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_c_intptr_t[i]  =  -1000000000 ;
        }

        blk_all.int_c_intmax_t[4]          =  9223372036854775807LL ;
        blk_all.int_c_intmax_t[3]          =  0                    ;
        blk_all.int_c_intmax_t[2]          = -9223372036854775807LL;
        blk_all.int_c_intmax_t[1]          =  1000000             ;
        blk_all.int_c_intmax_t[0]          = -2147483648LL       ;

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_c_int8_t[i]    = 127 ;
        }

        blk_all.int_c_int16_t[4]           =  32767 ;
        blk_all.int_c_int16_t[3]           =  0      ;
        blk_all.int_c_int16_t[2]           = -32768   ;
        blk_all.int_c_int16_t[1]           =  32767  ;
        blk_all.int_c_int16_t[0]           =  127   ;

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_c_int32_t[i]   =  -2147483648 ;
        }

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_c_int64_t[i]   =  -9223372036850123456LL ;
        }

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_c_int_least8_t[i] =  127 ;
        }

        blk_all.int_c_int_least16_t[4]     = 32767 ;
        blk_all.int_c_int_least16_t[3]     = 0     ;
        blk_all.int_c_int_least16_t[2]     = -32768 ;
        blk_all.int_c_int_least16_t[1]     = 32767 ;
        blk_all.int_c_int_least16_t[0]     = 127  ;

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_c_int_least32_t[i] =  2147483647 ;
        }

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_c_int_least64_t[i] = -111111111111111111LL ;
        }

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_c_int_fast8_t[i]   = -127 ;
        }

        for ( i = 0; i < 5; i++ ) {
            blk_all.int_c_int_fast16_t[i]   = 6 ;
        }


        blk_all.int_c_int_fast32_t[4]      =  2147483647 ;
        blk_all.int_c_int_fast32_t[3]      =  127        ;
        blk_all.int_c_int_fast32_t[2]      = -2147483648 ;
        blk_all.int_c_int_fast32_t[1]      =  0          ;
        blk_all.int_c_int_fast32_t[0]      =  1000000    ;

        blk_all.int_c_int_fast64_t[4]      =  9223372036854775807LL ;
        blk_all.int_c_int_fast64_t[3]      =  0                   ;
        blk_all.int_c_int_fast64_t[2]      = -9223372036854775807LL;
        blk_all.int_c_int_fast64_t[1]      =  1000000            ;
        blk_all.int_c_int_fast64_t[0]      = -2147483648LL       ;


/* --------------------------------------------------------------*
*       3) Verify values before returning to Fortran             *
* --------------------------------------------------------------*/

        if (blk_all.int_s1a[4]         !=  127                    )    exit (140);
        if (blk_all.int_s1a[3]         !=  0                      )    exit (140);
        if (blk_all.int_s1a[2]         !=  -128                   )    exit (140);
        if (blk_all.int_s1a[1]         !=  127                    )    exit (140);
        if (blk_all.int_s1a[0]         !=  127                    )    exit (140);

        for ( i = 0; i < 5; i++ ) {
          if ( blk_all.int_s1b[i]      != 127                     )    exit (141);
        }

        if (blk_all.int_s2a[4]         !=  32767                  )    exit (142);
        if (blk_all.int_s2a[3]         !=  0                      )    exit (142);
        if (blk_all.int_s2a[2]         !=  -32768                 )    exit (142);
        if (blk_all.int_s2a[1]         !=  32767                  )    exit (142);
        if (blk_all.int_s2a[0]         !=  127                    )    exit (142);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_s2b[i]     != 32767                  )    exit (143);
        }

        if (blk_all.int_s4a[4]         !=  2147483647             )    exit (144);
        if (blk_all.int_s4a[3]         !=  127                    )    exit (144);
        if (blk_all.int_s4a[2]         != -2147483648             )    exit (144);
        if (blk_all.int_s4a[1]         !=  0                      )    exit (144);
        if (blk_all.int_s4a[0]         !=  1000000                )    exit (144);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_s4b[i]     != 2147483647             )    exit (145);
        }

        if (blk_all.int_s8a[4]         !=  9223372036854775807LL  )    exit (146);
        if (blk_all.int_s8a[3]         !=  0                      )    exit (146);
        if (blk_all.int_s8a[2]         != -9223372036854775807LL  )    exit (146);
        if (blk_all.int_s8a[1]         !=  1000000                )    exit (146);
        if (blk_all.int_s8a[0]         != -2147483648LL           )    exit (146);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_s8b[i]     != 9223372036854775807LL  )    exit (147);
        }

        if (blk_all.int_c_signed_char[4] !=  127                    )    exit (148);
        if (blk_all.int_c_signed_char[3] !=  0                      )    exit (148);
        if (blk_all.int_c_signed_char[2] != -128                    )    exit (148);
        if (blk_all.int_c_signed_char[1] !=  127                    )    exit (148);
        if (blk_all.int_c_signed_char[0] !=  127                    )    exit (148);

        if (blk_all.int_c_short[4]     !=  32767                  )    exit (149);
        if (blk_all.int_c_short[3]     !=  0                      )    exit (149);
        if (blk_all.int_c_short[2]     != -32768                  )    exit (149);
        if (blk_all.int_c_short[1]     !=  32767                  )    exit (149);
        if (blk_all.int_c_short[0]     !=  127                    )    exit (149);

        if (blk_all.int_c_int[4]       !=  2147483647             )    exit (150);
        if (blk_all.int_c_int[3]       !=  127                    )    exit (150);
        if (blk_all.int_c_int[2]       != -2147483648             )    exit (150);
        if (blk_all.int_c_int[1]       !=  0                      )    exit (150);
        if (blk_all.int_c_int[0]       !=  1000000                )    exit (150);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_long[i]      !=  -2147483648         )   exit (151);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_long_long[i] != -9223372036854775807LL )   exit (152);
        }

        if (blk_all.int_c_size_t[4]            !=  2147483647     )    exit (153);
        if (blk_all.int_c_size_t[3]            !=  127            )    exit (153);
        if (blk_all.int_c_size_t[2]            !=  2147483646     )    exit (153);
        if (blk_all.int_c_size_t[1]            !=  0              )    exit (153);
        if (blk_all.int_c_size_t[0]            !=  1000000        )    exit (153);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_intptr_t[i]  !=  -1000000000               )   exit (154);
        }

        if (blk_all.int_c_intmax_t[4]          !=  9223372036854775807LL )    exit (155);
        if (blk_all.int_c_intmax_t[3]          !=  0                     )    exit (155);
        if (blk_all.int_c_intmax_t[2]          != -9223372036854775807LL )    exit (155);
        if (blk_all.int_c_intmax_t[1]          !=  1000000               )    exit (165);
        if (blk_all.int_c_intmax_t[0]          != -2147483648LL          )    exit (165);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int8_t[i]    != 127                       )   exit (156);
        }

        if (blk_all.int_c_int16_t[4]           !=  32767                 )    exit (157);
        if (blk_all.int_c_int16_t[3]           !=  0                     )    exit (157);
        if (blk_all.int_c_int16_t[2]           != -32768                 )    exit (157);
        if (blk_all.int_c_int16_t[1]           !=  32767                 )    exit (157);
        if (blk_all.int_c_int16_t[0]           !=  127                   )    exit (157);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int32_t[i]   !=  -2147483648                )   exit (158);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int64_t[i]   !=  -9223372036850123456LL     )   exit (159);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int_least8_t[i] !=  127                    )   exit (160);
        }

        if (blk_all.int_c_int_least16_t[4]     != 32767                  )    exit (161);
        if (blk_all.int_c_int_least16_t[3]     != 0                      )    exit (161);
        if (blk_all.int_c_int_least16_t[2]     != -32768                 )    exit (161);
        if (blk_all.int_c_int_least16_t[1]     != 32767                  )    exit (161);
        if (blk_all.int_c_int_least16_t[0]     != 127                    )    exit (161);

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int_least32_t[i] !=  2147483647            )    exit (162);
        }


        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int_least64_t[i] != -111111111111111111LL   ) exit (163);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int_fast8_t[i]   != -127                    )    exit (164);
        }

        for ( i = 0; i < 5; i++ ) {
           if ( blk_all.int_c_int_fast16_t[i]   != 6                       )    exit (171);
        }

        if (blk_all.int_c_int_fast32_t[4]      !=  2147483647             )    exit (165);
        if (blk_all.int_c_int_fast32_t[3]      !=  127                    )    exit (165);
        if (blk_all.int_c_int_fast32_t[2]      != -2147483648             )    exit (165);
        if (blk_all.int_c_int_fast32_t[1]      !=  0                      )    exit (165);
        if (blk_all.int_c_int_fast32_t[0]      !=  1000000                )    exit (165);

        if (blk_all.int_c_int_fast64_t[4]      !=  9223372036854775807LL  )    exit (166);
        if (blk_all.int_c_int_fast64_t[3]      !=  0                      )    exit (166);
        if (blk_all.int_c_int_fast64_t[2]      != -9223372036854775807LL  )    exit (166);
        if (blk_all.int_c_int_fast64_t[1]      !=  1000000                )    exit (166);
        if (blk_all.int_c_int_fast64_t[0]      != -2147483648LL           )    exit (166);

}

