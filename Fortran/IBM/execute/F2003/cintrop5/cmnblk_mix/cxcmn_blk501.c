#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "precision_r.inc"


// --------------------------------------------------------------------- //
// 			Default Array Variables  		         //	
// --------------------------------------------------------------------- //

struct foo {
        int int_d[5];
        float real_d[5];
        char char_d[16];
} blk_d;


void csub_d(){

/* --------------------------------------------------------------*
*  	1) Verify values from Fortran code                       *
* --------------------------------------------------------------*/

        // Check integer values from Fortran
        if ( blk_d.int_d[0] != 2100000009 )             exit(70);
        if ( blk_d.int_d[1] != -50 )                    exit(71);
        if ( blk_d.int_d[2] != -0 )                     exit(72);
        if ( blk_d.int_d[3] != 99  )                    exit(73);
        if ( blk_d.int_d[4] != -2100000001 )            exit(74);


        // Check real values from Fortran by using precision functions from precision_r.inc 
        if ( precision_flt( blk_d.real_d[0]  ,   3.402823E+37f )  !=  1 ) exit (75);
        if ( precision_flt( blk_d.real_d[1]  ,  -0.0000009f    )  !=  1 ) exit (76);
        if ( precision_flt( blk_d.real_d[2]  ,  -3.404443E+37f )  !=  1 ) exit (77);
        if ( precision_flt( blk_d.real_d[3]  ,  -0.115494E-37f )  !=  1 ) exit (78);
        if ( precision_flt( blk_d.real_d[4]  ,   1.175494E-39f )  !=  1 ) exit (79);


       // Check character values from Fortran
       if ( blk_d.char_d[0]   !=    '\f'    )           exit(80);
       if ( blk_d.char_d[1]   !=    'I'     )           exit(81);
       if ( blk_d.char_d[2]   !=    '\''    )           exit(82);
       if ( blk_d.char_d[3]   !=    'm'     )           exit(83);
       if ( blk_d.char_d[4]   !=    ' '     )           exit(84);
       if ( blk_d.char_d[5]   !=    'a'     )           exit(85);
       if ( blk_d.char_d[6]   !=    '\t'    )           exit(86);
       if ( blk_d.char_d[7]   !=    '\"'    )           exit(87);
       if ( blk_d.char_d[8]   !=    'T'     )           exit(88);
       if ( blk_d.char_d[9]   !=    'r'     )           exit(89);
       if ( blk_d.char_d[10]  !=    'E'     )           exit(90);
       if ( blk_d.char_d[11]  !=    'e'     )           exit(91);
       if ( blk_d.char_d[12]  !=    '\"'    )           exit(92);
       if ( blk_d.char_d[13]  !=    '.'     )           exit(93);
       if ( blk_d.char_d[14]  !=    '\b'    )           exit(94);
       if ( blk_d.char_d[15]  !=    '!'     )           exit(95);


/* --------------------------------------------------------------*
*      2) Modify the values and pass to Fortran                  *
* --------------------------------------------------------------*/

	blk_d.int_d[0] = -2100000001;
	blk_d.int_d[1] = 99;
	blk_d.int_d[2] = -0;
	blk_d.int_d[3] = -50;
	blk_d.int_d[4] = 2100000009;

	blk_d.real_d[0] = 1.175494E-39;
	blk_d.real_d[1] = -0.115494E-37;
	blk_d.real_d[2] = -3.404443E+37;
	blk_d.real_d[3] = -0.0000009;
	blk_d.real_d[4] = 3.402823E+37;

	blk_d.char_d[0] = '\f';
	blk_d.char_d[1] = 'N';
	blk_d.char_d[2] = 'o';
	blk_d.char_d[3] = ',';
	blk_d.char_d[4] = ' ';
	blk_d.char_d[5] = 'I';
	blk_d.char_d[6] = '\'';
	blk_d.char_d[7] = 'm';
	blk_d.char_d[8] = ' ';
	blk_d.char_d[9] = 'a';
	blk_d.char_d[10] = ' ';
	blk_d.char_d[11] = 'd';
	blk_d.char_d[12] = 'o';
	blk_d.char_d[13] = 'g';
	blk_d.char_d[14] = '!';
	blk_d.char_d[15] = '\n';


/* --------------------------------------------------------------*
*  	1) Verify values before returning to Fortran             *
* --------------------------------------------------------------*/

        // Check integer values after they are modified
        if ( blk_d.int_d[0] !=  -2100000001 )           exit(120);
        if ( blk_d.int_d[1] !=  99 )                    exit(121);
        if ( blk_d.int_d[2] !=  -0 )                    exit(122);
        if ( blk_d.int_d[3] !=  -50 )                   exit(123);
        if ( blk_d.int_d[4] !=  2100000009 )            exit(124);

        // Check real values after they are modified by using precision functions from precision_r.inc
        if ( precision_flt( blk_d.real_d[0]  ,   1.175494E-39f )  !=  1 ) exit (125);
        if ( precision_flt( blk_d.real_d[1]  ,  -0.115494E-37f )  !=  1 ) exit (126);
        if ( precision_flt( blk_d.real_d[2]  ,  -3.404443E+37f )  !=  1 ) exit (127);
        if ( precision_flt( blk_d.real_d[3]  ,  -0.0000009f    )  !=  1 ) exit (128);
        if ( precision_flt( blk_d.real_d[4]  ,   3.402823E+37f )  !=  1 ) exit (129);

        // Check character values after they are modified
        if ( blk_d.char_d[0]    != '\f' )               exit(130);
        if ( blk_d.char_d[1]    != 'N'  )               exit(131);
        if ( blk_d.char_d[2]    != 'o'  )               exit(132);
        if ( blk_d.char_d[3]    != ','  )               exit(133);
        if ( blk_d.char_d[4]    != ' '  )               exit(134);
        if ( blk_d.char_d[5]    != 'I'  )               exit(135);
        if ( blk_d.char_d[6]    != '\'' )               exit(136);
        if ( blk_d.char_d[7]    != 'm'  )               exit(137);
        if ( blk_d.char_d[8]    != ' '  )               exit(138);
        if ( blk_d.char_d[9]    != 'a'  )               exit(139);
        if ( blk_d.char_d[10]   != ' '  )               exit(140);
        if ( blk_d.char_d[11]   != 'd'  )               exit(141);
        if ( blk_d.char_d[12]   != 'o'  )               exit(142);
        if ( blk_d.char_d[13]   != 'g'  )               exit(143);
        if ( blk_d.char_d[14]   != '!'  )               exit(144);
        if ( blk_d.char_d[15]   != '\n' )               exit(145);
}

