#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char   		Bnd_Lbl15[16];


void csub1(){

/* --------------------------------------------------------------*
*       1) Verify values from Fortran code                       *
* --------------------------------------------------------------*/
       if ( Bnd_Lbl15[0]   !=    '\f'    )           exit(50);
       if ( Bnd_Lbl15[1]   !=    'I'     )           exit(51);
       if ( Bnd_Lbl15[2]   !=    '\''    )           exit(52);
       if ( Bnd_Lbl15[3]   !=    'm'     )           exit(53);
       if ( Bnd_Lbl15[4]   !=    ' '     )           exit(54);
       if ( Bnd_Lbl15[5]   !=    'a'     )           exit(55);
       if ( Bnd_Lbl15[6]   !=    '\t'    )           exit(56);
       if ( Bnd_Lbl15[7]   !=    '\"'    )           exit(57);
       if ( Bnd_Lbl15[8]   !=    'T'     )           exit(58);
       if ( Bnd_Lbl15[9]   !=    'r'     )           exit(59);
       if ( Bnd_Lbl15[10]  !=    'E'     )           exit(60);
       if ( Bnd_Lbl15[11]  !=    'e'     )           exit(61);
       if ( Bnd_Lbl15[12]  !=    '\"'    )           exit(62);
       if ( Bnd_Lbl15[13]  !=    '.'     )           exit(63);
       if ( Bnd_Lbl15[14]  !=    '\b'    )           exit(64);
       if ( Bnd_Lbl15[15]  !=    '!'     )           exit(65);


/* --------------------------------------------------------------*
*      2) Modify the values and pass to Fortran                  *
* --------------------------------------------------------------*/
        Bnd_Lbl15[0] = '\f';
        Bnd_Lbl15[1] = 'N';
        Bnd_Lbl15[2] = 'o';
        Bnd_Lbl15[3] = ',';
        Bnd_Lbl15[4] = ' ';
        Bnd_Lbl15[5] = 'I';
        Bnd_Lbl15[6] = '\'';
        Bnd_Lbl15[7] = 'm';
        Bnd_Lbl15[8] = ' ';
        Bnd_Lbl15[9] = 'a';
        Bnd_Lbl15[10] = ' ';
        Bnd_Lbl15[11] = 'd';
        Bnd_Lbl15[12] = 'o';
        Bnd_Lbl15[13] = 'g';
        Bnd_Lbl15[14] = '!';
        Bnd_Lbl15[15] = '\n';


/* --------------------------------------------------------------*
*       3) Verify values before returning to Fortran             *
* --------------------------------------------------------------*/
        if ( Bnd_Lbl15[0]    != '\f' )               exit(66);
        if ( Bnd_Lbl15[1]    != 'N'  )               exit(67);
        if ( Bnd_Lbl15[2]    != 'o'  )               exit(68);
        if ( Bnd_Lbl15[3]    != ','  )               exit(69);
        if ( Bnd_Lbl15[4]    != ' '  )               exit(70);
        if ( Bnd_Lbl15[5]    != 'I'  )               exit(71);
        if ( Bnd_Lbl15[6]    != '\'' )               exit(72);
        if ( Bnd_Lbl15[7]    != 'm'  )               exit(73);
        if ( Bnd_Lbl15[8]    != ' '  )               exit(74);
        if ( Bnd_Lbl15[9]    != 'a'  )               exit(75);
        if ( Bnd_Lbl15[10]   != ' '  )               exit(76);
        if ( Bnd_Lbl15[11]   != 'd'  )               exit(77);
        if ( Bnd_Lbl15[12]   != 'o'  )               exit(78);
        if ( Bnd_Lbl15[13]   != 'g'  )               exit(79);
        if ( Bnd_Lbl15[14]   != '!'  )               exit(80);
        if ( Bnd_Lbl15[15]   != '\n' )               exit(81);


}

