
/*
        C code for testcase "fxisok02.f" and "fxisok03.f"
*/

#include <stdio.h>
#include <stdlib.h>

char fnt1(char *a, signed char *b) {

   if ( *a != 'A' ) exit(21);
   if ( *b != 'B' ) exit(23);

   *a = 'C';
   *b = 'D';

   return 0;
}

char fnt2(char a, signed char b) {

   if ( a != 'A' ) exit(25);
   if ( b != 'B' ) exit(27);

   a = 'C';
   b = 'D';

   return 0;
}

char fnt3(char *a, signed char *b) {

   if ( *a != 'A' ) exit(29);
   if ( *b != 'B' ) exit(31);

   return 0;
}

char fnt4(char a, signed char b) {

   if ( a != 'A' ) exit(33);
   if ( b != 'B' ) exit(35);

   return 0;
}

char fnt5(const char *a, const signed char *b) {

   if ( *a != 'A' ) exit(37);
   if ( *b != 'B' ) exit(39);

   return 0;
}

char fnt6(const char a, const signed char b) {

   if ( a != 'A' ) exit(41);
   if ( b != 'B' ) exit(43);

   return 0;
}
