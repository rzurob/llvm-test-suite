
/*
        C code for testcase "fxisok04.f" and "fxisok05.f"
*/

#include <stdio.h>
#include <stdlib.h>

void sub1(char *a, signed char *b) {

   if ( *a != 'A' ) exit(21);
   if ( *b != 'B' ) exit(23);

   *a = 'C';
   *b = 'D';

}

void sub2(char a, signed char b) {

   if ( a != 'A' ) exit(25);
   if ( b != 'B' ) exit(27);

   a = 'C';
   b = 'D';

}

void sub3(char *a, signed char *b) {

   if ( *a != 'A' ) exit(29);
   if ( *b != 'B' ) exit(31);

}

void sub4(char a, signed char b) {

   if ( a != 'A' ) exit(33);
   if ( b != 'B' ) exit(35);

}

void sub5(const char *a, const signed char *b) {

   if ( *a != 'A' ) exit(37);
   if ( *b != 'B' ) exit(39);

}

void sub6(const char a, const signed char b) {

   if ( a != 'A' ) exit(41);
   if ( b != 'B' ) exit(43);

}
