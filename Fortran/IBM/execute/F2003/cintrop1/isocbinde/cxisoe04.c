
/*
        C code for testcase "fxisoe04.f" and "fxisoe05.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

void sub1(int_least8_t *a, int_least16_t *b) {

   if ( *a != 5 ) exit(21);
   if ( *b != 10 ) exit(23);

   *a = *a + 5;
   *b = *b + 10;

}

void sub2(int_least8_t a, int_least16_t b) {

   if ( a != 5 ) exit(25);
   if ( b != 10 ) exit(27);

   a = a + 5;
   b = b + 10;

}

void sub3(int_least8_t *a, int_least16_t *b) {

   if ( *a != 5 ) exit(29);
   if ( *b != 10 ) exit(31);

}

void sub4(int_least8_t a, int_least16_t b) {

   if ( a != 5 ) exit(33);
   if ( b != 10 ) exit(35);

}

void sub5(const int_least8_t *a, const int_least16_t *b) {

   if ( *a != 5 ) exit(37);
   if ( *b != 10 ) exit(39);

}

void sub6(const int_least8_t a, const int_least16_t b) {

   if ( a != 5 ) exit(41);
   if ( b != 10 ) exit(43);

}
