
/*
        C code for testcase "fxisoi04.f" and "fxisoi05.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

void sub1(int_fast16_t *a) {

   if ( *a != 5 ) exit(21);

   *a = *a + 5;

}

void sub2(int_fast16_t a) {

   if ( a != 5 ) exit(23);

   a = a + 5;

}

void sub3(int_fast16_t *a) {

   if ( *a != 5 ) exit(25);

}

void sub4(int_fast16_t a) {

   if ( a != 5 ) exit(27);

}

void sub5(const int_fast16_t *a) {

   if ( *a != 5 ) exit(29);

}

void sub6(const int_fast16_t a) {

   if ( a != 5 ) exit(31);

}
