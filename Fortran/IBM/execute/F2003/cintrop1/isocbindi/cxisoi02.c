
/*
        C code for testcase "fxisoi02.f" and "fxisoi03.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

int_fast16_t fnt1(int_fast16_t *a) {

   if ( *a != 5 ) exit(21);

   *a = *a + 5;

   return 0;
}

int_fast16_t fnt2(int_fast16_t a) {

   if ( a != 5 ) exit(23);

   a = a + 5;

   return 0;
}

int_fast16_t fnt3(int_fast16_t *a) {

   if ( *a != 5 ) exit(25);

   return 0;
}

int_fast16_t fnt4(int_fast16_t a) {

   if ( a != 5 ) exit(27);

   return 0;
}

int_fast16_t fnt5(const int_fast16_t *a) {

   if ( *a != 5 ) exit(29);

   return 0;
}

int_fast16_t fnt6(const int_fast16_t a) {

   if ( a != 5 ) exit(31);

   return 0;
}
