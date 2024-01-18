
/*
        C code for testcase "fxisoj02.f" and "fxisoj03.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <inttypes.h>

size_t fnt1(size_t *a, intptr_t *b) {

   if ( *a != 5 ) exit(21);
   if ( *b != 10 ) exit(23);

   *a = *a + 5;
   *b = *b + 10;

   return 0;
}

size_t fnt2(size_t a, intptr_t b) {

   if ( a != 5 ) exit(25);
   if ( b != 10 ) exit(27);

   a = a + 5;
   b = b + 10;

   return 0;
}

size_t fnt3(size_t *a, intptr_t *b) {

   if ( *a != 5 ) exit(29);
   if ( *b != 10 ) exit(31);

   return 0;
}

size_t fnt4(size_t a, intptr_t b) {

   if ( a != 5 ) exit(33);
   if ( b != 10 ) exit(35);

   return 0;
}

size_t fnt5(const size_t *a, const intptr_t *b) {

   if ( *a != 5 ) exit(37);
   if ( *b != 10 ) exit(39);

   return 0;
}

size_t fnt6(const size_t a, const intptr_t b) {

   if ( a != 5 ) exit(41);
   if ( b != 10 ) exit(43);

   return 0;
}
