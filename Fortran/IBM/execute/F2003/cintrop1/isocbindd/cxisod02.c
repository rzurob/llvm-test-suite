
/*
        C code for testcase "fxisod02.f" and "fxisod03.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

int32_t fnt1(int32_t *a, int64_t *b) {

   if ( *a != 5 ) exit(21);
   if ( *b != 10 ) exit(23);

   *a = *a + 5;
   *b = *b + 10;

   return 0;
}

int32_t fnt2(int32_t a, int64_t b) {

   if ( a != 5 ) exit(25);
   if ( b != 10 ) exit(27);

   a = a + 5;
   b = b + 10;

   return 0;
}

int32_t fnt3(int32_t *a, int64_t *b) {

   if ( *a != 5 ) exit(29);
   if ( *b != 10 ) exit(31);

   return 0;
}

int32_t fnt4(int32_t a, int64_t b) {

   if ( a != 5 ) exit(33);
   if ( b != 10 ) exit(35);

   return 0;
}

int32_t fnt5(const int32_t *a, const int64_t *b) {

   if ( *a != 5 ) exit(37);
   if ( *b != 10 ) exit(39);

   return 0;
}

int32_t fnt6(const int32_t a, const int64_t b) {

   if ( a != 5 ) exit(41);
   if ( b != 10 ) exit(43);

   return 0;
}
