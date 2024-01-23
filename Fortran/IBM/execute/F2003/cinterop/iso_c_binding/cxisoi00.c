
/*
	C code for testcase "fxisoi00.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

int main() {

   int_fast16_t fnt1(int_fast16_t *);
   int_fast16_t fnt2(int_fast16_t);
   int_fast16_t fnt3(int_fast16_t *);
   int_fast16_t fnt4(int_fast16_t);
   int_fast16_t fnt5(const int_fast16_t *);
   int_fast16_t fnt6(const int_fast16_t);

   int_fast16_t a, ret;

/* Test 1 */

   a = 5;

   ret = fnt1(&a);

   if ( a != 10 ) exit(21);
   if ( ret != 100 ) exit(23);

/* Test 2 */

   a = 5;

   ret = fnt2(a);

   if ( a != 5 ) exit(25);
   if ( ret != 100 ) exit(27);

/* Test 3 */

   a = 5;

   ret = fnt3(&a);

   if ( a != 5 ) exit(29);
   if ( ret != 25 ) exit(31);

/* Test 4 */

   a = 5;

   ret = fnt4(a);

   if ( a != 5 ) exit(33);
   if ( ret != 25 ) exit(35);

/* Test 5 */

   a = 5;

   ret = fnt5(&a);

   if ( a != 5 ) exit(37);
   if ( ret != 25 ) exit(39);

/* Test 6 */

   a = 5;

   ret = fnt6(a);

   if ( a != 5 ) exit(41);
   if ( ret != 25 ) exit(43);

   return 0;

}
