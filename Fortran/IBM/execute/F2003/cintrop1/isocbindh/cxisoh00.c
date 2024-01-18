
/*
	C code for testcase "fxisoh00.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

int main() {

   int_fast32_t fnt1(int_fast32_t *, int_fast64_t *);
   int_fast32_t fnt2(int_fast32_t, int_fast64_t);
   int_fast32_t fnt3(int_fast32_t *, int_fast64_t *);
   int_fast32_t fnt4(int_fast32_t, int_fast64_t);
   int_fast32_t fnt5(const int_fast32_t *, const int_fast64_t *);
   int_fast32_t fnt6(const int_fast32_t, const int_fast64_t);

   int_fast32_t a, ret;
   int_fast64_t b;

/* Test 1 */

   a = 5;
   b = 10;

   ret = fnt1(&a,&b);

   if ( a != 10 ) exit(21);
   if ( b != 20 ) exit(23);
   if ( ret != 120 ) exit(25);

/* Test 2 */

   a = 5;
   b = 10;

   ret = fnt2(a,b);

   if ( a != 5 ) exit(27);
   if ( b != 10 ) exit(29);
   if ( ret != 120 ) exit(31);

/* Test 3 */

   a = 5;
   b = 10;

   ret = fnt3(&a,&b);

   if ( a != 5 ) exit(33);
   if ( b != 10 ) exit(35);
   if ( ret != 35 ) exit(37);

/* Test 4 */

   a = 5;
   b = 10;

   ret = fnt4(a,b);

   if ( a != 5 ) exit(39);
   if ( b != 10 ) exit(41);
   if ( ret != 35 ) exit(43);

/* Test 5 */

   a = 5;
   b = 10;

   ret = fnt5(&a,&b);

   if ( a != 5 ) exit(45);
   if ( b != 10 ) exit(47);
   if ( ret != 35 ) exit(49);

/* Test 6 */

   a = 5;
   b = 10;

   ret = fnt6(a,b);

   if ( a != 5 ) exit(51);
   if ( b != 10 ) exit(53);
   if ( ret != 35 ) exit(55);

   return 0;

}
