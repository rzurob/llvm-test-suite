
/*
	C code for testcase "fxisoc00.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

int main() {

   void sub1(int8_t *, int16_t *);
   void sub2(int8_t, int16_t);
   void sub3(int8_t *, int16_t *);
   void sub4(int8_t, int16_t);
   void sub5(const int8_t *, const int16_t *);
   void sub6(const int8_t, const int16_t);

   int8_t a, ret;
   int16_t b;

/* Test 1 */

   a = 5;
   b = 10;

   sub1(&a,&b);

   if ( a != 10 ) exit(21);
   if ( b != 20 ) exit(23);

/* Test 2 */

   a = 5;
   b = 10;

   sub2(a,b);

   if ( a != 5 ) exit(25);
   if ( b != 10 ) exit(27);

/* Test 3 */

   a = 5;
   b = 10;

   sub3(&a,&b);

   if ( a != 5 ) exit(29);
   if ( b != 10 ) exit(31);

/* Test 4 */

   a = 5;
   b = 10;

   sub4(a,b);

   if ( a != 5 ) exit(33);
   if ( b != 10 ) exit(35);

/* Test 5 */

   a = 5;
   b = 10;

   sub5(&a,&b);

   if ( a != 5 ) exit(37);
   if ( b != 10 ) exit(39);

/* Test 6 */

   a = 5;
   b = 10;

   sub6(a,b);

   if ( a != 5 ) exit(41);
   if ( b != 10 ) exit(43);

   return 0;

}