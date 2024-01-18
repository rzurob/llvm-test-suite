
/*
	C code for testcase "fxisoi00.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

int main() {

   void sub1(int_fast16_t *);
   void sub2(int_fast16_t);
   void sub3(int_fast16_t *);
   void sub4(int_fast16_t);
   void sub5(const int_fast16_t *);
   void sub6(const int_fast16_t);

   int_fast16_t a, ret;

/* Test 1 */

   a = 5;

   sub1(&a);

   if ( a != 10 ) exit(21);

/* Test 2 */

   a = 5;

   sub2(a);

   if ( a != 5 ) exit(23);

/* Test 3 */

   a = 5;

   sub3(&a);

   if ( a != 5 ) exit(25);

/* Test 4 */

   a = 5;

   sub4(a);

   if ( a != 5 ) exit(27);

/* Test 5 */

   a = 5;

   sub5(&a);

   if ( a != 5 ) exit(29);

/* Test 6 */

   a = 5;

   sub6(a);

   if ( a != 5 ) exit(31);

   return 0;

}
